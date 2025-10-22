;; title: DrivePool - Autonomous Vehicle Insurance Pool
;; version: 1.0.0
;; summary: Collective self-insurance for autonomous vehicles with real-time risk assessment
;; description: A mutual insurance pool that uses IoT telemetry data to dynamically adjust
;;              premiums and process claims automatically for autonomous vehicle owners

;; traits
(define-trait oracle-trait
  (
    (verify-crash-data (uint uint) (response bool uint))
    (get-risk-score (principal) (response uint uint))
  )
)

;; token definitions
(define-fungible-token pool-token)
(define-non-fungible-token policy-nft uint)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_POLICY_NOT_FOUND (err u102))
(define-constant ERR_CLAIM_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_RISK_SCORE (err u104))
(define-constant ERR_POOL_INSUFFICIENT (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_ALREADY_VOTED (err u107))
(define-constant ERR_NOT_MEMBER (err u108))
(define-constant ERR_CONTRACT_PAUSED (err u109))
(define-constant ERR_OVERFLOW (err u110))
(define-constant ERR_UNDERFLOW (err u111))
(define-constant ERR_RATE_LIMIT_EXCEEDED (err u112))
(define-constant ERR_INVALID_INPUT (err u113))
(define-constant ERR_CLAIM_AMOUNT_TOO_HIGH (err u114))

(define-constant MIN_RISK_SCORE u1)
(define-constant MAX_RISK_SCORE u100)
(define-constant BASE_PREMIUM u1000000) ;; 1 STX in microSTX
(define-constant CLAIM_VOTING_PERIOD u144) ;; ~24 hours in blocks
(define-constant MIN_POOL_BALANCE u10000000) ;; 10 STX minimum pool balance
(define-constant RATE_LIMIT_BLOCKS u10) ;; Minimum blocks between operations
(define-constant MAX_CLAIM_AMOUNT u100000000) ;; 100 STX maximum claim
(define-constant MAX_POLICIES_PER_MEMBER u10)

;; data vars
(define-data-var total-pool-balance uint u0)
(define-data-var policy-counter uint u0)
(define-data-var claim-counter uint u0)
(define-data-var oracle-address (optional principal) none)
(define-data-var pool-active bool true)

;; data maps
(define-map policies 
  { policy-id: uint }
  {
    owner: principal,
    vehicle-id: (string-ascii 64),
    premium: uint,
    risk-score: uint,
    balance: uint,
    active: bool,
    last-updated: uint
  }
)

(define-map member-policies
  { member: principal }
  { policy-ids: (list 10 uint) }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    amount: uint,
    status: (string-ascii 20),
    crash-data-hash: (buff 32),
    created-at: uint,
    votes-for: uint,
    votes-against: uint,
    voters: (list 100 principal)
  }
)

(define-map risk-adjustments
  { policy-id: uint }
  {
    previous-score: uint,
    new-score: uint,
    adjustment-factor: uint,
    timestamp: uint
  }
)

(define-map governance-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-map surplus-distributions
  { year: uint, member: principal }
  { amount: uint, claimed: bool }
)

(define-map last-operation-block principal uint) ;; Rate limiting

;; Security helper functions

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (asserts! (>= result a) ERR_OVERFLOW)
    (ok result)))

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b)
    (ok (- a b))
    ERR_UNDERFLOW))

(define-private (safe-mul (a uint) (b uint))
  (let ((result (* a b)))
    (asserts! (or (is-eq b u0) (is-eq (/ result b) a)) ERR_OVERFLOW)
    (ok result)))

(define-private (safe-div (a uint) (b uint))
  (if (> b u0)
    (ok (/ a b))
    ERR_INVALID_INPUT))

(define-private (check-rate-limit (user principal))
  (let ((current-block burn-block-height)
        (last-block (default-to u0 (map-get? last-operation-block user))))
    (asserts! (>= (- current-block last-block) RATE_LIMIT_BLOCKS) ERR_RATE_LIMIT_EXCEEDED)
    (map-set last-operation-block user current-block)
    (ok true)))

(define-private (validate-vehicle-id (vehicle-id (string-ascii 64)))
  (if (> (len vehicle-id) u0)
    (ok true)
    ERR_INVALID_INPUT))

;; public functions

;; Pause pool (owner only)
(define-public (pause-pool)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (var-get pool-active) ERR_INVALID_INPUT)
    (var-set pool-active false)
    (ok true)))

;; Unpause pool (owner only)
(define-public (unpause-pool)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get pool-active)) ERR_INVALID_INPUT)
    (var-set pool-active true)
    (ok true)))

;; Join the insurance pool with a new policy
(define-public (join-pool (vehicle-id (string-ascii 64)) (initial-deposit uint))
  (let (
    (current-counter (var-get policy-counter))
    (policy-id (unwrap! (safe-add current-counter u1) ERR_OVERFLOW))
    (caller tx-sender)
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (>= initial-deposit BASE_PREMIUM) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> initial-deposit u0) ERR_INVALID_INPUT)
    (try! (validate-vehicle-id vehicle-id))
    (try! (check-rate-limit caller))
    
    ;; Transfer initial deposit to contract (external call first - reentrancy protection)
    (try! (stx-transfer? initial-deposit caller (as-contract tx-sender)))
    
    ;; Mint policy NFT
    (try! (nft-mint? policy-nft policy-id caller))
    
    ;; Create policy record
    (map-set policies
      { policy-id: policy-id }
      {
        owner: caller,
        vehicle-id: vehicle-id,
        premium: BASE_PREMIUM,
        risk-score: u50, ;; Default middle risk score
        balance: initial-deposit,
        active: true,
        last-updated: burn-block-height
      }
    )
    
    ;; Update member policies list
    (let ((current-policies (default-to { policy-ids: (list) } 
                                       (map-get? member-policies { member: caller }))))
      (asserts! (< (len (get policy-ids current-policies)) MAX_POLICIES_PER_MEMBER) ERR_INVALID_INPUT)
      (map-set member-policies
        { member: caller }
        { policy-ids: (unwrap! (as-max-len? 
                                (append (get policy-ids current-policies) policy-id) u10)
                               ERR_NOT_AUTHORIZED) }
      )
    )
    
    ;; Update counters and pool balance with safe math
    (var-set policy-counter policy-id)
    (var-set total-pool-balance (unwrap! (safe-add (var-get total-pool-balance) initial-deposit) ERR_OVERFLOW))
    
    (ok policy-id)
  )
)

;; Update risk score based on telemetry data (called by oracle)
(define-public (update-risk-score (policy-id uint) (new-risk-score uint))
  (let (
    (oracle (unwrap! (var-get oracle-address) ERR_NOT_AUTHORIZED))
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq tx-sender oracle) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= new-risk-score MIN_RISK_SCORE) 
                   (<= new-risk-score MAX_RISK_SCORE)) ERR_INVALID_RISK_SCORE)
    (asserts! (get active policy) ERR_POLICY_NOT_FOUND)
    
    (let (
      (old-score (get risk-score policy))
      (score-product (unwrap! (safe-mul new-risk-score u100) ERR_OVERFLOW))
      (adjustment-factor (if (> new-risk-score old-score)
                            (unwrap! (safe-div score-product old-score) ERR_INVALID_INPUT)
                            (unwrap! (safe-div (unwrap! (safe-mul old-score u100) ERR_OVERFLOW) new-risk-score) ERR_INVALID_INPUT)))
      (premium-product (unwrap! (safe-mul BASE_PREMIUM new-risk-score) ERR_OVERFLOW))
      (new-premium (unwrap! (safe-div premium-product u50) ERR_INVALID_INPUT))
    )
      ;; Update policy with new risk score and premium
      (map-set policies
        { policy-id: policy-id }
        (merge policy {
          risk-score: new-risk-score,
          premium: new-premium,
          last-updated: burn-block-height
        })
      )
      
      ;; Record risk adjustment
      (map-set risk-adjustments
        { policy-id: policy-id }
        {
          previous-score: old-score,
          new-score: new-risk-score,
          adjustment-factor: adjustment-factor,
          timestamp: burn-block-height
        }
      )
      
      (ok new-premium)
    )
  )
)

;; Pay monthly premium
(define-public (pay-premium (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq (get owner policy) caller) ERR_NOT_AUTHORIZED)
    (asserts! (get active policy) ERR_POLICY_NOT_FOUND)
    
    (let ((premium-amount (get premium policy)))
      ;; Transfer premium to contract (external call first - reentrancy protection)
      (try! (stx-transfer? premium-amount caller (as-contract tx-sender)))
      
      ;; Update policy balance and pool balance with safe math
      (let (
        (new-policy-balance (unwrap! (safe-add (get balance policy) premium-amount) ERR_OVERFLOW))
        (new-pool-balance (unwrap! (safe-add (var-get total-pool-balance) premium-amount) ERR_OVERFLOW))
      )
        (map-set policies
          { policy-id: policy-id }
          (merge policy { 
            balance: new-policy-balance,
            last-updated: burn-block-height 
          })
        )
        
        (var-set total-pool-balance new-pool-balance)
        
        (ok premium-amount)
      )
    )
  )
)

;; Submit insurance claim
(define-public (submit-claim (policy-id uint) (claim-amount uint) (crash-data-hash (buff 32)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (current-counter (var-get claim-counter))
    (claim-id (unwrap! (safe-add current-counter u1) ERR_OVERFLOW))
    (caller tx-sender)
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq (get owner policy) caller) ERR_NOT_AUTHORIZED)
    (asserts! (get active policy) ERR_POLICY_NOT_FOUND)
    (asserts! (> claim-amount u0) ERR_INVALID_INPUT)
    (asserts! (<= claim-amount MAX_CLAIM_AMOUNT) ERR_CLAIM_AMOUNT_TOO_HIGH)
    (asserts! (<= claim-amount (var-get total-pool-balance)) ERR_POOL_INSUFFICIENT)
    
    ;; Create claim record
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: caller,
        amount: claim-amount,
        status: "pending",
        crash-data-hash: crash-data-hash,
        created-at: burn-block-height,
        votes-for: u0,
        votes-against: u0,
        voters: (list)
      }
    )
    
    (var-set claim-counter claim-id)
    
    (ok claim-id)
  )
)

;; Vote on claim (pool members can vote)
(define-public (vote-on-claim (claim-id uint) (approve bool))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) ERR_POLICY_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-pool-member caller) ERR_NOT_MEMBER)
    (asserts! (< burn-block-height (unwrap! (safe-add (get created-at claim) CLAIM_VOTING_PERIOD) ERR_OVERFLOW)) ERR_VOTING_ENDED)
    (asserts! (not (is-some (index-of (get voters claim) caller))) ERR_ALREADY_VOTED)
    
    (let (
      (updated-voters (unwrap! (as-max-len? (append (get voters claim) caller) u100) 
                              ERR_NOT_AUTHORIZED))
      (votes-for (if approve (unwrap! (safe-add (get votes-for claim) u1) ERR_OVERFLOW) (get votes-for claim)))
      (votes-against (if approve (get votes-against claim) (unwrap! (safe-add (get votes-against claim) u1) ERR_OVERFLOW)))
    )
      ;; Update claim with vote
      (map-set claims
        { claim-id: claim-id }
        (merge claim {
          votes-for: votes-for,
          votes-against: votes-against,
          voters: updated-voters
        })
      )
      
      (ok true)
    )
  )
)

;; Process approved claim
(define-public (process-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (>= burn-block-height (unwrap! (safe-add (get created-at claim) CLAIM_VOTING_PERIOD) ERR_OVERFLOW)) ERR_VOTING_ENDED)
    (asserts! (> (get votes-for claim) (get votes-against claim)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (var-get total-pool-balance) (get amount claim)) ERR_POOL_INSUFFICIENT)
    
    ;; Transfer claim amount to claimant (external call first - reentrancy protection)
    (try! (as-contract (stx-transfer? (get amount claim) tx-sender (get claimant claim))))
    
    ;; Update pool balance and claim status with safe math
    (var-set total-pool-balance (unwrap! (safe-sub (var-get total-pool-balance) (get amount claim)) ERR_UNDERFLOW))
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "approved" })
    )
    
    (ok (get amount claim))
  )
)

;; Distribute surplus to safe drivers at year end
(define-public (distribute-surplus (year uint))
  (let (
    (total-balance (var-get total-pool-balance))
    (surplus (if (> total-balance MIN_POOL_BALANCE) 
                (unwrap! (safe-sub total-balance MIN_POOL_BALANCE) ERR_UNDERFLOW)
                u0))
  )
    (asserts! (var-get pool-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> surplus u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> year u0) ERR_INVALID_INPUT)
    
    ;; This would distribute surplus based on safety scores
    ;; Implementation would iterate through all active policies
    (ok surplus)
  )
)

;; Set oracle address (only contract owner)
(define-public (set-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set oracle-address (some oracle))
    (ok true)
  )
)

;; read only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

;; Get member's policies
(define-read-only (get-member-policies (member principal))
  (map-get? member-policies { member: member })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

;; Get pool statistics
(define-read-only (get-pool-stats)
  {
    total-balance: (var-get total-pool-balance),
    total-policies: (var-get policy-counter),
    total-claims: (var-get claim-counter),
    pool-active: (var-get pool-active)
  }
)

;; Check if address is pool member
(define-read-only (is-pool-member (member principal))
  (is-some (map-get? member-policies { member: member }))
)

;; Get risk adjustment history
(define-read-only (get-risk-adjustment (policy-id uint))
  (map-get? risk-adjustments { policy-id: policy-id })
)

;; Calculate premium for risk score
(define-read-only (calculate-premium (risk-score uint))
  (/ (* BASE_PREMIUM risk-score) u50)
)

;; NEW: Security read-only functions
(define-read-only (is-pool-active)
  (var-get pool-active))

(define-read-only (get-rate-limit-blocks)
  RATE_LIMIT_BLOCKS)

(define-read-only (get-last-operation-block (user principal))
  (default-to u0 (map-get? last-operation-block user)))

(define-read-only (get-max-claim-amount)
  MAX_CLAIM_AMOUNT)

(define-read-only (get-max-policies-per-member)
  MAX_POLICIES_PER_MEMBER)

;; private functions

;; Validate risk score range
(define-private (is-valid-risk-score (score uint))
  (and (>= score MIN_RISK_SCORE) (<= score MAX_RISK_SCORE))
)

;; Calculate surplus distribution for member
(define-private (calculate-member-surplus (policy-id uint) (total-surplus uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (let (
      (risk-factor (unwrap-panic (safe-sub u100 (get risk-score policy))))
      (time-factor (if (> burn-block-height (get last-updated policy)) u100 u50))
      (product1 (unwrap-panic (safe-mul total-surplus risk-factor)))
      (product2 (unwrap-panic (safe-mul product1 time-factor)))
    )
      (unwrap-panic (safe-div product2 u10000))
    )
    u0
  )
)
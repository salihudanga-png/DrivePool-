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

(define-constant MIN_RISK_SCORE u1)
(define-constant MAX_RISK_SCORE u100)
(define-constant BASE_PREMIUM u1000000) ;; 1 STX in microSTX
(define-constant CLAIM_VOTING_PERIOD u144) ;; ~24 hours in blocks
(define-constant MIN_POOL_BALANCE u10000000) ;; 10 STX minimum pool balance

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
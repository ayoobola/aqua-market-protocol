;; AquaMarket - Decentralized Resource Allocation and Exchange Protocol
;; This smart contract enables decentralized trading of liquid resource credits
;; on the blockchain, providing transparency and efficiency for market participants.

;; ========== OWNER & ERROR CODES ==========
(define-constant admin-account tx-sender)
(define-constant error-unauthorized (err u200))
(define-constant error-insufficient-resource (err u201))
(define-constant error-transaction-unsuccessful (err u202))
(define-constant error-invalid-rate (err u203))
(define-constant error-invalid-quantity (err u204))
(define-constant error-invalid-commission (err u205))
(define-constant error-reimbursement-failed (err u206))
(define-constant error-self-transaction (err u207))
(define-constant error-capacity-exceeded (err u208))
(define-constant error-invalid-capacity (err u209))

;; ========== GLOBAL SETTINGS ==========
;; Base rate for resource units in microstacks (1 STX = 1,000,000 microstacks)
(define-data-var base-rate uint u100)

;; Maximum resource units each participant can register
(define-data-var participant-resource-ceiling uint u10000)

;; Commission percentage applied to transactions (out of 100)
(define-data-var transaction-commission uint u5)

;; Percentage returned during reimbursement (out of 100)
(define-data-var reimbursement-rate uint u90)

;; System-wide capacity limit for total resource units 
(define-data-var system-capacity-limit uint u1000000)

;; Current total resource units registered in the system
(define-data-var total-registered-resources uint u0)

;; ========== DATA STRUCTURES ==========
;; Track participant resource balances
(define-map participant-resource-balances principal uint)

;; Track participant token balances
(define-map participant-token-balances principal uint)

;; Registry of resources available for exchange
(define-map resource-exchange-registry {participant: principal} {quantity: uint, rate: uint})

;; ========== UTILITY FUNCTIONS ==========

;; Calculate commission amount for a transaction
(define-private (calculate-commission (value uint))
  (/ (* value (var-get transaction-commission)) u100))

;; Calculate reimbursement value for returned resources
(define-private (calculate-reimbursement (quantity uint))
  (/ (* quantity (var-get base-rate) (var-get reimbursement-rate)) u100))

;; Update the system-wide resource tracking
(define-private (adjust-system-resources (adjustment int))
  (let (
    (current-total (var-get total-registered-resources))
    (adjusted-total (if (< adjustment 0)
                     (if (>= current-total (to-uint (- 0 adjustment)))
                         (- current-total (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-total (to-uint adjustment))))
  )
    ;; Ensure system capacity isn't exceeded
    (asserts! (<= adjusted-total (var-get system-capacity-limit)) error-capacity-exceeded)
    ;; Update the system total
    (var-set total-registered-resources adjusted-total)
    (ok true)))

;; ========== PARTICIPANT OPERATIONS ==========

;; Register new resources in the system
(define-public (register-resources (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-resource-balances tx-sender)))
    (new-balance (+ current-balance quantity))
    (system-total (var-get total-registered-resources))
    (updated-total (+ system-total quantity))
  )
    ;; Validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (<= new-balance (var-get participant-resource-ceiling)) error-capacity-exceeded)
    (asserts! (<= updated-total (var-get system-capacity-limit)) error-capacity-exceeded)

    ;; Update participant's balance
    (map-set participant-resource-balances tx-sender new-balance)

    ;; Update system total
    (var-set total-registered-resources updated-total)

    ;; Return success
    (ok true)))

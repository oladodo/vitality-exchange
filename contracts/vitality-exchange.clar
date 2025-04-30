;; Vitality Exchange - Blockchain Platform for Wellness Metrics Trading
;; This contract enables users to securely store, trade, and monetize their wellness metrics
;; in a decentralized marketplace built on Stacks blockchain.

;; --------------------
;; Error Code Registry
;; --------------------
(define-constant admin-only-err (err u100))
(define-constant insufficient-metrics-err (err u101))
(define-constant zero-pricing-err (err u102))
(define-constant zero-quantity-err (err u103))
(define-constant invalid-fee-percentage-err (err u104))
(define-constant transaction-failure-err (err u105))
(define-constant self-transaction-err (err u106))
(define-constant capacity-exceeded-err (err u107))
(define-constant invalid-capacity-value-err (err u108))

;; --------------------
;; Contract Administration
;; --------------------
(define-constant administrator tx-sender)

;; --------------------
;; System Parameters
;; --------------------
;; Pricing and limits for platform operation
(define-data-var metric-base-price uint u200)          ;; Base price per wellness metric unit (microSTX)
(define-data-var user-metric-ceiling uint u5000)       ;; Maximum metrics per user account
(define-data-var platform-fee-rate uint u5)            ;; Platform transaction fee (percentage)
(define-data-var cancellation-return-rate uint u80)    ;; Percentage returned on transaction cancellation
(define-data-var platform-metric-ceiling uint u100000) ;; Total platform metrics capacity
(define-data-var platform-metric-count uint u0)        ;; Current metrics in platform storage

;; --------------------
;; Storage Maps
;; --------------------
;; User account records
(define-map user-metric-holdings principal uint)     ;; Tracks user's metric balance
(define-map user-currency-holdings principal uint)   ;; Tracks user's STX balance
(define-map marketplace-listings {account: principal} {quantity: uint, unit-price: uint})

;; --------------------
;; Internal Functions
;; --------------------

;; Calculate platform service fee
;; @param transaction-value: total value of the transaction
;; @returns calculated platform fee
(define-private (determine-service-fee (transaction-value uint))
  (/ (* transaction-value (var-get platform-fee-rate)) u100))

;; Calculate cancellation reimbursement
;; @param metric-count: number of metrics being cancelled
;; @returns amount to be refunded
(define-private (determine-cancellation-refund (metric-count uint))
  (/ (* metric-count (var-get metric-base-price) (var-get cancellation-return-rate)) u100))

;; Update platform's total metric count
;; @param adjustment: positive or negative adjustment to current count
;; @returns success or capacity exceeded error
(define-private (adjust-platform-metrics (adjustment int))
  (let (
    (current-count (var-get platform-metric-count))
    (adjusted-count (if (< adjustment 0)
                     (if (>= current-count (to-uint (- 0 adjustment)))
                         (- current-count (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-count (to-uint adjustment))))
  )
    (asserts! (<= adjusted-count (var-get platform-metric-ceiling)) capacity-exceeded-err)
    (var-set platform-metric-count adjusted-count)
    (ok true)))

;; --------------------
;; Public API Functions
;; --------------------

;; List wellness metrics for sale
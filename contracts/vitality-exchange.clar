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
;; @param quantity: number of metrics to offer
;; @param unit-price: price per metric unit
;; @returns success or error
(define-public (publish-metric-offering (quantity uint) (unit-price uint))
  (let (
    (owner-balance (default-to u0 (map-get? user-metric-holdings tx-sender)))
    (current-listing-quantity (get quantity (default-to {quantity: u0, unit-price: u0} 
                                            (map-get? marketplace-listings {account: tx-sender}))))
    (total-listed-quantity (+ quantity current-listing-quantity))
  )
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (> unit-price u0) zero-pricing-err)
    (asserts! (>= owner-balance total-listed-quantity) insufficient-metrics-err)
    (try! (adjust-platform-metrics (to-int quantity)))
    (map-set marketplace-listings {account: tx-sender} 
            {quantity: total-listed-quantity, unit-price: unit-price})
    (ok true)))

;; Remove metrics from marketplace
;; @param quantity: number of metrics to delist
;; @returns success or error
(define-public (withdraw-metric-offering (quantity uint))
  (let (
    (current-listing-quantity (get quantity (default-to {quantity: u0, unit-price: u0} 
                                           (map-get? marketplace-listings {account: tx-sender}))))
  )
    (asserts! (>= current-listing-quantity quantity) insufficient-metrics-err)
    (try! (adjust-platform-metrics (to-int (- quantity))))
    (map-set marketplace-listings {account: tx-sender} 
             {quantity: (- current-listing-quantity quantity), 
              unit-price: (get unit-price (default-to {quantity: u0, unit-price: u0} 
                                         (map-get? marketplace-listings {account: tx-sender})))})
    (ok true)))

;; Purchase metrics from another user
;; @param provider: account selling the metrics
;; @param quantity: number of metrics to purchase
;; @returns success or error
(define-public (acquire-provider-metrics (provider principal) (quantity uint))
  (let (
    (listing-details (default-to {quantity: u0, unit-price: u0} 
                    (map-get? marketplace-listings {account: provider})))
    (metrics-cost (* quantity (get unit-price listing-details)))
    (service-fee (determine-service-fee metrics-cost))
    (total-payment (+ metrics-cost service-fee))
    (provider-balance (default-to u0 (map-get? user-metric-holdings provider)))
    (buyer-currency (default-to u0 (map-get? user-currency-holdings tx-sender)))
    (provider-currency (default-to u0 (map-get? user-currency-holdings provider)))
    (admin-currency (default-to u0 (map-get? user-currency-holdings administrator)))
  )
    (asserts! (not (is-eq tx-sender provider)) self-transaction-err)
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (>= (get quantity listing-details) quantity) insufficient-metrics-err)
    (asserts! (>= provider-balance quantity) insufficient-metrics-err)
    (asserts! (>= buyer-currency total-payment) insufficient-metrics-err)

    ;; Update provider's metric balance and listing
    (map-set user-metric-holdings provider (- provider-balance quantity))
    (map-set marketplace-listings {account: provider} 
             {quantity: (- (get quantity listing-details) quantity), 
              unit-price: (get unit-price listing-details)})

    ;; Update buyer's currency and metric balances
    (map-set user-currency-holdings tx-sender (- buyer-currency total-payment))
    (map-set user-metric-holdings tx-sender 
             (+ (default-to u0 (map-get? user-metric-holdings tx-sender)) quantity))

    ;; Distribute payment to provider and platform
    (map-set user-currency-holdings provider (+ provider-currency metrics-cost))
    (map-set user-currency-holdings administrator (+ admin-currency service-fee))

    (ok true)))

;; Process metric return and partial refund
;; @param quantity: number of metrics to return
;; @returns success or error
(define-public (return-acquired-metrics (quantity uint))
  (let (
    (user-metrics (default-to u0 (map-get? user-metric-holdings tx-sender)))
    (refund-amount (determine-cancellation-refund quantity))
    (admin-currency (default-to u0 (map-get? user-currency-holdings administrator)))
  )
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (>= user-metrics quantity) insufficient-metrics-err)
    (asserts! (>= admin-currency refund-amount) transaction-failure-err)

    ;; Update user's metric balance
    (map-set user-metric-holdings tx-sender (- user-metrics quantity))

    ;; Process refund transaction
    (map-set user-currency-holdings tx-sender 
             (+ (default-to u0 (map-get? user-currency-holdings tx-sender)) refund-amount))
    (map-set user-currency-holdings administrator (- admin-currency refund-amount))

    ;; Return metrics to administrator account
    (map-set user-metric-holdings administrator 
             (+ (default-to u0 (map-get? user-metric-holdings administrator)) quantity))

    ;; Update platform metric count
    (try! (adjust-platform-metrics (to-int (- quantity))))

    (ok true)))

;; Submit new wellness metrics to the platform
;; @param quantity: number of metrics to upload
;; @returns success or error
(define-public (submit-wellness-metrics (quantity uint))
  (let (
    (current-user-metrics (default-to u0 (map-get? user-metric-holdings tx-sender)))
    (max-metrics (var-get user-metric-ceiling))
    (unit-price (var-get metric-base-price))
    (submission-cost (* quantity unit-price))
    (user-currency (default-to u0 (map-get? user-currency-holdings tx-sender)))
    (admin-currency (default-to u0 (map-get? user-currency-holdings administrator)))
  )
    ;; Validation checks
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (>= user-currency submission-cost) insufficient-metrics-err)
    (asserts! (<= (+ current-user-metrics quantity) max-metrics) capacity-exceeded-err)
    (try! (adjust-platform-metrics (to-int quantity)))

    ;; Update user's metric balance
    (map-set user-metric-holdings tx-sender (+ current-user-metrics quantity))

    ;; Process payment
    (map-set user-currency-holdings tx-sender (- user-currency submission-cost))
    (map-set user-currency-holdings administrator (+ admin-currency submission-cost))

    (ok true)))

;; Transfer metrics between accounts
;; @param recipient: account receiving the metrics
;; @param quantity: number of metrics to transfer
;; @returns success or error
(define-public (transmit-metrics-to-recipient (recipient principal) (quantity uint))
  (let (
    (sender-metrics (default-to u0 (map-get? user-metric-holdings tx-sender)))
    (recipient-metrics (default-to u0 (map-get? user-metric-holdings recipient)))
    (recipient-limit (var-get user-metric-ceiling))
  )
    ;; Validation checks
    (asserts! (>= sender-metrics quantity) insufficient-metrics-err)
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (not (is-eq tx-sender recipient)) self-transaction-err)
    (asserts! (<= (+ recipient-metrics quantity) recipient-limit) capacity-exceeded-err)

    ;; Update metric balances
    (map-set user-metric-holdings tx-sender (- sender-metrics quantity))
    (map-set user-metric-holdings recipient (+ recipient-metrics quantity))

    (ok true)))

;; --------------------
;; Administrative Functions
;; --------------------

;; Emergency intervention for metric recovery
;; @param source-account: account to recover from
;; @param quantity: quantity to recover
;; @param destination-account: account to transfer recovered metrics to
;; @returns success or error
(define-public (facilitate-emergency-recovery (source-account principal) (quantity uint) (destination-account principal))
  (let (
    (source-metrics (default-to u0 (map-get? user-metric-holdings source-account)))
    (destination-metrics (default-to u0 (map-get? user-metric-holdings destination-account)))
    (max-metrics (var-get user-metric-ceiling))
  )
    ;; Admin authorization check
    (asserts! (is-eq tx-sender administrator) admin-only-err)

    ;; Validation checks
    (asserts! (>= source-metrics quantity) insufficient-metrics-err)
    (asserts! (> quantity u0) zero-quantity-err)
    (asserts! (<= (+ destination-metrics quantity) max-metrics) capacity-exceeded-err)

    ;; Process transfer
    (map-set user-metric-holdings source-account (- source-metrics quantity))
    (map-set user-metric-holdings destination-account (+ destination-metrics quantity))

    ;; Audit logging
    (print {operation: "emergency-recovery", 
            source: source-account, 
            destination: destination-account, 
            quantity: quantity})

    (ok true)))

;; Update platform capacity limit
;; @param new-capacity: new total capacity for platform
;; @returns success or error
(define-public (modify-platform-capacity (new-capacity uint))
  (begin
    ;; Admin authorization check
    (asserts! (is-eq tx-sender administrator) admin-only-err)

    ;; Validation check
    (asserts! (>= new-capacity (var-get platform-metric-count)) invalid-capacity-value-err)

    ;; Update capacity
    (var-set platform-metric-ceiling new-capacity)

    ;; Audit logging
    (print {operation: "capacity-modified", 
            previous-capacity: (var-get platform-metric-ceiling), 
            new-capacity: new-capacity})

    (ok true)))

;; Currency withdrawal from platform
;; @param amount: STX amount to withdraw (in microstacks)
;; @returns success or error
(define-public (extract-currency (amount uint))
  (let (
    (user-balance (default-to u0 (map-get? user-currency-holdings tx-sender)))
  )
    ;; Validation checks
    (asserts! (>= user-balance amount) insufficient-metrics-err)
    (asserts! (> amount u0) zero-quantity-err)

    ;; Process withdrawal
    (map-set user-currency-holdings tx-sender (- user-balance amount))

    ;; Audit logging
    (print {operation: "currency-withdrawal", 
            account: tx-sender, 
            amount: amount})

    (ok true)))

;; Update metric pricing
;; @param revised-price: new price per metric unit (in microstacks)
;; @returns success or error
(define-public (revise-metric-pricing (revised-price uint))
  (begin
    ;; Admin authorization check
    (asserts! (is-eq tx-sender administrator) admin-only-err)

    ;; Validation check
    (asserts! (> revised-price u0) zero-pricing-err)

    ;; Update price
    (var-set metric-base-price revised-price)

    ;; Audit logging
    (print {operation: "price-revision", 
            previous-price: (var-get metric-base-price), 
            new-price: revised-price})

    (ok true)))

;; Fund user account with currency
;; @param amount: STX amount to deposit (in microstacks)
;; @returns success or error
(define-public (fund-account (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-currency-holdings tx-sender)))
  )
    ;; Validation check
    (asserts! (> amount u0) zero-quantity-err)

    ;; Process deposit
    (map-set user-currency-holdings tx-sender (+ current-balance amount))

    ;; Audit logging
    (print {operation: "account-funding", 
            account: tx-sender, 
            amount: amount})

    (ok true)))



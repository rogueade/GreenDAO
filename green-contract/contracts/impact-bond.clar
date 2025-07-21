;; Bond Marketplace Contract
;; Handles secondary market trading and order matching

;; Constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_ORDER (err u201))
(define-constant ERR_ORDER_NOT_FOUND (err u202))
(define-constant ERR_INSUFFICIENT_BALANCE (err u203))
(define-constant ERR_INVALID_PRICE (err u204))

;; Data Variables
(define-data-var next-order-id uint u1)
(define-data-var marketplace-fee-rate uint u100) ;; 1% in basis points

;; Data Maps
(define-map market-orders
  { order-id: uint }
  {
    bond-id: uint,
    seller: principal,
    amount: uint,
    price-per-token: uint,
    order-type: (string-ascii 8), ;; "sell" or "buy"
    created-at: uint,
    is-active: bool
  }
)

(define-map order-fills
  { order-id: uint, fill-id: uint }
  {
    buyer: principal,
    amount: uint,
    price: uint,
    filled-at: uint
  }
)

;; Read-only functions
(define-read-only (get-order (order-id uint))
  (map-get? market-orders { order-id: order-id })
)

(define-read-only (get-next-order-id)
  (var-get next-order-id)
)

;; Public functions
(define-public (create-sell-order (bond-id uint) (amount uint) (price-per-token uint))
  (let (
    (order-id (var-get next-order-id))
  )
    (asserts! (> amount u0) ERR_INVALID_ORDER)
    (asserts! (> price-per-token u0) ERR_INVALID_PRICE)
    
    ;; Verify seller has sufficient balance (would call main contract)
    ;; This is simplified - in practice would integrate with main contract
    
    (map-set market-orders
      { order-id: order-id }
      {
        bond-id: bond-id,
        seller: tx-sender,
        amount: amount,
        price-per-token: price-per-token,
        order-type: "sell",
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

(define-public (fill-sell-order (order-id uint) (amount uint))
  (let (
    (order-info (unwrap! (map-get? market-orders { order-id: order-id }) ERR_ORDER_NOT_FOUND))
    (total-cost (* amount (get price-per-token order-info)))
  )
    (asserts! (get is-active order-info) ERR_INVALID_ORDER)
    (asserts! (is-eq (get order-type order-info) "sell") ERR_INVALID_ORDER)
    (asserts! (>= (get amount order-info) amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update order
    (if (is-eq (get amount order-info) amount)
      (map-set market-orders
        { order-id: order-id }
        (merge order-info { is-active: false, amount: u0 })
      )
      (map-set market-orders
        { order-id: order-id }
        (merge order-info { amount: (- (get amount order-info) amount) })
      )
    )
    
    ;; In practice, this would integrate with the main contract to transfer tokens
    (ok true)
  )
)

(define-public (cancel-order (order-id uint))
  (let (
    (order-info (unwrap! (map-get? market-orders { order-id: order-id }) ERR_ORDER_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get seller order-info)) ERR_UNAUTHORIZED)
    (asserts! (get is-active order-info) ERR_INVALID_ORDER)
    
    (ok (map-set market-orders
      { order-id: order-id }
      (merge order-info { is-active: false })
    ))
  )
)

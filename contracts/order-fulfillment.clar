;; Order Fulfillment Contract
;; Manage orders from customers to verified local artisans

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u501))
(define-constant ERR-ORDER-NOT-FOUND (err u502))
(define-constant ERR-INVALID-STATUS (err u503))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u504))
(define-constant ERR-INVALID-AMOUNT (err u505))

;; Constants
(define-constant PLATFORM-FEE-RATE u250) ;; 2.5% platform fee
(define-constant ESCROW-PERIOD u1008) ;; ~7 days

;; Data variables
(define-data-var order-counter uint u0)
(define-data-var total-orders uint u0)

;; Data maps
(define-map orders
    uint ;; order-id
    {
        customer: principal,
        artisan: principal,
        product-id: uint,
        quantity: uint,
        total-amount: uint,
        platform-fee: uint,
        status: (string-ascii 20), ;; "pending", "confirmed", "shipped", "delivered", "cancelled"
        created-at: uint,
        confirmed-at: (optional uint),
        shipped-at: (optional uint),
        delivery-address: (string-ascii 200),
        tracking-number: (optional (string-ascii 50))
    }
)

(define-map order-escrow
    uint ;; order-id
    {
        amount-held: uint,
        release-date: uint,
        is-released: bool
    }
)

(define-map shipping-updates
    { order-id: uint, update-id: uint }
    {
        status: (string-ascii 50),
        location: (string-ascii 100),
        timestamp: uint,
        notes: (optional (string-ascii 200))
    }
)

;; Private functions
(define-private (get-next-order-id)
    (let ((current-id (var-get order-counter)))
        (var-set order-counter (+ current-id u1))
        (+ current-id u1)
    )
)

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount PLATFORM-FEE-RATE) u10000)
)

;; Read-only functions
(define-read-only (get-order (order-id uint))
    (map-get? orders order-id)
)

(define-read-only (get-order-escrow (order-id uint))
    (map-get? order-escrow order-id)
)

(define-read-only (get-shipping-update (order-id uint) (update-id uint))
    (map-get? shipping-updates { order-id: order-id, update-id: update-id })
)

(define-read-only (get-total-orders)
    (var-get total-orders)
)

;; Public functions
(define-public (create-order (artisan principal) (product-id uint) (quantity uint) (unit-price uint) (delivery-address (string-ascii 200)))
    (let 
        (
            (order-id (get-next-order-id))
            (total-amount (* quantity unit-price))
            (platform-fee (calculate-platform-fee total-amount))
        )
        (asserts! (> quantity u0) ERR-INVALID-AMOUNT)
        (asserts! (> unit-price u0) ERR-INVALID-AMOUNT)
        
        ;; Create order
        (map-set orders order-id
            {
                customer: tx-sender,
                artisan: artisan,
                product-id: product-id,
                quantity: quantity,
                total-amount: total-amount,
                platform-fee: platform-fee,
                status: "pending",
                created-at: block-height,
                confirmed-at: none,
                shipped-at: none,
                delivery-address: delivery-address,
                tracking-number: none
            }
        )
        
        ;; Set up escrow (simplified - in production would handle actual STX transfer)
        (map-set order-escrow order-id
            {
                amount-held: total-amount,
                release-date: (+ block-height ESCROW-PERIOD),
                is-released: false
            }
        )
        
        (var-set total-orders (+ (var-get total-orders) u1))
        (ok order-id)
    )
)

(define-public (confirm-order (order-id uint))
    (let 
        (
            (order-data (unwrap! (map-get? orders order-id) ERR-ORDER-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get artisan order-data)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status order-data) "pending") ERR-INVALID-STATUS)
        
        (map-set orders order-id
            (merge order-data {
                status: "confirmed",
                confirmed-at: (some block-height)
            })
        )
        
        (ok true)
    )
)

(define-public (ship-order (order-id uint) (tracking-number (string-ascii 50)))
    (let 
        (
            (order-data (unwrap! (map-get? orders order-id) ERR-ORDER-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get artisan order-data)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status order-data) "confirmed") ERR-INVALID-STATUS)
        
        (map-set orders order-id
            (merge order-data {
                status: "shipped",
                shipped-at: (some block-height),
                tracking-number: (some tracking-number)
            })
        )
        
        ;; Add shipping update
        (map-set shipping-updates { order-id: order-id, update-id: u1 }
            {
                status: "shipped",
                location: "Origin",
                timestamp: block-height,
                notes: none
            }
        )
        
        (ok true)
    )
)

(define-public (add-shipping-update (order-id uint) (update-id uint) (status (string-ascii 50)) (location (string-ascii 100)) (notes (optional (string-ascii 200))))
    (let 
        (
            (order-data (unwrap! (map-get? orders order-id) ERR-ORDER-NOT-FOUND))
        )
        ;; For simplicity, allow any user to add updates (in production would be restricted to shipping providers)
        
        (map-set shipping-updates { order-id: order-id, update-id: update-id }
            {
                status: status,
                location: location,
                timestamp: block-height,
                notes: notes
            }
        )
        
        (ok true)
    )
)

(define-public (confirm-delivery (order-id uint))
    (let 
        (
            (order-data (unwrap! (map-get? orders order-id) ERR-ORDER-NOT-FOUND))
            (escrow-data (unwrap! (map-get? order-escrow order-id) ERR-ORDER-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get customer order-data)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status order-data) "shipped") ERR-INVALID-STATUS)
        
        ;; Update order status
        (map-set orders order-id
            (merge order-data {
                status: "delivered"
            })
        )
        
        ;; Release escrow (simplified)
        (map-set order-escrow order-id
            (merge escrow-data {
                is-released: true
            })
        )
        
        ;; Record sale for artisan (would call artisan-verification contract)
        ;; (try! (contract-call? .artisan-verification record-sale (get artisan order-data)))
        
        (ok true)
    )
)

(define-public (cancel-order (order-id uint) (reason (string-ascii 200)))
    (let 
        (
            (order-data (unwrap! (map-get? orders order-id) ERR-ORDER-NOT-FOUND))
            (escrow-data (unwrap! (map-get? order-escrow order-id) ERR-ORDER-NOT-FOUND))
        )
        ;; Allow customer or artisan to cancel if not yet shipped
        (asserts! (or 
            (is-eq tx-sender (get customer order-data))
            (is-eq tx-sender (get artisan order-data))
        ) ERR-UNAUTHORIZED)
        (asserts! (or 
            (is-eq (get status order-data) "pending")
            (is-eq (get status order-data) "confirmed")
        ) ERR-INVALID-STATUS)
        
        ;; Update order status
        (map-set orders order-id
            (merge order-data {
                status: "cancelled"
            })
        )
        
        ;; Release escrow back to customer (simplified)
        (map-set order-escrow order-id
            (merge escrow-data {
                is-released: true
            })
        )
        
        (ok true)
    )
)


;; title: order-fulfillment
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;


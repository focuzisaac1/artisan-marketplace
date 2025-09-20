;; Artisan Verification Contract
;; Verify artisan credentials and authenticate handmade products

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-ARTISAN-NOT-FOUND (err u402))
(define-constant ERR-ALREADY-VERIFIED (err u403))
(define-constant ERR-INVALID-CREDENTIALS (err u404))
(define-constant ERR-PRODUCT-NOT-FOUND (err u405))

;; Constants
(define-constant VERIFICATION-FEE u100000) ;; 0.1 STX
(define-constant MIN-PORTFOLIO-ITEMS u3)

;; Data variables
(define-data-var artisan-counter uint u0)
(define-data-var product-counter uint u0)

;; Data maps
(define-map artisans
    principal
    {
        name: (string-ascii 100),
        craft-type: (string-ascii 50),
        experience-years: uint,
        location: (string-ascii 100),
        is-verified: bool,
        verification-date: (optional uint),
        portfolio-count: uint,
        total-sales: uint,
        reputation-score: uint
    }
)

(define-map products
    uint ;; product-id
    {
        artisan: principal,
        name: (string-ascii 100),
        description: (string-ascii 500),
        category: (string-ascii 50),
        price: uint,
        is-authentic: bool,
        materials: (string-ascii 200),
        creation-time: uint,
        created-at: uint
    }
)

(define-map portfolio-items
    { artisan: principal, item-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        image-url: (string-ascii 200),
        completion-date: uint
    }
)

;; Private functions
(define-private (get-next-artisan-id)
    (let ((current-id (var-get artisan-counter)))
        (var-set artisan-counter (+ current-id u1))
        (+ current-id u1)
    )
)

(define-private (get-next-product-id)
    (let ((current-id (var-get product-counter)))
        (var-set product-counter (+ current-id u1))
        (+ current-id u1)
    )
)

;; Read-only functions
(define-read-only (get-artisan (artisan principal))
    (map-get? artisans artisan)
)

(define-read-only (get-product (product-id uint))
    (map-get? products product-id)
)

(define-read-only (get-portfolio-item (artisan principal) (item-id uint))
    (map-get? portfolio-items { artisan: artisan, item-id: item-id })
)

(define-read-only (is-artisan-verified (artisan principal))
    (match (map-get? artisans artisan)
        artisan-data (get is-verified artisan-data)
        false
    )
)

;; Public functions
(define-public (register-artisan (name (string-ascii 100)) (craft-type (string-ascii 50)) (experience-years uint) (location (string-ascii 100)))
    (begin
        (map-set artisans tx-sender
            {
                name: name,
                craft-type: craft-type,
                experience-years: experience-years,
                location: location,
                is-verified: false,
                verification-date: none,
                portfolio-count: u0,
                total-sales: u0,
                reputation-score: u50
            }
        )
        (ok true)
    )
)

(define-public (add-portfolio-item (title (string-ascii 100)) (description (string-ascii 300)) (image-url (string-ascii 200)))
    (let 
        (
            (artisan-data (unwrap! (map-get? artisans tx-sender) ERR-ARTISAN-NOT-FOUND))
            (item-id (+ (get portfolio-count artisan-data) u1))
        )
        (map-set portfolio-items { artisan: tx-sender, item-id: item-id }
            {
                title: title,
                description: description,
                image-url: image-url,
                completion-date: block-height
            }
        )
        
        (map-set artisans tx-sender
            (merge artisan-data {
                portfolio-count: item-id
            })
        )
        
        (ok item-id)
    )
)

(define-public (verify-artisan (artisan principal))
    (let 
        (
            (artisan-data (unwrap! (map-get? artisans artisan) ERR-ARTISAN-NOT-FOUND))
        )
        ;; For simplicity, allow any user to verify (in production would be admin-only)
        (asserts! (not (get is-verified artisan-data)) ERR-ALREADY-VERIFIED)
        (asserts! (>= (get portfolio-count artisan-data) MIN-PORTFOLIO-ITEMS) ERR-INVALID-CREDENTIALS)
        
        (map-set artisans artisan
            (merge artisan-data {
                is-verified: true,
                verification-date: (some block-height)
            })
        )
        
        (ok true)
    )
)

(define-public (create-product (name (string-ascii 100)) (description (string-ascii 500)) (category (string-ascii 50)) (price uint) (materials (string-ascii 200)) (creation-time uint))
    (let 
        (
            (product-id (get-next-product-id))
            (artisan-data (unwrap! (map-get? artisans tx-sender) ERR-ARTISAN-NOT-FOUND))
        )
        (asserts! (get is-verified artisan-data) ERR-UNAUTHORIZED)
        
        (map-set products product-id
            {
                artisan: tx-sender,
                name: name,
                description: description,
                category: category,
                price: price,
                is-authentic: true, ;; All products from verified artisans are authentic
                materials: materials,
                creation-time: creation-time,
                created-at: block-height
            }
        )
        
        (ok product-id)
    )
)

(define-public (update-reputation (artisan principal) (new-score uint))
    (let 
        (
            (artisan-data (unwrap! (map-get? artisans artisan) ERR-ARTISAN-NOT-FOUND))
        )
        ;; For simplicity, allow any user to update (in production would be restricted)
        (asserts! (<= new-score u100) ERR-INVALID-CREDENTIALS)
        
        (map-set artisans artisan
            (merge artisan-data {
                reputation-score: new-score
            })
        )
        
        (ok true)
    )
)

(define-public (record-sale (artisan principal))
    (let 
        (
            (artisan-data (unwrap! (map-get? artisans artisan) ERR-ARTISAN-NOT-FOUND))
        )
        (map-set artisans artisan
            (merge artisan-data {
                total-sales: (+ (get total-sales artisan-data) u1)
            })
        )
        
        (ok true)
    )
)


;; title: artisan-verification
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


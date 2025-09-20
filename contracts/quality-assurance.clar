;; Quality Assurance Contract
;; Community-driven quality ratings and authenticity guarantees

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u601))
(define-constant ERR-REVIEW-NOT-FOUND (err u602))
(define-constant ERR-ALREADY-REVIEWED (err u603))
(define-constant ERR-INVALID-RATING (err u604))
(define-constant ERR-ORDER-NOT-FOUND (err u605))

;; Constants
(define-constant MIN-RATING u1)
(define-constant MAX-RATING u5)
(define-constant QUALITY-THRESHOLD u400) ;; 4.0 average rating

;; Data variables
(define-data-var review-counter uint u0)

;; Data maps
(define-map product-reviews
    { product-id: uint, reviewer: principal }
    {
        rating: uint, ;; 1-5 scale
        quality-score: uint, ;; 1-100 scale
        authenticity-verified: bool,
        review-text: (string-ascii 500),
        photos-attached: uint,
        created-at: uint,
        helpful-votes: uint
    }
)

(define-map product-ratings
    uint ;; product-id
    {
        total-reviews: uint,
        average-rating: uint,
        average-quality: uint,
        authenticity-score: uint,
        last-updated: uint
    }
)

(define-map artisan-ratings
    principal
    {
        total-reviews: uint,
        average-rating: uint,
        quality-score: uint,
        authenticity-rate: uint,
        total-products: uint,
        verified-products: uint
    }
)

(define-map quality-disputes
    uint ;; dispute-id
    {
        product-id: uint,
        customer: principal,
        artisan: principal,
        issue-type: (string-ascii 50),
        description: (string-ascii 500),
        status: (string-ascii 20), ;; "open", "investigating", "resolved", "closed"
        resolution: (optional (string-ascii 300)),
        created-at: uint
    }
)

;; Private functions
(define-private (get-next-review-id)
    (let ((current-id (var-get review-counter)))
        (var-set review-counter (+ current-id u1))
        (+ current-id u1)
    )
)

(define-private (is-valid-rating (rating uint))
    (and (>= rating MIN-RATING) (<= rating MAX-RATING))
)

(define-private (calculate-weighted-average (current-avg uint) (current-count uint) (new-rating uint))
    (/ (+ (* current-avg current-count) new-rating) (+ current-count u1))
)

;; Read-only functions
(define-read-only (get-product-review (product-id uint) (reviewer principal))
    (map-get? product-reviews { product-id: product-id, reviewer: reviewer })
)

(define-read-only (get-product-ratings (product-id uint))
    (map-get? product-ratings product-id)
)

(define-read-only (get-artisan-ratings (artisan principal))
    (map-get? artisan-ratings artisan)
)

(define-read-only (get-quality-dispute (dispute-id uint))
    (map-get? quality-disputes dispute-id)
)

(define-read-only (is-high-quality-product (product-id uint))
    (match (map-get? product-ratings product-id)
        ratings
            (>= (get average-quality ratings) QUALITY-THRESHOLD)
        false
    )
)

;; Public functions
(define-public (submit-review (product-id uint) (rating uint) (quality-score uint) (authenticity-verified bool) (review-text (string-ascii 500)) (photos-attached uint))
    (let 
        (
            (existing-review (map-get? product-reviews { product-id: product-id, reviewer: tx-sender }))
        )
        (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
        (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
        (asserts! (and (>= quality-score u1) (<= quality-score u100)) ERR-INVALID-RATING)
        
        ;; Create review
        (map-set product-reviews { product-id: product-id, reviewer: tx-sender }
            {
                rating: rating,
                quality-score: quality-score,
                authenticity-verified: authenticity-verified,
                review-text: review-text,
                photos-attached: photos-attached,
                created-at: block-height,
                helpful-votes: u0
            }
        )
        
        ;; Update product ratings
        (update-product-ratings product-id rating quality-score authenticity-verified)
        
        (ok true)
    )
)

(define-public (vote-helpful (product-id uint) (reviewer principal))
    (let 
        (
            (review (unwrap! (map-get? product-reviews { product-id: product-id, reviewer: reviewer }) ERR-REVIEW-NOT-FOUND))
        )
        (asserts! (not (is-eq tx-sender reviewer)) ERR-UNAUTHORIZED)
        
        (map-set product-reviews { product-id: product-id, reviewer: reviewer }
            (merge review {
                helpful-votes: (+ (get helpful-votes review) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (report-quality-issue (product-id uint) (artisan principal) (issue-type (string-ascii 50)) (description (string-ascii 500)))
    (let 
        (
            (dispute-id (get-next-review-id))
        )
        (map-set quality-disputes dispute-id
            {
                product-id: product-id,
                customer: tx-sender,
                artisan: artisan,
                issue-type: issue-type,
                description: description,
                status: "open",
                resolution: none,
                created-at: block-height
            }
        )
        
        (ok dispute-id)
    )
)

(define-public (resolve-quality-dispute (dispute-id uint) (resolution (string-ascii 300)))
    (let 
        (
            (dispute (unwrap! (map-get? quality-disputes dispute-id) ERR-REVIEW-NOT-FOUND))
        )
        ;; For simplicity, allow any user to resolve (in production would be admin-only)
        (asserts! (is-eq (get status dispute) "open") ERR-UNAUTHORIZED)
        
        (map-set quality-disputes dispute-id
            (merge dispute {
                status: "resolved",
                resolution: (some resolution)
            })
        )
        
        (ok true)
    )
)

(define-public (verify-product-authenticity (product-id uint) (is-authentic bool) (verification-notes (string-ascii 300)))
    ;; For simplicity, allow any user to verify (in production would be expert verifiers only)
    (match (map-get? product-ratings product-id)
        ratings
            (begin
                (map-set product-ratings product-id
                    (merge ratings {
                        authenticity-score: (if is-authentic u100 u0),
                        last-updated: block-height
                    })
                )
                (ok true)
            )
        ;; Initialize ratings if they don't exist
        (begin
            (map-set product-ratings product-id
                {
                    total-reviews: u0,
                    average-rating: u0,
                    average-quality: u0,
                    authenticity-score: (if is-authentic u100 u0),
                    last-updated: block-height
                }
            )
            (ok true)
        )
    )
)

;; Private helper functions
(define-private (update-product-ratings (product-id uint) (new-rating uint) (new-quality uint) (is-authentic bool))
    (match (map-get? product-ratings product-id)
        current-ratings
            (let 
                (
                    (new-count (+ (get total-reviews current-ratings) u1))
                    (new-avg-rating (calculate-weighted-average 
                        (get average-rating current-ratings)
                        (get total-reviews current-ratings)
                        new-rating
                    ))
                    (new-avg-quality (calculate-weighted-average
                        (get average-quality current-ratings)
                        (get total-reviews current-ratings)
                        new-quality
                    ))
                )
                (map-set product-ratings product-id
                    (merge current-ratings {
                        total-reviews: new-count,
                        average-rating: new-avg-rating,
                        average-quality: new-avg-quality,
                        last-updated: block-height
                    })
                )
            )
        ;; Initialize if no ratings exist
        (map-set product-ratings product-id
            {
                total-reviews: u1,
                average-rating: new-rating,
                average-quality: new-quality,
                authenticity-score: (if is-authentic u100 u50),
                last-updated: block-height
            }
        )
    )
)


;; title: quality-assurance
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


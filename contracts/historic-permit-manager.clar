;; historic-permit-manager
;; Manages historic preservation permits with architectural review and compliance tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-input (err u103))

;; Data Variables
(define-data-var permit-counter uint u0)
(define-data-var property-counter uint u0)
(define-data-var review-counter uint u0)

;; Data Maps
(define-map historic-properties
  uint
  {
    address: (string-ascii 200),
    designation: (string-ascii 100),
    year-built: uint,
    significance: (string-utf8 500),
    owner: principal,
    designated-at: uint
  }
)

(define-map permits
  uint
  {
    property-id: uint,
    applicant: principal,
    work-description: (string-utf8 500),
    estimated-cost: uint,
    status: (string-ascii 20),
    submitted-at: uint,
    approved-at: (optional uint),
    reviewer: (optional principal)
  }
)

(define-map architectural-reviews
  uint
  {
    permit-id: uint,
    reviewer: principal,
    recommendation: (string-ascii 20),
    comments: (string-utf8 500),
    reviewed-at: uint
  }
)

(define-map property-permits
  uint
  (list 30 uint)
)

(define-map authorized-reviewers principal bool)

;; Authorization
(define-public (add-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-reviewers reviewer true))
  )
)

(define-public (remove-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-delete authorized-reviewers reviewer))
  )
)

(define-read-only (is-reviewer (user principal))
  (default-to false (map-get? authorized-reviewers user))
)

;; Property Management
(define-public (designate-property (address (string-ascii 200))
                                    (designation (string-ascii 100))
                                    (year-built uint)
                                    (significance (string-utf8 500))
                                    (owner principal))
  (let
    ((property-id (+ (var-get property-counter) u1)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-reviewer tx-sender)) err-unauthorized)
    (asserts! (> (len address) u0) err-invalid-input)
    
    (map-set historic-properties property-id {
      address: address,
      designation: designation,
      year-built: year-built,
      significance: significance,
      owner: owner,
      designated-at: block-height
    })
    
    (var-set property-counter property-id)
    (ok property-id)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? historic-properties property-id)
)

;; Permit Management
(define-public (apply-for-permit (property-id uint)
                                  (work-description (string-utf8 500))
                                  (estimated-cost uint))
  (let
    (
      (permit-id (+ (var-get permit-counter) u1))
      (property (unwrap! (map-get? historic-properties property-id) err-not-found))
      (permit-history (default-to (list) (map-get? property-permits property-id)))
    )
    (asserts! (> (len work-description) u0) err-invalid-input)
    (asserts! (> estimated-cost u0) err-invalid-input)
    
    (map-set permits permit-id {
      property-id: property-id,
      applicant: tx-sender,
      work-description: work-description,
      estimated-cost: estimated-cost,
      status: "pending",
      submitted-at: block-height,
      approved-at: none,
      reviewer: none
    })
    
    (map-set property-permits property-id
      (unwrap! (as-max-len? (append permit-history permit-id) u30) err-invalid-input))
    
    (var-set permit-counter permit-id)
    (ok permit-id)
  )
)

(define-public (assign-reviewer (permit-id uint) (reviewer principal))
  (let
    ((permit (unwrap! (map-get? permits permit-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-reviewer tx-sender)) err-unauthorized)
    (asserts! (is-reviewer reviewer) err-unauthorized)
    
    (ok (map-set permits permit-id
      (merge permit {reviewer: (some reviewer), status: "under-review"})))
  )
)

(define-public (submit-review (permit-id uint)
                                (recommendation (string-ascii 20))
                                (comments (string-utf8 500)))
  (let
    (
      (permit (unwrap! (map-get? permits permit-id) err-not-found))
      (review-id (+ (var-get review-counter) u1))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-reviewer tx-sender)) err-unauthorized)
    (asserts! (> (len comments) u0) err-invalid-input)
    
    (map-set architectural-reviews review-id {
      permit-id: permit-id,
      reviewer: tx-sender,
      recommendation: recommendation,
      comments: comments,
      reviewed-at: block-height
    })
    
    (var-set review-counter review-id)
    (ok review-id)
  )
)

(define-public (approve-permit (permit-id uint))
  (let
    ((permit (unwrap! (map-get? permits permit-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-reviewer tx-sender)) err-unauthorized)
    
    (ok (map-set permits permit-id
      (merge permit {status: "approved", approved-at: (some block-height)})))
  )
)

(define-public (deny-permit (permit-id uint))
  (let
    ((permit (unwrap! (map-get? permits permit-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-reviewer tx-sender)) err-unauthorized)
    
    (ok (map-set permits permit-id
      (merge permit {status: "denied"})))
  )
)

(define-read-only (get-permit (permit-id uint))
  (map-get? permits permit-id)
)

(define-read-only (get-property-permits (property-id uint))
  (map-get? property-permits property-id)
)

(define-read-only (get-review (review-id uint))
  (map-get? architectural-reviews review-id)
)

;; Counters
(define-read-only (get-permit-counter)
  (ok (var-get permit-counter))
)

(define-read-only (get-property-counter)
  (ok (var-get property-counter))
)

(define-read-only (get-review-counter)
  (ok (var-get review-counter))
)

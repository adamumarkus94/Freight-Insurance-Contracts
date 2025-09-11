;; Shipment Tracking Integration Contract
;; Provides real-time tracking capabilities for freight insurance policies

;; Error constants
(define-constant err-unauthorized (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-status (err u202))
(define-constant err-tracking-already-exists (err u203))
(define-constant err-invalid-milestone (err u204))
(define-constant err-milestone-already-passed (err u205))
(define-constant err-policy-not-active (err u206))

;; Import FIC contract for policy validation
(define-constant FIC-CONTRACT .FIC)

;; Tracking status constants
(define-constant STATUS-CREATED "created")
(define-constant STATUS-PICKED-UP "picked-up")
(define-constant STATUS-IN-TRANSIT "in-transit")
(define-constant STATUS-CHECKPOINT "checkpoint")
(define-constant STATUS-OUT-FOR-DELIVERY "out-for-delivery")
(define-constant STATUS-DELIVERED "delivered")
(define-constant STATUS-EXCEPTION "exception")

;; Core tracking data for each shipment
(define-map shipment-tracking
  { policy-id: uint }
  {
    carrier: principal,
    current-status: (string-ascii 20),
    current-location: (string-ascii 100),
    estimated-delivery: uint,
    created-block: uint,
    last-update-block: uint,
    total-updates: uint,
    exception-count: uint
  }
)

;; Detailed tracking history with individual updates
(define-map tracking-updates
  { policy-id: uint, update-id: uint }
  {
    status: (string-ascii 20),
    location: (string-ascii 100),
    notes: (string-ascii 200),
    timestamp: uint,
    carrier: principal,
    milestone-reached: bool
  }
)

;; Milestone definitions for automated policy updates
(define-map policy-milestones
  { policy-id: uint }
  {
    pickup-completed: bool,
    first-checkpoint: bool,
    halfway-point: bool,
    final-checkpoint: bool,
    delivery-attempted: bool,
    delivery-completed: bool
  }
)

;; Counter for tracking update IDs per policy
(define-map update-counters
  { policy-id: uint }
  { counter: uint }
)

;; Initialize shipment tracking for a policy
(define-public (initialize-tracking (policy-id uint) (estimated-delivery-blocks uint))
  (let (
    (policy-info (unwrap! (contract-call? FIC-CONTRACT get-policy policy-id) err-not-found))
  )
    ;; (asserts! (is-some policy-info) err-not-found)
    ;; (asserts! (is-eq tx-sender (get carrier (unwrap-panic policy-info))) err-unauthorized)
    ;; (asserts! (is-eq (get status (unwrap-panic policy-info)) "active") err-policy-not-active)
    ;; (asserts! (is-none (map-get? shipment-tracking { policy-id: policy-id })) err-tracking-already-exists)
    
    (map-set shipment-tracking
      { policy-id: policy-id }
      {
        carrier: tx-sender,
        current-status: STATUS-CREATED,
        current-location: "Origin",
        estimated-delivery: (+ stacks-block-height estimated-delivery-blocks),
        created-block: stacks-block-height,
        last-update-block: stacks-block-height,
        total-updates: u1,
        exception-count: u0
      }
    )
    
    (map-set policy-milestones
      { policy-id: policy-id }
      {
        pickup-completed: false,
        first-checkpoint: false,
        halfway-point: false,
        final-checkpoint: false,
        delivery-attempted: false,
        delivery-completed: false
      }
    )
    
    (map-set update-counters { policy-id: policy-id } { counter: u0 })
    ;; (try! (add-tracking-update policy-id STATUS-CREATED "Origin" "Tracking initialized" false))
    (ok true)
  )
)


;; Private function to add tracking update entry
(define-private (add-tracking-update (policy-id uint) (status (string-ascii 20)) 
                                    (location (string-ascii 100)) (notes (string-ascii 200)) 
                                    (is-milestone bool))
  (let (
    (counter-info (default-to { counter: u0 } (map-get? update-counters { policy-id: policy-id })))
    (new-counter (+ (get counter counter-info) u1))
  )
    (map-set update-counters { policy-id: policy-id } { counter: new-counter })
    (map-set tracking-updates
      { policy-id: policy-id, update-id: new-counter }
      {
        status: status,
        location: location,
        notes: notes,
        timestamp: stacks-block-height,
        carrier: tx-sender,
        milestone-reached: is-milestone
      }
    )
    (ok new-counter)
  )
)

;; Check if status represents a milestone
(define-private (is-milestone-status (status (string-ascii 20)))
  (or (is-eq status STATUS-PICKED-UP)
      (is-eq status STATUS-IN-TRANSIT)
      (is-eq status STATUS-CHECKPOINT)
      (is-eq status STATUS-OUT-FOR-DELIVERY)
      (is-eq status STATUS-DELIVERED))
)

;; Validate tracking status
(define-private (is-valid-status (status (string-ascii 20)))
  (or (is-eq status STATUS-CREATED)
      (is-eq status STATUS-PICKED-UP)
      (is-eq status STATUS-IN-TRANSIT)
      (is-eq status STATUS-CHECKPOINT)
      (is-eq status STATUS-OUT-FOR-DELIVERY)
      (is-eq status STATUS-DELIVERED)
      (is-eq status STATUS-EXCEPTION))
)

;; Update milestone tracking and trigger policy updates

;; Update estimated delivery time
(define-public (update-estimated-delivery (policy-id uint) (new-estimated-blocks uint))
  (let (
    (tracking (unwrap! (map-get? shipment-tracking { policy-id: policy-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get carrier tracking)) err-unauthorized)
    (map-set shipment-tracking
      { policy-id: policy-id }
      (merge tracking { estimated-delivery: (+ stacks-block-height new-estimated-blocks) })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get current tracking information
(define-read-only (get-tracking-info (policy-id uint))
  (map-get? shipment-tracking { policy-id: policy-id })
)

;; Get specific tracking update
(define-read-only (get-tracking-update (policy-id uint) (update-id uint))
  (map-get? tracking-updates { policy-id: policy-id, update-id: update-id })
)

;; Get all milestone information
(define-read-only (get-milestone-status (policy-id uint))
  (map-get? policy-milestones { policy-id: policy-id })
)

;; Get tracking update count
(define-read-only (get-update-count (policy-id uint))
  (default-to u0 (get counter (map-get? update-counters { policy-id: policy-id })))
)

;; Get current delivery status summary
(define-read-only (get-delivery-summary (policy-id uint))
  (match (map-get? shipment-tracking { policy-id: policy-id })
    tracking-info
    (let (
      (milestones (default-to 
        { pickup-completed: false, first-checkpoint: false, halfway-point: false, 
          final-checkpoint: false, delivery-attempted: false, delivery-completed: false }
        (map-get? policy-milestones { policy-id: policy-id })))
    )
      (ok {
        current-status: (get current-status tracking-info),
        current-location: (get current-location tracking-info),
        estimated-delivery: (get estimated-delivery tracking-info),
        total-updates: (get total-updates tracking-info),
        exception-count: (get exception-count tracking-info),
        pickup-completed: (get pickup-completed milestones),
        in-transit: (get first-checkpoint milestones),
        delivery-completed: (get delivery-completed milestones),
        progress-percentage: (calculate-progress-percentage milestones)
      })
    )
    err-not-found
  )
)

;; Calculate delivery progress as percentage
(define-private (calculate-progress-percentage (milestones { pickup-completed: bool, first-checkpoint: bool, 
                                               halfway-point: bool, final-checkpoint: bool, 
                                               delivery-attempted: bool, delivery-completed: bool }))
  (let (
    (completed-milestones (+ (if (get pickup-completed milestones) u1 u0)
                           (if (get first-checkpoint milestones) u1 u0)
                           (if (get halfway-point milestones) u1 u0)
                           (if (get final-checkpoint milestones) u1 u0)
                           (if (get delivery-attempted milestones) u1 u0)
                           (if (get delivery-completed milestones) u1 u0)))
  )
    (/ (* completed-milestones u100) u6)
  )
)

;; Check if delivery is overdue
(define-read-only (is-delivery-overdue (policy-id uint))
  (match (map-get? shipment-tracking { policy-id: policy-id })
    tracking-info
    (and (< (get estimated-delivery tracking-info) stacks-block-height)
         (not (is-eq (get current-status tracking-info) STATUS-DELIVERED)))
    false
  )
)

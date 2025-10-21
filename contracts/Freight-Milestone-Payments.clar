;; title: Freight-Milestone-Payments

(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u101))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u102))
(define-constant ERR_INVALID_MILESTONE (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_SHIPMENT_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_PARTICIPANT (err u106))
(define-constant ERR_MILESTONE_NOT_COMPLETED (err u107))
(define-constant ERR_DISPUTE_NOT_FOUND (err u108))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u109))
(define-constant ERR_DISPUTE_RESOLVED (err u110))
(define-constant ERR_NOT_ARBITRATOR (err u111))
(define-constant ERR_INVALID_DISPUTE_TYPE (err u112))
(define-constant ERR_NO_SHIPMENTS (err u113))
(define-constant ERR_INVALID_RATING (err u114))

(define-constant MILESTONE_PICKUP u1)
(define-constant MILESTONE_HALFWAY u2)
(define-constant MILESTONE_DELIVERY u3)

(define-constant STATUS_PENDING u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_CANCELLED u3)

(define-constant DISPUTE_MILESTONE u1)
(define-constant DISPUTE_PAYMENT u2)
(define-constant DISPUTE_CANCELLATION u3)

(define-constant DISPUTE_STATUS_OPEN u1)
(define-constant DISPUTE_STATUS_UNDER_REVIEW u2)
(define-constant DISPUTE_STATUS_RESOLVED u3)

(define-data-var next-shipment-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var contract-arbitrator principal tx-sender)

(define-map shipments uint {
    shipper: principal,
    trucker: principal,
    total-payment: uint,
    pickup-payment: uint,
    halfway-payment: uint,
    delivery-payment: uint,
    pickup-completed: bool,
    halfway-completed: bool,
    delivery-completed: bool,
    pickup-block: (optional uint),
    halfway-block: (optional uint),
    delivery-block: (optional uint),
    status: uint,
    created-block: uint
})

(define-map shipment-funds uint uint)

(define-map disputes uint {
    shipment-id: uint,
    dispute-type: uint,
    initiator: principal,
    respondent: principal,
    description: (string-ascii 500),
    milestone: (optional uint),
    status: uint,
    created-block: uint,
    resolved-block: (optional uint),
    resolution: (optional (string-ascii 500)),
    winner: (optional principal)
})

(define-map dispute-evidence uint {
    dispute-id: uint,
    submitter: principal,
    evidence: (string-ascii 1000),
    submitted-block: uint
})

(define-map participant-shipments principal {
    shipment-ids: (list 200 uint),
    total-count: uint
})

(define-map participant-stats principal {
    total-completed: uint,
    total-cancelled: uint,
    total-earned: uint,
    total-spent: uint,
    average-rating: uint,
    rating-count: uint
})

(define-map shipment-ratings uint {
    shipper-rating: (optional uint),
    trucker-rating: (optional uint),
    shipper-feedback: (optional (string-ascii 500)),
    trucker-feedback: (optional (string-ascii 500))
})

(define-private (is-valid-milestone (milestone uint))
    (or 
        (is-eq milestone MILESTONE_PICKUP)
        (or 
            (is-eq milestone MILESTONE_HALFWAY)
            (is-eq milestone MILESTONE_DELIVERY))))

(define-private (get-milestone-payment (shipment-id uint) (milestone uint))
    (match (map-get? shipments shipment-id)
        shipment
        (if (is-eq milestone MILESTONE_PICKUP)
            (ok (get pickup-payment shipment))
            (if (is-eq milestone MILESTONE_HALFWAY)
                (ok (get halfway-payment shipment))
                (if (is-eq milestone MILESTONE_DELIVERY)
                    (ok (get delivery-payment shipment))
                    ERR_INVALID_MILESTONE)))
        ERR_SHIPMENT_NOT_FOUND))

(define-private (is-milestone-completed (shipment-id uint) (milestone uint))
    (match (map-get? shipments shipment-id)
        shipment
        (if (is-eq milestone MILESTONE_PICKUP)
            (ok (get pickup-completed shipment))
            (if (is-eq milestone MILESTONE_HALFWAY)
                (ok (get halfway-completed shipment))
                (if (is-eq milestone MILESTONE_DELIVERY)
                    (ok (get delivery-completed shipment))
                    ERR_INVALID_MILESTONE)))
        ERR_SHIPMENT_NOT_FOUND))

(define-private (validate-payments (pickup uint) (halfway uint) (delivery uint) (total uint))
    (is-eq total (+ pickup (+ halfway delivery))))

(define-private (update-milestone-completion (shipment-id uint) (milestone uint))
    (match (map-get? shipments shipment-id)
        shipment
        (let ((current-block u1))
            (if (is-eq milestone MILESTONE_PICKUP)
                (map-set shipments shipment-id (merge shipment {
                    pickup-completed: true,
                    pickup-block: (some current-block)
                }))
                (if (is-eq milestone MILESTONE_HALFWAY)
                    (map-set shipments shipment-id (merge shipment {
                        halfway-completed: true,
                        halfway-block: (some current-block)
                    }))
                    (if (is-eq milestone MILESTONE_DELIVERY)
                        (map-set shipments shipment-id (merge shipment {
                            delivery-completed: true,
                            delivery-block: (some current-block),
                            status: STATUS_COMPLETED
                        }))
                        false))))
        false))

(define-public (create-shipment 
    (trucker principal)
    (pickup-payment uint)
    (halfway-payment uint)
    (delivery-payment uint))
    (let ((shipment-id (var-get next-shipment-id))
          (total-payment (+ pickup-payment (+ halfway-payment delivery-payment))))
        (asserts! (validate-payments pickup-payment halfway-payment delivery-payment total-payment) ERR_INSUFFICIENT_FUNDS)
        (try! (stx-transfer? total-payment tx-sender (as-contract tx-sender)))
        (map-set shipments shipment-id {
            shipper: tx-sender,
            trucker: trucker,
            total-payment: total-payment,
            pickup-payment: pickup-payment,
            halfway-payment: halfway-payment,
            delivery-payment: delivery-payment,
            pickup-completed: false,
            halfway-completed: false,
            delivery-completed: false,
            pickup-block: none,
            halfway-block: none,
            delivery-block: none,
            status: STATUS_ACTIVE,
            created-block: u1
        })
        (map-set shipment-funds shipment-id total-payment)
        (unwrap-panic (track-shipment-for-participant tx-sender shipment-id))
        (unwrap-panic (track-shipment-for-participant trucker shipment-id))
        (unwrap-panic (update-participant-stat tx-sender u0 u0 u0 total-payment))
        (map-set shipment-ratings shipment-id {
            shipper-rating: none,
            trucker-rating: none,
            shipper-feedback: none,
            trucker-feedback: none
        })
        (var-set next-shipment-id (+ shipment-id u1))
        (ok shipment-id)))

(define-public (complete-milestone (shipment-id uint) (milestone uint))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND)))
        (asserts! (is-valid-milestone milestone) ERR_INVALID_MILESTONE)
        (asserts! (is-eq (get status shipment) STATUS_ACTIVE) ERR_SHIPMENT_NOT_ACTIVE)
        (asserts! (or (is-eq tx-sender (get shipper shipment)) (is-eq tx-sender (get trucker shipment))) ERR_NOT_AUTHORIZED)
        (asserts! (not (unwrap-panic (is-milestone-completed shipment-id milestone))) ERR_MILESTONE_ALREADY_COMPLETED)
        (if (is-eq milestone MILESTONE_HALFWAY)
            (asserts! (unwrap-panic (is-milestone-completed shipment-id MILESTONE_PICKUP)) ERR_MILESTONE_NOT_COMPLETED)
            true)
        (if (is-eq milestone MILESTONE_DELIVERY)
            (asserts! (and 
                (unwrap-panic (is-milestone-completed shipment-id MILESTONE_PICKUP))
                (unwrap-panic (is-milestone-completed shipment-id MILESTONE_HALFWAY))) ERR_MILESTONE_NOT_COMPLETED)
            true)
        (asserts! (update-milestone-completion shipment-id milestone) ERR_INVALID_MILESTONE)
        (ok true)))

(define-public (claim-milestone-payment (shipment-id uint) (milestone uint))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
          (payment-amount (unwrap! (get-milestone-payment shipment-id milestone) ERR_INVALID_MILESTONE)))
        (asserts! (is-valid-milestone milestone) ERR_INVALID_MILESTONE)
        (asserts! (is-eq tx-sender (get trucker shipment)) ERR_NOT_AUTHORIZED)
        (asserts! (unwrap-panic (is-milestone-completed shipment-id milestone)) ERR_MILESTONE_NOT_COMPLETED)
        (try! (as-contract (stx-transfer? payment-amount tx-sender (get trucker shipment))))
        (unwrap-panic (update-participant-stat (get trucker shipment) u0 u0 payment-amount u0))
        (if (is-eq milestone MILESTONE_DELIVERY)
            (unwrap-panic (update-participant-stat (get trucker shipment) u1 u0 u0 u0))
            true)
        (ok payment-amount)))

(define-public (cancel-shipment (shipment-id uint))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
          (remaining-funds (unwrap! (map-get? shipment-funds shipment-id) ERR_SHIPMENT_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get shipper shipment)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status shipment) STATUS_ACTIVE) ERR_SHIPMENT_NOT_ACTIVE)
        (map-set shipments shipment-id (merge shipment { status: STATUS_CANCELLED }))
        (unwrap-panic (update-participant-stat (get shipper shipment) u0 u1 u0 u0))
        (unwrap-panic (update-participant-stat (get trucker shipment) u0 u1 u0 u0))
        (let ((pickup-refund (if (get pickup-completed shipment) u0 (get pickup-payment shipment)))
              (halfway-refund (if (get halfway-completed shipment) u0 (get halfway-payment shipment)))
              (delivery-refund (if (get delivery-completed shipment) u0 (get delivery-payment shipment)))
              (total-refund (+ pickup-refund (+ halfway-refund delivery-refund))))
            (if (> total-refund u0)
                (try! (as-contract (stx-transfer? total-refund tx-sender (get shipper shipment))))
                true))
        (ok true)))

(define-public (create-dispute 
    (shipment-id uint)
    (dispute-type uint)
    (description (string-ascii 500))
    (milestone (optional uint)))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
          (dispute-id (var-get next-dispute-id))
          (respondent (if (is-eq tx-sender (get shipper shipment)) 
                         (get trucker shipment) 
                         (get shipper shipment))))
        (asserts! (or (is-eq dispute-type DISPUTE_MILESTONE) 
                     (or (is-eq dispute-type DISPUTE_PAYMENT) 
                         (is-eq dispute-type DISPUTE_CANCELLATION))) ERR_INVALID_DISPUTE_TYPE)
        (asserts! (or (is-eq tx-sender (get shipper shipment)) 
                     (is-eq tx-sender (get trucker shipment))) ERR_NOT_AUTHORIZED)
        (asserts! (> (len description) u0) ERR_INVALID_DISPUTE_TYPE)
        
        (map-set disputes dispute-id {
            shipment-id: shipment-id,
            dispute-type: dispute-type,
            initiator: tx-sender,
            respondent: respondent,
            description: description,
            milestone: milestone,
            status: DISPUTE_STATUS_OPEN,
            created-block: stacks-block-height,
            resolved-block: none,
            resolution: none,
            winner: none
        })
        
        (var-set next-dispute-id (+ dispute-id u1))
        (ok dispute-id)))

(define-public (submit-evidence (dispute-id uint) (evidence (string-ascii 1000)))
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
        (asserts! (or (is-eq tx-sender (get initiator dispute)) 
                     (is-eq tx-sender (get respondent dispute))) ERR_NOT_AUTHORIZED)
        (asserts! (not (is-eq (get status dispute) DISPUTE_STATUS_RESOLVED)) ERR_DISPUTE_RESOLVED)
        (asserts! (> (len evidence) u0) ERR_INVALID_DISPUTE_TYPE)
        
        (map-set dispute-evidence dispute-id {
            dispute-id: dispute-id,
            submitter: tx-sender,
            evidence: evidence,
            submitted-block: stacks-block-height
        })
        
        (ok true)))

(define-public (arbitrate-dispute 
    (dispute-id uint)
    (winner principal)
    (resolution (string-ascii 500)))
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-arbitrator)) ERR_NOT_ARBITRATOR)
        (asserts! (not (is-eq (get status dispute) DISPUTE_STATUS_RESOLVED)) ERR_DISPUTE_RESOLVED)
        (asserts! (or (is-eq winner (get initiator dispute)) 
                     (is-eq winner (get respondent dispute))) ERR_INVALID_PARTICIPANT)
        
        (map-set disputes dispute-id (merge dispute {
            status: DISPUTE_STATUS_RESOLVED,
            resolved-block: (some stacks-block-height),
            resolution: (some resolution),
            winner: (some winner)
        }))
        
        (ok true)))

(define-public (escalate-dispute (dispute-id uint))
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
        (asserts! (or (is-eq tx-sender (get initiator dispute)) 
                     (is-eq tx-sender (get respondent dispute))) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status dispute) DISPUTE_STATUS_OPEN) ERR_DISPUTE_RESOLVED)
        
        (map-set disputes dispute-id (merge dispute {
            status: DISPUTE_STATUS_UNDER_REVIEW
        }))
        
        (ok true)))

(define-public (set-arbitrator (new-arbitrator principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-arbitrator)) ERR_NOT_ARBITRATOR)
        (var-set contract-arbitrator new-arbitrator)
        (ok true)))

(define-read-only (get-shipment (shipment-id uint))
    (map-get? shipments shipment-id))

(define-read-only (get-shipment-status (shipment-id uint))
    (match (map-get? shipments shipment-id)
        shipment (ok (get status shipment))
        ERR_SHIPMENT_NOT_FOUND))

(define-read-only (get-milestone-status (shipment-id uint) (milestone uint))
    (if (is-valid-milestone milestone)
        (is-milestone-completed shipment-id milestone)
        ERR_INVALID_MILESTONE))

(define-read-only (get-shipment-progress (shipment-id uint))
    (match (map-get? shipments shipment-id)
        shipment
        (ok {
            pickup-completed: (get pickup-completed shipment),
            halfway-completed: (get halfway-completed shipment),
            delivery-completed: (get delivery-completed shipment),
            pickup-block: (get pickup-block shipment),
            halfway-block: (get halfway-block shipment),
            delivery-block: (get delivery-block shipment),
            status: (get status shipment)
        })
        ERR_SHIPMENT_NOT_FOUND))

(define-read-only (get-payment-breakdown (shipment-id uint))
    (match (map-get? shipments shipment-id)
        shipment
        (ok {
            pickup-payment: (get pickup-payment shipment),
            halfway-payment: (get halfway-payment shipment),
            delivery-payment: (get delivery-payment shipment),
            total-payment: (get total-payment shipment)
        })
        ERR_SHIPMENT_NOT_FOUND))

(define-read-only (get-shipment-participants (shipment-id uint))
    (match (map-get? shipments shipment-id)
        shipment
        (ok {
            shipper: (get shipper shipment),
            trucker: (get trucker shipment)
        })
        ERR_SHIPMENT_NOT_FOUND))

(define-read-only (is-participant (shipment-id uint) (user principal))
    (match (map-get? shipments shipment-id)
        shipment
        (ok (or (is-eq user (get shipper shipment)) (is-eq user (get trucker shipment))))
        ERR_SHIPMENT_NOT_FOUND))

(define-read-only (get-next-shipment-id)
    (var-get next-shipment-id))

(define-read-only (get-dispute (dispute-id uint))
    (map-get? disputes dispute-id))

(define-read-only (get-dispute-evidence (evidence-id uint))
    (map-get? dispute-evidence evidence-id))

(define-read-only (get-contract-arbitrator)
    (var-get contract-arbitrator))

(define-private (track-shipment-for-participant (participant principal) (shipment-id uint))
    (let ((current-data (default-to 
                          { shipment-ids: (list), total-count: u0 } 
                          (map-get? participant-shipments participant)))
          (current-ids (get shipment-ids current-data))
          (current-count (get total-count current-data)))
        (map-set participant-shipments participant {
            shipment-ids: (unwrap-panic (as-max-len? (append current-ids shipment-id) u200)),
            total-count: (+ current-count u1)
        })
        (ok true)))

(define-private (update-participant-stat 
    (participant principal) 
    (completed-delta uint)
    (cancelled-delta uint)
    (earned-delta uint)
    (spent-delta uint))
    (let ((current-stats (default-to 
                           { total-completed: u0, total-cancelled: u0, total-earned: u0, 
                             total-spent: u0, average-rating: u0, rating-count: u0 } 
                           (map-get? participant-stats participant))))
        (map-set participant-stats participant {
            total-completed: (+ (get total-completed current-stats) completed-delta),
            total-cancelled: (+ (get total-cancelled current-stats) cancelled-delta),
            total-earned: (+ (get total-earned current-stats) earned-delta),
            total-spent: (+ (get total-spent current-stats) spent-delta),
            average-rating: (get average-rating current-stats),
            rating-count: (get rating-count current-stats)
        })
        (ok true)))

(define-public (rate-participant (shipment-id uint) (rating uint) (feedback (string-ascii 500)))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
          (ratings (unwrap! (map-get? shipment-ratings shipment-id) ERR_SHIPMENT_NOT_FOUND)))
        (asserts! (is-eq (get status shipment) STATUS_COMPLETED) ERR_SHIPMENT_NOT_ACTIVE)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
        (asserts! (or (is-eq tx-sender (get shipper shipment)) 
                     (is-eq tx-sender (get trucker shipment))) ERR_NOT_AUTHORIZED)
        
        (if (is-eq tx-sender (get shipper shipment))
            (begin
                (asserts! (is-none (get trucker-rating ratings)) ERR_MILESTONE_ALREADY_COMPLETED)
                (map-set shipment-ratings shipment-id (merge ratings {
                    trucker-rating: (some rating),
                    trucker-feedback: (some feedback)
                }))
                (unwrap-panic (update-rating-stats (get trucker shipment) rating))
                (ok true))
            (begin
                (asserts! (is-none (get shipper-rating ratings)) ERR_MILESTONE_ALREADY_COMPLETED)
                (map-set shipment-ratings shipment-id (merge ratings {
                    shipper-rating: (some rating),
                    shipper-feedback: (some feedback)
                }))
                (unwrap-panic (update-rating-stats (get shipper shipment) rating))
                (ok true)))))

(define-private (update-rating-stats (participant principal) (new-rating uint))
    (let ((current-stats (default-to 
                           { total-completed: u0, total-cancelled: u0, total-earned: u0, 
                             total-spent: u0, average-rating: u0, rating-count: u0 } 
                           (map-get? participant-stats participant)))
          (current-avg (get average-rating current-stats))
          (current-count (get rating-count current-stats))
          (new-count (+ current-count u1))
          (new-avg (/ (+ (* current-avg current-count) new-rating) new-count)))
        (map-set participant-stats participant (merge current-stats {
            average-rating: new-avg,
            rating-count: new-count
        }))
        (ok true)))

(define-read-only (get-participant-shipments (participant principal))
    (map-get? participant-shipments participant))

(define-read-only (get-participant-stats (participant principal))
    (map-get? participant-stats participant))

(define-read-only (get-shipment-ratings (shipment-id uint))
    (map-get? shipment-ratings shipment-id))

(define-read-only (get-participant-performance (participant principal))
    (match (map-get? participant-stats participant)
        stats
        (let ((total-shipments (+ (get total-completed stats) (get total-cancelled stats))))
            (ok {
                total-shipments: total-shipments,
                completion-rate: (if (> total-shipments u0) 
                                    (/ (* (get total-completed stats) u100) total-shipments) 
                                    u0),
                total-completed: (get total-completed stats),
                total-cancelled: (get total-cancelled stats),
                total-earned: (get total-earned stats),
                total-spent: (get total-spent stats),
                average-rating: (get average-rating stats),
                rating-count: (get rating-count stats)
            }))
        ERR_NO_SHIPMENTS))

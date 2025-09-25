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
        (ok payment-amount)))

(define-public (cancel-shipment (shipment-id uint))
    (let ((shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
          (remaining-funds (unwrap! (map-get? shipment-funds shipment-id) ERR_SHIPMENT_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get shipper shipment)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status shipment) STATUS_ACTIVE) ERR_SHIPMENT_NOT_ACTIVE)
        (map-set shipments shipment-id (merge shipment { status: STATUS_CANCELLED }))
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

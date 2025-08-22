;; title: Freight-Milestone-Payments

(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u101))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u102))
(define-constant ERR_INVALID_MILESTONE (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_SHIPMENT_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_PARTICIPANT (err u106))
(define-constant ERR_MILESTONE_NOT_COMPLETED (err u107))

(define-constant MILESTONE_PICKUP u1)
(define-constant MILESTONE_HALFWAY u2)
(define-constant MILESTONE_DELIVERY u3)

(define-constant STATUS_PENDING u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_CANCELLED u3)

(define-data-var next-shipment-id uint u1)

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

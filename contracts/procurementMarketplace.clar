;; ProcurementMarketplace Contract
;; Streamline business procurement with supplier matching and competitive bidding
;; A decentralized marketplace for transparent procurement processes

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-invalid-deadline (err u102))
(define-constant err-bid-too-low (err u103))
(define-constant err-bid-deadline-passed (err u104))
(define-constant err-procurement-not-found (err u105))
(define-constant err-unauthorized (err u106))

;; Data structures
(define-map procurements 
  uint 
  {
    buyer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    deadline: uint,
    status: (string-ascii 20),
    winning-bid: uint,
    winning-supplier: (optional principal)
  })

(define-map bids 
  {procurement-id: uint, supplier: principal}
  {
    amount: uint,
    proposal: (string-ascii 300),
    timestamp: uint
  })

;; Tracking variables
(define-data-var procurement-counter uint u0)
(define-data-var total-procurements uint u0)

;; Function 1: Create Procurement Request
;; Allows businesses to post procurement requirements with budget and deadline           
(define-public (create-procurement (title (string-ascii 100)) (description (string-ascii 500)) (budget uint) (deadline uint))
  ;; body yaha se start hoga
  (let
    (
      (procurement-id (+ (var-get procurement-counter) u1))
      (current-block stacks-block-height)
    )
    (begin
    (asserts! (> budget u0) err-invalid-amount)
      (asserts! (> deadline current-block) err-invalid-deadline)
      
      ;; Create procurement entry
      (map-set procurements procurement-id
        {
          buyer: tx-sender,
          title: title,
          description: description,
          budget: budget,
          deadline: deadline,
          status: "open",
          winning-bid: u0,
          winning-supplier: none
        })
      
      ;; Update counters
      (var-set procurement-counter procurement-id)
      (var-set total-procurements (+ (var-get total-procurements) u1))
      
      ;; Transfer budget to contract as escrow
      (try! (stx-transfer? budget tx-sender (as-contract tx-sender)))
      
      (print {event: "procurement-created", id: procurement-id, buyer: tx-sender, budget: budget})
      (ok procurement-id)))
)  
    


;; Function 2: Submit Bid
;; Allows suppliers to submit competitive bids for procurement requests
(define-public (submit-bid (procurement-id uint) (bid-amount uint) (proposal (string-ascii 300)))
  (let
    (
      (procurement-data (unwrap! (map-get? procurements procurement-id) err-procurement-not-found))
      (current-block stacks-block-height)
    )
    (begin
      (asserts! (> bid-amount u0) err-invalid-amount)
      (asserts! (<= current-block (get deadline procurement-data)) err-bid-deadline-passed)
      (asserts! (<= bid-amount (get budget procurement-data)) err-bid-too-low)
      
      ;; Store the bid
      (map-set bids {procurement-id: procurement-id, supplier: tx-sender}
        {
          amount: bid-amount,
          proposal: proposal,
          timestamp: current-block
        })
      
      ;; Update procurement with lowest bid if this is the lowest
      (let
        (
          (current-winning-bid (get winning-bid procurement-data))
        )
        (if (or (is-eq current-winning-bid u0) (< bid-amount current-winning-bid))
          (map-set procurements procurement-id
            (merge procurement-data
              {
                winning-bid: bid-amount,
                winning-supplier: (some tx-sender)
              }))
          true))
      
      (print {event: "bid-submitted", procurement-id: procurement-id, supplier: tx-sender, amount: bid-amount})
      (ok true)))
)
;; Read-only functions for data retrieval
(define-read-only (get-procurement (procurement-id uint))
  (map-get? procurements procurement-id))

(define-read-only (get-bid (procurement-id uint) (supplier principal))
  (map-get? bids {procurement-id: procurement-id, supplier: supplier}))

(define-read-only (get-total-procurements)
  (var-get total-procurements))

(define-read-only (get-procurement-counter)
  (var-get procurement-counter))






 
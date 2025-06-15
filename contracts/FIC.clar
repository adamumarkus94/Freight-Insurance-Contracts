(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-policy-expired (err u104))
(define-constant err-policy-not-active (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-claim-already-processed (err u107))
(define-constant err-claim-not-approved (err u108))
(define-constant err-invalid-amount (err u109))

(define-data-var premium-rate uint u5) ;; 5% premium rate by default
(define-data-var claim-processing-time uint u144) ;; 24 hours in blocks (assuming 10 min blocks)
(define-data-var total-premiums uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var contract-balance uint u0)


(define-constant err-insufficient-signatures (err u113))
(define-constant err-already-signed (err u114))
(define-constant err-not-authorized-signer (err u115))
(define-constant err-multisig-not-required (err u116))

(define-data-var multisig-threshold uint u10000)
(define-data-var multisig-enabled bool false)

(define-map authorized-signers
  { signer: principal }
  { authorized: bool, added-block: uint }
)

(define-map multisig-policies
  { policy-id: uint }
  {
    required-signatures: uint,
    current-signatures: uint,
    signers: (list 10 principal),
    executed: bool
  }
)

(define-map multisig-claims
  { claim-id: uint }
  {
    required-signatures: uint,
    current-signatures: uint,
    signers: (list 10 principal),
    executed: bool
  }
)

(define-map policy-signatures
  { policy-id: uint, signer: principal }
  { signed: bool, block-signed: uint }
)

(define-map claim-signatures
  { claim-id: uint, signer: principal }
  { signed: bool, block-signed: uint }
)

(define-map policies
  { policy-id: uint }
  {
    shipper: principal,
    carrier: principal,
    receiver: principal,
    value: uint,
    premium: uint,
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20)
  }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimer: principal,
    amount: uint,
    block-filed: uint,
    status: (string-ascii 20),
    evidence: (string-ascii 256)
  }
)

(define-map policy-claims
  { policy-id: uint }
  { claim-ids: (list 20 uint) }
)

(define-map policy-counter
  { counter-id: (string-ascii 10) }
  { counter: uint }
)

(define-map claim-counter
  { counter-id: (string-ascii 10) }
  { counter: uint }
)

(define-private (get-policy-count)
  (default-to u0 (get counter (map-get? policy-counter { counter-id: "policies" })))
)

(define-private (get-claim-count)
  (default-to u0 (get counter (map-get? claim-counter { counter-id: "claims" })))
)

(define-private (increment-policy-count)
  (let ((current-count (get-policy-count)))
    (map-set policy-counter { counter-id: "policies" } { counter: (+ current-count u1) })
    (+ current-count u1)
  )
)

(define-private (increment-claim-count)
  (let ((current-count (get-claim-count)))
    (map-set claim-counter { counter-id: "claims" } { counter: (+ current-count u1) })
    (+ current-count u1)
  )
)

(define-public (initialize-counters)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set policy-counter { counter-id: "policies" } { counter: u0 })
    (map-set claim-counter { counter-id: "claims" } { counter: u0 })
    (ok true)
  )
)

(define-public (set-premium-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set premium-rate new-rate)
    (ok new-rate)
  )
)

(define-public (set-claim-processing-time (new-time uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set claim-processing-time new-time)
    (ok new-time)
  )
)

(define-public (create-policy (carrier principal) (receiver principal) (value uint) (duration uint))
  (let 
    (
      (policy-id (increment-policy-count))
      (premium-amount (/ (* value (var-get premium-rate)) u100))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (> value u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? premium-amount tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (map-set policies 
      { policy-id: policy-id }
      {
        shipper: tx-sender,
        carrier: carrier,
        receiver: receiver,
        value: value,
        premium: premium-amount,
        start-block: start-block,
        end-block: end-block,
        status: "active"
      }
    )
    
    (map-set policy-claims { policy-id: policy-id } { claim-ids: (list) })
    
    (var-set total-premiums (+ (var-get total-premiums) premium-amount))
    (var-set contract-balance (+ (var-get contract-balance) premium-amount))
    
    (ok policy-id)
  )
)

(define-public (file-claim (policy-id uint) (amount uint) (evidence (string-ascii 256)))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (claim-id (increment-claim-count))
    )
    (asserts! (or (is-eq tx-sender (get shipper policy)) 
                 (is-eq tx-sender (get receiver policy))) 
             err-unauthorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (<= amount (get value policy)) err-invalid-amount)
    
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimer: tx-sender,
        amount: amount,
        block-filed: stacks-block-height,
        status: "pending",
        evidence: evidence
      }
    )
    
    (let ((current-claims (default-to { claim-ids: (list) } (map-get? policy-claims { policy-id: policy-id }))))
      (map-set policy-claims 
        { policy-id: policy-id } 
        { claim-ids: (unwrap! (as-max-len? (append (get claim-ids current-claims) claim-id) u20) err-unauthorized) }
      )
    )
    
    (ok claim-id)
  )
)

(define-public (approve-claim (claim-id uint))
  (let 
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
      (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    (asserts! (<= (get amount claim) (var-get contract-balance)) err-insufficient-funds)
    
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "approved" })
    )
    
    (map-set policies
      { policy-id: (get policy-id claim) }
      (merge policy { status: "claimed" })
    )
    
    (var-set total-claims-paid (+ (var-get total-claims-paid) (get amount claim)))
    (var-set contract-balance (- (var-get contract-balance) (get amount claim)))
    
    (as-contract (stx-transfer? (get amount claim) tx-sender (get claimer claim)))
  )
)

(define-public (reject-claim (claim-id uint) (reason (string-ascii 256)))
  (let 
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "rejected", evidence: reason })
    )
    
    (ok true)
  )
)

(define-public (confirm-delivery (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get receiver policy)) err-unauthorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { status: "delivered" })
    )
    
    (ok true)
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

(define-read-only (get-policy-claims-list (policy-id uint))
  (default-to { claim-ids: (list) } (map-get? policy-claims { policy-id: policy-id }))
)

(define-read-only (get-contract-stats)
  {
    premium-rate: (var-get premium-rate),
    total-premiums: (var-get total-premiums),
    total-claims-paid: (var-get total-claims-paid),
    contract-balance: (var-get contract-balance)
  }
)


(define-constant err-no-dispute-exists (err u110))
(define-constant err-dispute-exists (err u111))

(define-map claim-disputes
  { claim-id: uint }
  {
    carrier-response: (string-ascii 256),
    block-filed: uint,
    resolved: bool
  }
)


(define-public (enable-multisig (threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> threshold u0) err-invalid-amount)
    (var-set multisig-threshold threshold)
    (var-set multisig-enabled true)
    (ok true)
  )
)

(define-public (disable-multisig)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set multisig-enabled false)
    (ok true)
  )
)

(define-public (add-authorized-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-signers
      { signer: signer }
      { authorized: true, added-block: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (remove-authorized-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-signers
      { signer: signer }
      { authorized: false, added-block: stacks-block-height }
    )
    (ok true)
  )
)

(define-private (is-authorized-signer (signer principal))
  (default-to false 
    (get authorized (map-get? authorized-signers { signer: signer }))
  )
)

(define-private (requires-multisig (value uint))
  (and (var-get multisig-enabled) (>= value (var-get multisig-threshold)))
)

(define-public (create-multisig-policy (carrier principal) (receiver principal) (value uint) (duration uint) (required-sigs uint))
  (let 
    (
      (policy-id (increment-policy-count))
      (premium-amount (/ (* value (var-get premium-rate)) u100))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (requires-multisig value) err-multisig-not-required)
    (asserts! (> value u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (> required-sigs u0) err-invalid-amount)
    (asserts! (<= required-sigs u10) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? premium-amount tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (map-set policies 
      { policy-id: policy-id }
      {
        shipper: tx-sender,
        carrier: carrier,
        receiver: receiver,
        value: value,
        premium: premium-amount,
        start-block: start-block,
        end-block: end-block,
        status: "pending-multisig"
      }
    )
    
    (map-set multisig-policies
      { policy-id: policy-id }
      {
        required-signatures: required-sigs,
        current-signatures: u0,
        signers: (list),
        executed: false
      }
    )
    
    (map-set policy-claims { policy-id: policy-id } { claim-ids: (list) })
    
    (var-set total-premiums (+ (var-get total-premiums) premium-amount))
    (var-set contract-balance (+ (var-get contract-balance) premium-amount))
    
    (ok policy-id)
  )
)

(define-public (sign-policy (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (multisig-info (unwrap! (map-get? multisig-policies { policy-id: policy-id }) err-not-found))
      (existing-signature (map-get? policy-signatures { policy-id: policy-id, signer: tx-sender }))
    )
    (asserts! (is-authorized-signer tx-sender) err-not-authorized-signer)
    (asserts! (is-eq (get status policy) "pending-multisig") err-policy-not-active)
    (asserts! (is-none existing-signature) err-already-signed)
    (asserts! (is-eq (get executed multisig-info) false) err-claim-already-processed)
    
    (map-set policy-signatures
      { policy-id: policy-id, signer: tx-sender }
      { signed: true, block-signed: stacks-block-height }
    )
    
    (let ((new-sig-count (+ (get current-signatures multisig-info) u1)))
      (map-set multisig-policies
        { policy-id: policy-id }
        (merge multisig-info 
          { 
            current-signatures: new-sig-count,
            signers: (unwrap! (as-max-len? (append (get signers multisig-info) tx-sender) u10) err-unauthorized)
          }
        )
      )
      
      (if (>= new-sig-count (get required-signatures multisig-info))
        (begin
          (map-set policies
            { policy-id: policy-id }
            (merge policy { status: "active" })
          )
          (map-set multisig-policies
            { policy-id: policy-id }
            (merge multisig-info { executed: true })
          )
          (ok "policy-activated")
        )
        (ok "signature-recorded")
      )
    )
  )
)

(define-public (create-multisig-claim-approval (claim-id uint) (required-sigs uint))
  (let 
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
      (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (requires-multisig (get amount claim)) err-multisig-not-required)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    (asserts! (> required-sigs u0) err-invalid-amount)
    (asserts! (<= required-sigs u10) err-invalid-amount)
    
    (map-set multisig-claims
      { claim-id: claim-id }
      {
        required-signatures: required-sigs,
        current-signatures: u0,
        signers: (list),
        executed: false
      }
    )
    
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "pending-multisig" })
    )
    
    (ok true)
  )
)

(define-public (sign-claim-approval (claim-id uint))
  (let 
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
      (multisig-info (unwrap! (map-get? multisig-claims { claim-id: claim-id }) err-not-found))
      (existing-signature (map-get? claim-signatures { claim-id: claim-id, signer: tx-sender }))
    )
    (asserts! (is-authorized-signer tx-sender) err-not-authorized-signer)
    (asserts! (is-eq (get status claim) "pending-multisig") err-claim-already-processed)
    (asserts! (is-none existing-signature) err-already-signed)
    (asserts! (is-eq (get executed multisig-info) false) err-claim-already-processed)
    
    (map-set claim-signatures
      { claim-id: claim-id, signer: tx-sender }
      { signed: true, block-signed: stacks-block-height }
    )
    
    (let ((new-sig-count (+ (get current-signatures multisig-info) u1)))
      (map-set multisig-claims
        { claim-id: claim-id }
        (merge multisig-info 
          { 
            current-signatures: new-sig-count,
            signers: (unwrap! (as-max-len? (append (get signers multisig-info) tx-sender) u10) err-unauthorized)
          }
        )
      )
      
      (if (>= new-sig-count (get required-signatures multisig-info))
        (let ((policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found)))
          (map-set claims
            { claim-id: claim-id }
            (merge claim { status: "approved" })
          )
          
          (map-set policies
            { policy-id: (get policy-id claim) }
            (merge policy { status: "claimed" })
          )
          
          (map-set multisig-claims
            { claim-id: claim-id }
            (merge multisig-info { executed: true })
          )
          
          (var-set total-claims-paid (+ (var-get total-claims-paid) (get amount claim)))
          (var-set contract-balance (- (var-get contract-balance) (get amount claim)))
          
          (try! (as-contract (stx-transfer? (get amount claim) tx-sender (get claimer claim))))
          (ok true)
        )
        (ok true)
      )
    )
  )
)
(define-read-only (get-multisig-policy-info (policy-id uint))
  (map-get? multisig-policies { policy-id: policy-id })
)

(define-read-only (get-multisig-claim-info (claim-id uint))
  (map-get? multisig-claims { claim-id: claim-id })
)

(define-read-only (get-policy-signature (policy-id uint) (signer principal))
  (map-get? policy-signatures { policy-id: policy-id, signer: signer })
)

(define-read-only (get-claim-signature (claim-id uint) (signer principal))
  (map-get? claim-signatures { claim-id: claim-id, signer: signer })
)

(define-read-only (is-signer-authorized (signer principal))
  (is-authorized-signer signer)
)

(define-read-only (get-multisig-settings)
  {
    enabled: (var-get multisig-enabled),
    threshold: (var-get multisig-threshold)
  }
)


(define-public (file-dispute (claim-id uint) (response (string-ascii 256)))
  (let 
    (
      (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
      (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get carrier policy)) err-unauthorized)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    (asserts! (is-none (map-get? claim-disputes { claim-id: claim-id })) err-dispute-exists)
    
    (map-set claim-disputes
      { claim-id: claim-id }
      {
        carrier-response: response,
        block-filed: stacks-block-height,
        resolved: false
      }
    )
    (ok true)
  )
)

(define-read-only (get-dispute (claim-id uint))
  (map-get? claim-disputes { claim-id: claim-id })
)


(define-public (resolve-dispute (claim-id uint))
  (let 
    (
      (dispute (unwrap! (map-get? claim-disputes { claim-id: claim-id }) err-no-dispute-exists))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get resolved dispute) false) err-claim-already-processed)
    
    (map-set claim-disputes
      { claim-id: claim-id }
      (merge dispute { resolved: true })
    )
    
    (ok true)
  )
)
(define-public (get-dispute-status (claim-id uint))
  (let 
    (
      (dispute (unwrap! (map-get? claim-disputes { claim-id: claim-id }) err-no-dispute-exists))
    )
    (ok (get resolved dispute))
  )
)
(define-public (get-dispute-response (claim-id uint))
  (let 
    (
      (dispute (unwrap! (map-get? claim-disputes { claim-id: claim-id }) err-no-dispute-exists))
    )
    (ok (get carrier-response dispute))
  )
)
(define-public (get-dispute-filing-block (claim-id uint))
  (let 
    (
      (dispute (unwrap! (map-get? claim-disputes { claim-id: claim-id }) err-no-dispute-exists))
    )
    (ok (get block-filed dispute))
  )

)

(define-constant err-refund-not-available (err u112))
(define-constant refund-rate  u50) ;; 50% refund rate

(define-public (claim-premium-refund (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (policy-claimed (get claim-ids (get-policy-claims-list policy-id)))
      (refund-amount (/ (* (get premium policy) refund-rate) u100))
    )
    (asserts! (is-eq tx-sender (get shipper policy)) err-unauthorized)
    (asserts! (is-eq (get status policy) "delivered") err-policy-not-active)
    (asserts! (is-eq (len policy-claimed) u0) err-refund-not-available)
    (asserts! (<= refund-amount (var-get contract-balance)) err-insufficient-funds)
    
    (var-set contract-balance (- (var-get contract-balance) refund-amount))
    
    (as-contract (stx-transfer? refund-amount tx-sender (get shipper policy)))
  )
)




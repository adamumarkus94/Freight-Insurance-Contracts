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

(define-constant err-invalid-tier (err u117))
(define-constant err-discount-code-not-found (err u118))
(define-constant err-discount-code-expired (err u119))
(define-constant err-discount-code-already-used (err u120))
(define-constant err-discount-code-exists (err u121))
(define-constant err-insufficient-loyalty-points (err u122))

(define-map user-loyalty-profiles
  { user: principal }
  {
    total-policies: uint,
    successful-deliveries: uint,
    claim-free-policies: uint,
    total-value-insured: uint,
    loyalty-points: uint,
    tier-level: uint,
    last-updated: uint
  }
)

(define-map tier-benefits
  { tier: uint }
  {
    discount-percentage: uint,
    min-policies: uint,
    min-value: uint,
    bonus-points-multiplier: uint,
    max-discount-codes: uint
  }
)

(define-map discount-codes
  { code: (string-ascii 20) }
  {
    creator: principal,
    discount-percentage: uint,
    expiry-block: uint,
    max-uses: uint,
    current-uses: uint,
    min-value: uint,
    active: bool
  }
)

(define-map user-discount-usage
  { user: principal, code: (string-ascii 20) }
  { used: bool, used-block: uint }
)

(define-map policy-loyalty-tracking
  { policy-id: uint }
  {
    user: principal,
    loyalty-points-earned: uint,
    tier-at-creation: uint,
    discount-applied: uint
  }
)

(define-data-var loyalty-points-per-policy uint u10)
(define-data-var bonus-points-successful-delivery uint u25)
(define-data-var bonus-points-claim-free uint u15)
(define-data-var volume-bonus-threshold uint u100000)
(define-data-var volume-bonus-points uint u50)

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

(define-public (initialize-tier-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tier-benefits { tier: u0 } { discount-percentage: u0, min-policies: u0, min-value: u0, bonus-points-multiplier: u1, max-discount-codes: u0 })
    (map-set tier-benefits { tier: u1 } { discount-percentage: u5, min-policies: u5, min-value: u50000, bonus-points-multiplier: u1, max-discount-codes: u1 })
    (map-set tier-benefits { tier: u2 } { discount-percentage: u10, min-policies: u15, min-value: u150000, bonus-points-multiplier: u2, max-discount-codes: u2 })
    (map-set tier-benefits { tier: u3 } { discount-percentage: u15, min-policies: u30, min-value: u300000, bonus-points-multiplier: u3, max-discount-codes: u3 })
    (map-set tier-benefits { tier: u4 } { discount-percentage: u20, min-policies: u50, min-value: u500000, bonus-points-multiplier: u4, max-discount-codes: u5 })
    (ok true)
  )
)

(define-private (get-or-create-loyalty-profile (user principal))
  (default-to
    {
      total-policies: u0,
      successful-deliveries: u0,
      claim-free-policies: u0,
      total-value-insured: u0,
      loyalty-points: u0,
      tier-level: u0,
      last-updated: stacks-block-height
    }
    (map-get? user-loyalty-profiles { user: user })
  )
)

(define-private (calculate-tier-level (profile { total-policies: uint, successful-deliveries: uint, claim-free-policies: uint, total-value-insured: uint, loyalty-points: uint, tier-level: uint, last-updated: uint }))
  (if (and (>= (get total-policies profile) u50) (>= (get total-value-insured profile) u500000))
    u4
    (if (and (>= (get total-policies profile) u30) (>= (get total-value-insured profile) u300000))
      u3
      (if (and (>= (get total-policies profile) u15) (>= (get total-value-insured profile) u150000))
        u2
        (if (and (>= (get total-policies profile) u5) (>= (get total-value-insured profile) u50000))
          u1
          u0
        )
      )
    )
  )
)

(define-private (get-tier-discount (tier uint))
  (default-to u0 (get discount-percentage (map-get? tier-benefits { tier: tier })))
)

(define-private (calculate-loyalty-points (value uint) (tier uint))
  (let (
    (base-points (var-get loyalty-points-per-policy))
    (multiplier (default-to u1 (get bonus-points-multiplier (map-get? tier-benefits { tier: tier }))))
    (volume-bonus (if (>= value (var-get volume-bonus-threshold)) (var-get volume-bonus-points) u0))
  )
    (+ (* base-points multiplier) volume-bonus)
  )
)

(define-public (create-policy-with-loyalty (carrier principal) (receiver principal) (value uint) (duration uint) (discount-code (optional (string-ascii 20))))
  (let 
    (
      (policy-id (increment-policy-count))
      (user-profile (get-or-create-loyalty-profile tx-sender))
      (tier-level (calculate-tier-level user-profile))
      (tier-discount (get-tier-discount tier-level))
      (code-discount (if (is-some discount-code) (get-discount-from-code (unwrap-panic discount-code) value) u0))
      (total-discount (+ tier-discount code-discount))
      (premium-amount (/ (* value (var-get premium-rate)) u100))
      (discounted-premium (- premium-amount (/ (* premium-amount total-discount) u100)))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
      (loyalty-points-earned (calculate-loyalty-points value tier-level))
    )
    (asserts! (> value u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? discounted-premium tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (if (is-some discount-code) 
        (try! (use-discount-code (unwrap-panic discount-code))) 
        true)
    
    (map-set policies 
      { policy-id: policy-id }
      {
        shipper: tx-sender,
        carrier: carrier,
        receiver: receiver,
        value: value,
        premium: discounted-premium,
        start-block: start-block,
        end-block: end-block,
        status: "active"
      }
    )
    
    (map-set policy-claims { policy-id: policy-id } { claim-ids: (list) })
    
    (let (
      (updated-profile (merge user-profile {
        total-policies: (+ (get total-policies user-profile) u1),
        total-value-insured: (+ (get total-value-insured user-profile) value),
        loyalty-points: (+ (get loyalty-points user-profile) loyalty-points-earned),
        tier-level: tier-level,
        last-updated: stacks-block-height
      }))
    )
      (map-set user-loyalty-profiles { user: tx-sender } updated-profile)
    )
    
    (map-set policy-loyalty-tracking
      { policy-id: policy-id }
      {
        user: tx-sender,
        loyalty-points-earned: loyalty-points-earned,
        tier-at-creation: tier-level,
        discount-applied: total-discount
      }
    )
    
    (var-set total-premiums (+ (var-get total-premiums) discounted-premium))
    (var-set contract-balance (+ (var-get contract-balance) discounted-premium))
    
    (ok policy-id)
  )
)

(define-private (get-discount-from-code (code (string-ascii 20)) (value uint))
  (let ((discount-info (map-get? discount-codes { code: code })))
    (if (is-some discount-info)
      (let ((info (unwrap-panic discount-info)))
        (if (and (get active info) 
                 (> (get expiry-block info) stacks-block-height)
                 (< (get current-uses info) (get max-uses info))
                 (>= value (get min-value info))
                 (is-none (map-get? user-discount-usage { user: tx-sender, code: code })))
          (get discount-percentage info)
          u0
        )
      )
      u0
    )
  )
)

(define-private (use-discount-code (code (string-ascii 20)))
  (let ((discount-info (unwrap! (map-get? discount-codes { code: code }) err-discount-code-not-found)))
    (asserts! (get active discount-info) err-discount-code-not-found)
    (asserts! (> (get expiry-block discount-info) stacks-block-height) err-discount-code-expired)
    (asserts! (< (get current-uses discount-info) (get max-uses discount-info)) err-discount-code-already-used)
    (asserts! (is-none (map-get? user-discount-usage { user: tx-sender, code: code })) err-discount-code-already-used)
    
    (map-set discount-codes
      { code: code }
      (merge discount-info { current-uses: (+ (get current-uses discount-info) u1) })
    )
    
    (map-set user-discount-usage
      { user: tx-sender, code: code }
      { used: true, used-block: stacks-block-height }
    )
    
    (ok true)
  )
)

(define-public (update-policy-loyalty-on-delivery (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (tracking (unwrap! (map-get? policy-loyalty-tracking { policy-id: policy-id }) err-not-found))
      (user (get user tracking))
      (user-profile (get-or-create-loyalty-profile user))
      (policy-claims-list (get claim-ids (get-policy-claims-list policy-id)))
      (is-claim-free (is-eq (len policy-claims-list) u0))
      (delivery-points (var-get bonus-points-successful-delivery))
      (claim-free-points (if is-claim-free (var-get bonus-points-claim-free) u0))
      (total-bonus-points (+ delivery-points claim-free-points))
    )
    (asserts! (is-eq tx-sender (get receiver policy)) err-unauthorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    
    (let (
      (updated-profile (merge user-profile {
        successful-deliveries: (+ (get successful-deliveries user-profile) u1),
        claim-free-policies: (+ (get claim-free-policies user-profile) (if is-claim-free u1 u0)),
        loyalty-points: (+ (get loyalty-points user-profile) total-bonus-points),
        tier-level: (calculate-tier-level user-profile),
        last-updated: stacks-block-height
      }))
    )
      (map-set user-loyalty-profiles { user: user } updated-profile)
    )
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { status: "delivered" })
    )
    
    (ok total-bonus-points)
  )
)

(define-public (create-discount-code (code (string-ascii 20)) (discount-percentage uint) (expiry-blocks uint) (max-uses uint) (min-value uint))
  (let 
    (
      (user-profile (get-or-create-loyalty-profile tx-sender))
      (tier-level (get tier-level user-profile))
      (tier-info (unwrap! (map-get? tier-benefits { tier: tier-level }) err-invalid-tier))
      (max-allowed-codes (get max-discount-codes tier-info))
      (expiry-block (+ stacks-block-height expiry-blocks))
    )
    (asserts! (> max-allowed-codes u0) err-insufficient-loyalty-points)
    (asserts! (is-none (map-get? discount-codes { code: code })) err-discount-code-exists)
    (asserts! (> discount-percentage u0) err-invalid-amount)
    (asserts! (<= discount-percentage u25) err-invalid-amount)
    (asserts! (> max-uses u0) err-invalid-amount)
    (asserts! (> expiry-blocks u0) err-invalid-amount)
    
    (map-set discount-codes
      { code: code }
      {
        creator: tx-sender,
        discount-percentage: discount-percentage,
        expiry-block: expiry-block,
        max-uses: max-uses,
        current-uses: u0,
        min-value: min-value,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (deactivate-discount-code (code (string-ascii 20)))
  (let ((discount-info (unwrap! (map-get? discount-codes { code: code }) err-discount-code-not-found)))
    (asserts! (is-eq tx-sender (get creator discount-info)) err-unauthorized)
    
    (map-set discount-codes
      { code: code }
      (merge discount-info { active: false })
    )
    
    (ok true)
  )
)

(define-public (spend-loyalty-points (points uint))
  (let ((user-profile (get-or-create-loyalty-profile tx-sender)))
    (asserts! (>= (get loyalty-points user-profile) points) err-insufficient-loyalty-points)
    
    (map-set user-loyalty-profiles
      { user: tx-sender }
      (merge user-profile { loyalty-points: (- (get loyalty-points user-profile) points) })
    )
    
    (ok true)
  )
)

(define-read-only (get-loyalty-profile (user principal))
  (get-or-create-loyalty-profile user)
)

(define-read-only (get-tier-benefits-info (tier uint))
  (map-get? tier-benefits { tier: tier })
)

(define-read-only (get-discount-code-info (code (string-ascii 20)))
  (map-get? discount-codes { code: code })
)

(define-read-only (get-policy-loyalty-info (policy-id uint))
  (map-get? policy-loyalty-tracking { policy-id: policy-id })
)

(define-read-only (get-user-discount-usage-info (user principal) (code (string-ascii 20)))
  (map-get? user-discount-usage { user: user, code: code })
)

(define-read-only (calculate-potential-discount (user principal) (value uint) (discount-code (optional (string-ascii 20))))
  (let 
    (
      (user-profile (get-or-create-loyalty-profile user))
      (tier-level (calculate-tier-level user-profile))
      (tier-discount (get-tier-discount tier-level))
      (code-discount (if (is-some discount-code) (get-discount-from-code (unwrap-panic discount-code) value) u0))
      (total-discount (+ tier-discount code-discount))
      (premium-amount (/ (* value (var-get premium-rate)) u100))
      (discount-amount (/ (* premium-amount total-discount) u100))
    )
    {
      tier-discount: tier-discount,
      code-discount: code-discount,
      total-discount: total-discount,
      original-premium: premium-amount,
      discount-amount: discount-amount,
      final-premium: (- premium-amount discount-amount)
    }
  )
)

;; Route Risk Assessment & Dynamic Pricing System
(define-constant err-route-not-found (err u123))
(define-constant err-invalid-risk-factor (err u124))
(define-constant err-route-already-exists (err u125))
(define-constant err-invalid-coordinates (err u126))
(define-constant err-risk-factor-not-found (err u127))

;; Route definitions with geographic and risk data
(define-map shipping-routes
  { route-id: (string-ascii 20) }
  {
    origin-country: (string-ascii 30),
    destination-country: (string-ascii 30),
    origin-port: (string-ascii 50),
    destination-port: (string-ascii 50),
    distance-km: uint,
    base-risk-score: uint, ;; 1-100 scale
    seasonal-multiplier: uint, ;; percentage adjustment
    active: bool,
    created-block: uint
  }
)

;; Dynamic risk factors that affect route pricing
(define-map route-risk-factors
  { route-id: (string-ascii 20), factor-type: (string-ascii 20) }
  {
    risk-multiplier: uint, ;; percentage impact on base premium
    severity-level: uint, ;; 1-5 scale
    expiry-block: uint,
    last-updated: uint,
    active: bool
  }
)

;; Historical route performance tracking
(define-map route-performance
  { route-id: (string-ascii 20) }
  {
    total-policies: uint,
    successful-deliveries: uint,
    total-claims: uint,
    total-claims-value: uint,
    average-transit-time: uint,
    last-incident-block: uint,
    performance-score: uint ;; calculated metric 1-100
  }
)

;; Route-specific policy tracking
(define-map route-policies
  { policy-id: uint }
  {
    route-id: (string-ascii 20),
    route-risk-premium: uint,
    risk-factors-applied: (list 5 (string-ascii 20)),
    calculated-risk-score: uint
  }
)

;; Risk factor definitions
(define-map risk-factor-definitions
  { factor-type: (string-ascii 20) }
  {
    description: (string-ascii 100),
    max-multiplier: uint,
    category: (string-ascii 30),
    base-duration: uint
  }
)

;; Administrative settings for route system
(define-data-var route-system-enabled bool false)
(define-data-var max-route-risk-multiplier uint u200) ;; 200% max increase
(define-data-var base-route-assessment-fee uint u1000) ;; STX fee for route analysis

(define-public (initialize-route-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Initialize common risk factor definitions
    (map-set risk-factor-definitions { factor-type: "weather" } 
      { description: "Severe weather conditions affecting transport", max-multiplier: u50, category: "environmental", base-duration: u1440 })
    (map-set risk-factor-definitions { factor-type: "political" } 
      { description: "Political instability or trade restrictions", max-multiplier: u75, category: "geopolitical", base-duration: u4320 })
    (map-set risk-factor-definitions { factor-type: "piracy" } 
      { description: "Maritime piracy or theft risk", max-multiplier: u100, category: "security", base-duration: u2160 })
    (map-set risk-factor-definitions { factor-type: "congestion" } 
      { description: "Port or transport congestion delays", max-multiplier: u25, category: "logistics", base-duration: u720 })
    (map-set risk-factor-definitions { factor-type: "infrastructure" } 
      { description: "Infrastructure damage or limitations", max-multiplier: u60, category: "physical", base-duration: u2880 })
    
    (var-set route-system-enabled true)
    (ok true)
  )
)

(define-public (register-shipping-route (route-id (string-ascii 20)) (origin-country (string-ascii 30)) (destination-country (string-ascii 30)) 
                                        (origin-port (string-ascii 50)) (destination-port (string-ascii 50)) 
                                        (distance-km uint) (base-risk-score uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get route-system-enabled) err-policy-not-active)
    (asserts! (is-none (map-get? shipping-routes { route-id: route-id })) err-route-already-exists)
    (asserts! (and (> base-risk-score u0) (<= base-risk-score u100)) err-invalid-risk-factor)
    (asserts! (> distance-km u0) err-invalid-coordinates)
    
    (map-set shipping-routes
      { route-id: route-id }
      {
        origin-country: origin-country,
        destination-country: destination-country,
        origin-port: origin-port,
        destination-port: destination-port,
        distance-km: distance-km,
        base-risk-score: base-risk-score,
        seasonal-multiplier: u100, ;; neutral starting point
        active: true,
        created-block: stacks-block-height
      }
    )
    
    ;; Initialize performance tracking
    (map-set route-performance
      { route-id: route-id }
      {
        total-policies: u0,
        successful-deliveries: u0,
        total-claims: u0,
        total-claims-value: u0,
        average-transit-time: u0,
        last-incident-block: u0,
        performance-score: u75 ;; neutral starting score
      }
    )
    
    (ok true)
  )
)

(define-public (update-route-risk-factor (route-id (string-ascii 20)) (factor-type (string-ascii 20)) 
                                         (risk-multiplier uint) (severity-level uint) (duration-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? shipping-routes { route-id: route-id })) err-route-not-found)
    (asserts! (is-some (map-get? risk-factor-definitions { factor-type: factor-type })) err-risk-factor-not-found)
    (asserts! (and (> severity-level u0) (<= severity-level u5)) err-invalid-risk-factor)
    (asserts! (<= risk-multiplier (var-get max-route-risk-multiplier)) err-invalid-risk-factor)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    
    (map-set route-risk-factors
      { route-id: route-id, factor-type: factor-type }
      {
        risk-multiplier: risk-multiplier,
        severity-level: severity-level,
        expiry-block: (+ stacks-block-height duration-blocks),
        last-updated: stacks-block-height,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-private (calculate-route-risk-premium (route-id (string-ascii 20)) (base-premium uint))
  (match (map-get? shipping-routes { route-id: route-id })
    route-info
    (let (
      (base-risk (get base-risk-score route-info))
      (seasonal-adj (get seasonal-multiplier route-info))
      (performance-data (default-to 
        { total-policies: u0, successful-deliveries: u0, total-claims: u0, total-claims-value: u0, 
          average-transit-time: u0, last-incident-block: u0, performance-score: u75 }
        (map-get? route-performance { route-id: route-id })))
    )
      ;; Calculate base adjustment from route risk score
      (let (
        (base-adjustment (/ (* base-premium base-risk) u100))
        (seasonal-adjustment (/ (* base-adjustment seasonal-adj) u100))
        (performance-adjustment (/ (* seasonal-adjustment (get performance-score performance-data)) u100))
      )
        ;; Apply active risk factors
        (fold apply-risk-factor-to-premium 
          (list "weather" "political" "piracy" "congestion" "infrastructure")
          { route-id: route-id, premium: performance-adjustment }
        )
      )
    )
    { route-id: route-id, premium: base-premium }
  )
)

(define-private (apply-risk-factor-to-premium (factor-type (string-ascii 20)) 
                                             (state { route-id: (string-ascii 20), premium: uint }))
  (let (
    (risk-factor (map-get? route-risk-factors { route-id: (get route-id state), factor-type: factor-type }))
    (current-premium (get premium state))
  )
    (if (and (is-some risk-factor) 
             (get active (unwrap-panic risk-factor))
             (> (get expiry-block (unwrap-panic risk-factor)) stacks-block-height))
      ;; Apply risk factor multiplier
      (let ((multiplier (get risk-multiplier (unwrap-panic risk-factor))))
        { route-id: (get route-id state), 
          premium: (+ current-premium (/ (* current-premium multiplier) u100)) }
      )
      ;; No change if factor not active or expired
      state
    )
  )
)

(define-public (create-route-based-policy (carrier principal) (receiver principal) (value uint) 
                                          (duration uint) (route-id (string-ascii 20)))
  (let (
    (policy-id (increment-policy-count))
    (route-info (unwrap! (map-get? shipping-routes { route-id: route-id }) err-route-not-found))
    (base-premium (/ (* value (var-get premium-rate)) u100))
    (route-premium-calc (calculate-route-risk-premium route-id base-premium))
    (final-premium (get premium route-premium-calc))
    (assessment-fee (var-get base-route-assessment-fee))
    (total-cost (+ final-premium assessment-fee))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration))
    (active-factors (list))
  )
    (asserts! (get active route-info) err-route-not-found)
    (asserts! (> value u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? total-cost tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    ;; Create the policy
    (map-set policies 
      { policy-id: policy-id }
      {
        shipper: tx-sender,
        carrier: carrier,
        receiver: receiver,
        value: value,
        premium: final-premium,
        start-block: start-block,
        end-block: end-block,
        status: "active"
      }
    )
    
    ;; Track route-specific information
    (map-set route-policies
      { policy-id: policy-id }
      {
        route-id: route-id,
        route-risk-premium: (- final-premium base-premium),
        risk-factors-applied: active-factors,
        calculated-risk-score: (get base-risk-score route-info)
      }
    )
    
    (map-set policy-claims { policy-id: policy-id } { claim-ids: (list) })
    
    ;; Update route performance statistics
    (update-route-statistics route-id u1 u0 u0)
    
    (var-set total-premiums (+ (var-get total-premiums) final-premium))
    (var-set contract-balance (+ (var-get contract-balance) final-premium))
    
    (ok policy-id)
  )
)



(define-private (update-route-statistics (route-id (string-ascii 20)) (policies-delta uint) 
                                        (deliveries-delta uint) (claims-delta uint))
  (let ((current-perf (default-to 
          { total-policies: u0, successful-deliveries: u0, total-claims: u0, total-claims-value: u0,
            average-transit-time: u0, last-incident-block: u0, performance-score: u75 }
          (map-get? route-performance { route-id: route-id }))))
    (map-set route-performance
      { route-id: route-id }
      (merge current-perf {
        total-policies: (+ (get total-policies current-perf) policies-delta),
        successful-deliveries: (+ (get successful-deliveries current-perf) deliveries-delta),
        total-claims: (+ (get total-claims current-perf) claims-delta),
        performance-score: (calculate-performance-score 
          (+ (get total-policies current-perf) policies-delta)
          (+ (get successful-deliveries current-perf) deliveries-delta)
          (+ (get total-claims current-perf) claims-delta))
      })
    )
  )
)

(define-private (calculate-performance-score (total-policies uint) (deliveries uint) (route-claims uint))
  (if (is-eq total-policies u0)
    u75 ;; Default score for new routes
    (let (
      (delivery-rate (if (> total-policies u0) (/ (* deliveries u100) total-policies) u0))
      (claim-rate (if (> total-policies u0) (/ (* route-claims u100) total-policies) u0))
    )
      ;; Performance score based on delivery success and low claim rates
      (if (< (- u100 (/ claim-rate u2)) u1) u1 
        (if (> (- u100 (/ claim-rate u2)) u100) u100 (- u100 (/ claim-rate u2))))
    )
  )
)

(define-public (update-route-delivery (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (route-policy (unwrap! (map-get? route-policies { policy-id: policy-id }) err-not-found))
    (route-id (get route-id route-policy))
  )
    (asserts! (is-eq tx-sender (get receiver policy)) err-unauthorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    
    ;; Update policy status
    (map-set policies
      { policy-id: policy-id }
      (merge policy { status: "delivered" })
    )
    
    ;; Update route statistics
    (update-route-statistics route-id u0 u1 u0)
    
    (ok true)
  )
)

;; Read-only functions for route information
(define-read-only (get-route-info (route-id (string-ascii 20)))
  (map-get? shipping-routes { route-id: route-id })
)

(define-read-only (get-route-performance (route-id (string-ascii 20)))
  (map-get? route-performance { route-id: route-id })
)

(define-read-only (get-route-risk-factors (route-id (string-ascii 20)))
  {
    weather: (map-get? route-risk-factors { route-id: route-id, factor-type: "weather" }),
    political: (map-get? route-risk-factors { route-id: route-id, factor-type: "political" }),
    piracy: (map-get? route-risk-factors { route-id: route-id, factor-type: "piracy" }),
    congestion: (map-get? route-risk-factors { route-id: route-id, factor-type: "congestion" }),
    infrastructure: (map-get? route-risk-factors { route-id: route-id, factor-type: "infrastructure" })
  }
)

(define-read-only (get-policy-route-info (policy-id uint))
  (map-get? route-policies { policy-id: policy-id })
)

(define-read-only (calculate-route-premium-preview (route-id (string-ascii 20)) (value uint))
  (let (
    (base-premium (/ (* value (var-get premium-rate)) u100))
    (route-premium-calc (calculate-route-risk-premium route-id base-premium))
    (final-premium (get premium route-premium-calc))
    (assessment-fee (var-get base-route-assessment-fee))
  )
    {
      base-premium: base-premium,
      route-risk-adjustment: (- final-premium base-premium),
      final-premium: final-premium,
      assessment-fee: assessment-fee,
      total-cost: (+ final-premium assessment-fee)
    }
  )
)




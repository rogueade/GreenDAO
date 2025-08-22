;; transparent-fund-allocation.clar
;; Smart contract for transparent fund allocation with escrow, milestone-based release,
;; multi-signature approvals, and on-chain auditing for impact investment projects

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-PROJECT-NOT-FOUND u101)
(define-constant ERR-MILESTONE-NOT-FOUND u102)
(define-constant ERR-INSUFFICIENT-FUNDS u103)
(define-constant ERR-INSUFFICIENT-APPROVALS u104)
(define-constant ERR-ALREADY-EXECUTED u105)
(define-constant ERR-ALREADY-APPROVED u106)
(define-constant ERR-INVALID-PERCENTAGE u107)

;; Data maps for projects
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    total-funds: uint,
    available-funds: uint,
    required-approvals: uint,
    active: bool,
    created-at: uint
  }
)

;; Data map for milestones
(define-map milestones
  { project-id: uint, milestone-id: uint }
  {
    description: (string-ascii 100),
    funds-percentage: uint,  ;; Percentage of total funds (1-100)
    verified: bool,
    funds-released: bool,
    approval-count: uint,    ;; Track the number of approvals
    created-at: uint
  }
)

;; Data map for approvals
(define-map approvals
  { project-id: uint, milestone-id: uint, approver: principal }
  { 
    approved: bool,
    approved-at: uint
  }
)

;; Data map for project approvers
(define-map project-approvers
  { project-id: uint, approver: principal }
  { can-approve: bool }
)

;; Data map for transaction history (audit trail)
(define-map transactions
  { tx-id: uint }
  {
    project-id: uint,
    milestone-id: (optional uint),
    tx-type: (string-ascii 20),
    amount: uint,
    sender: principal,
    recipient: (optional principal),
    timestamp: uint
  }
)

;; Counters
(define-data-var project-id-counter uint u0)
(define-data-var transaction-id-counter uint u0)

;; Function to register a new impact project
(define-public (register-project (name (string-ascii 100)) (required-approvals uint))
  (let
    (
      (new-project-id (+ (var-get project-id-counter) u1))
      (height stacks-block-height)
    )
    ;; Update project ID counter
    (var-set project-id-counter new-project-id)
    
    ;; Store project information
    (map-set projects
      { project-id: new-project-id }
      {
        owner: tx-sender,
        total-funds: u0,
        available-funds: u0,
        required-approvals: required-approvals,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Add project owner as an approver
    (map-set project-approvers
      { project-id: new-project-id, approver: tx-sender }
      { can-approve: true }
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction new-project-id none "PROJECT_CREATED" u0 none))
    
    ;; Return the new project ID
    (ok new-project-id)
  )
)

;; Function to add an approver to a project
(define-public (add-project-approver (project-id uint) (approver principal))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) (err ERR-PROJECT-NOT-FOUND)))
    )
    ;; Check if caller is the project owner
    (asserts! (is-eq tx-sender (get owner project)) (err ERR-NOT-AUTHORIZED))
    
    ;; Add approver
    (map-set project-approvers
      { project-id: project-id, approver: approver }
      { can-approve: true }
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction project-id none "APPROVER_ADDED" u0 none))
    
    (ok true)
  )
)

;; Function to add funds to a project (escrow)
(define-public (fund-project (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) (err ERR-PROJECT-NOT-FOUND)))
      (new-total-funds (+ (get total-funds project) amount))
      (new-available-funds (+ (get available-funds project) amount))
    )
    ;; Check if project is active
    (asserts! (get active project) (err ERR-PROJECT-NOT-FOUND))
    
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update project funds
    (map-set projects
      { project-id: project-id }
      (merge project {
        total-funds: new-total-funds,
        available-funds: new-available-funds
      })
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction project-id none "FUNDS_ADDED" amount none))
    
    (ok true)
  )
)

;; Function to add a milestone to a project
(define-public (add-milestone 
                (project-id uint) 
                (milestone-id uint) 
                (description (string-ascii 100))
                (funds-percentage uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) (err ERR-PROJECT-NOT-FOUND)))
      (height stacks-block-height)
    )
    ;; Check if caller is the project owner
    (asserts! (is-eq tx-sender (get owner project)) (err ERR-NOT-AUTHORIZED))
    
    ;; Check if percentage is valid (1-100)
    (asserts! (and (>= funds-percentage u1) (<= funds-percentage u100)) (err ERR-INVALID-PERCENTAGE))
    
    ;; Store milestone information
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        description: description,
        funds-percentage: funds-percentage,
        verified: false,
        funds-released: false,
        approval-count: u0,
        created-at: stacks-block-height
      }
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction project-id (some milestone-id) "MILESTONE_ADDED" u0 none))
    
    (ok true)
  )
)

;; Function for approvers to verify a milestone
(define-public (verify-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { project-id: project-id, milestone-id: milestone-id }) (err ERR-MILESTONE-NOT-FOUND)))
      (approver-status (unwrap! (map-get? project-approvers { project-id: project-id, approver: tx-sender }) (err ERR-NOT-AUTHORIZED)))
      (current-approval (map-get? approvals { project-id: project-id, milestone-id: milestone-id, approver: tx-sender }))
      (height stacks-block-height)
    )
    ;; Check if caller is an authorized approver
    (asserts! (get can-approve approver-status) (err ERR-NOT-AUTHORIZED))
    
    ;; Check if already approved
    (asserts! (or (is-none current-approval) (not (get approved (default-to { approved: false, approved-at: u0 } current-approval)))) (err ERR-ALREADY-APPROVED))
    
    ;; Update milestone approval count
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { 
        verified: true,
        approval-count: (+ (get approval-count milestone) u1)
      })
    )
    
    ;; Record approval
    (map-set approvals
      { project-id: project-id, milestone-id: milestone-id, approver: tx-sender }
      { 
        approved: true,
        approved-at: stacks-block-height
      }
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction project-id (some milestone-id) "MILESTONE_VERIFIED" u0 none))
    
    (ok true)
  )
)

;; Function to check if a milestone has enough approvals
(define-read-only (has-sufficient-approvals (project-id uint) (milestone-id uint))
  (match (map-get? projects { project-id: project-id })
    project (match (map-get? milestones { project-id: project-id, milestone-id: milestone-id })
              milestone (>= (get approval-count milestone) (get required-approvals project))
              false)
    false)
)

;; Function to release funds for a verified milestone
(define-public (release-milestone-funds (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) (err ERR-PROJECT-NOT-FOUND)))
      (milestone (unwrap! (map-get? milestones { project-id: project-id, milestone-id: milestone-id }) (err ERR-MILESTONE-NOT-FOUND)))
      (milestone-amount (calculate-milestone-amount project-id milestone-id))
    )
    ;; Check if caller is the project owner
    (asserts! (is-eq tx-sender (get owner project)) (err ERR-NOT-AUTHORIZED))
    
    ;; Check if milestone is verified and funds not yet released
    (asserts! (get verified milestone) (err ERR-MILESTONE-NOT-FOUND))
    (asserts! (not (get funds-released milestone)) (err ERR-ALREADY-EXECUTED))
    
    ;; Check if sufficient approvals received
    (asserts! (has-sufficient-approvals project-id milestone-id) (err ERR-INSUFFICIENT-APPROVALS))
    
    ;; Check if project has sufficient funds
    (asserts! (>= (get available-funds project) milestone-amount) (err ERR-INSUFFICIENT-FUNDS))
    
    ;; Transfer funds from contract to project owner
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get owner project))))
    
    ;; Update milestone as funds released
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { funds-released: true })
    )
    
    ;; Update project available funds
    (map-set projects
      { project-id: project-id }
      (merge project {
        available-funds: (- (get available-funds project) milestone-amount)
      })
    )
    
    ;; Record transaction for audit
    (unwrap-panic (record-transaction project-id (some milestone-id) "FUNDS_RELEASED" milestone-amount (some (get owner project))))
    
    (ok true)
  )
)

;; Helper function to calculate milestone amount based on percentage
(define-read-only (calculate-milestone-amount (project-id uint) (milestone-id uint))
  (let
    (
      (project (default-to 
                 { owner: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM, total-funds: u0, available-funds: u0, required-approvals: u0, active: false, created-at: u0 }
                 (map-get? projects { project-id: project-id })))
      (milestone (default-to 
                   { description: "", funds-percentage: u0, verified: false, funds-released: false, approval-count: u0, created-at: u0 }
                   (map-get? milestones { project-id: project-id, milestone-id: milestone-id })))
      (percentage (get funds-percentage milestone))
      (total-funds (get total-funds project))
    )
    ;; Calculate amount based on percentage of total funds
    (/ (* total-funds percentage) u100)
  )
)

;; Helper function to record transactions for audit trail
(define-private (record-transaction 
                 (project-id uint) 
                 (milestone-id (optional uint)) 
                 (tx-type (string-ascii 20))
                 (amount uint)
                 (recipient (optional principal)))
  (let
    (
      (new-tx-id (+ (var-get transaction-id-counter) u1))
      (height stacks-block-height)
    )
    ;; Update transaction ID counter
    (var-set transaction-id-counter new-tx-id)
    
    ;; Store transaction information
    (map-set transactions
      { tx-id: new-tx-id }
      {
        project-id: project-id,
        milestone-id: milestone-id,
        tx-type: tx-type,
        amount: amount,
        sender: tx-sender,
        recipient: recipient,
        timestamp: stacks-block-height
      }
    )
    
    (ok new-tx-id)
  )
)

;; Read-only function to get project details
(define-read-only (get-project-details (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Read-only function to get milestone details
(define-read-only (get-milestone-details (project-id uint) (milestone-id uint))
  (map-get? milestones { project-id: project-id, milestone-id: milestone-id })
)

;; Read-only function to check if an address is an approver for a project
(define-read-only (is-project-approver (project-id uint) (address principal))
  (let
    (
      (approver-status (map-get? project-approvers { project-id: project-id, approver: address }))
    )
    (if (is-some approver-status)
      (get can-approve (unwrap! approver-status false))
      false
    )
  )
)

;; Read-only function to get transaction details
(define-read-only (get-transaction-details (tx-id uint))
  (map-get? transactions { tx-id: tx-id })
)

;; Read-only function to check if a milestone is approved by a specific approver
(define-read-only (is-milestone-approved-by (project-id uint) (milestone-id uint) (approver principal))
  (let
    (
      (approval (map-get? approvals { project-id: project-id, milestone-id: milestone-id, approver: approver }))
    )
    (if (is-some approval)
      (get approved (unwrap! approval false))
      false
    )
  )
)
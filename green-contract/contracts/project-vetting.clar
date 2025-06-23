;; Project Vetting and Listing Smart Contract
;; Manages the application and approval process for impact projects

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_CLOSED (err u104))
(define-constant ERR_INSUFFICIENT_COMMITTEE_MEMBERS (err u105))
(define-constant ERR_INVALID_ESG_SCORE (err u106))
(define-constant ERR_MISSING_VERIFICATION_DOCS (err u107))

;; Project Status Enum
(define-constant STATUS_PENDING u0)
(define-constant STATUS_UNDER_REVIEW u1)
(define-constant STATUS_APPROVED u2)
(define-constant STATUS_REJECTED u3)
(define-constant STATUS_ACTIVE u4)
(define-constant STATUS_COMPLETED u5)

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var committee-size uint u0)
(define-data-var min-esg-score uint u70) ;; Minimum ESG score required
(define-data-var voting-period uint u1008) ;; ~1 week in blocks

;; Data Maps

;; Project Applications
(define-map projects
  { project-id: uint }
  {
    applicant: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    objectives: (string-ascii 300),
    sustainability-metrics: (string-ascii 200),
    team-info: (string-ascii 300),
    esg-score: uint,
    verification-docs-hash: (buff 32),
    funding-requested: uint,
    status: uint,
    created-at: uint,
    updated-at: uint
  }
)

;; Committee Members
(define-map committee-members
  { member: principal }
  { 
    is-active: bool,
    added-at: uint,
    expertise: (string-ascii 100)
  }
)

;; Voting Sessions
(define-map voting-sessions
  { project-id: uint }
  {
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    total-votes: uint,
    is-active: bool
  }
)

;; Individual Votes
(define-map votes
  { project-id: uint, voter: principal }
  {
    vote: bool, ;; true = approve, false = reject
    timestamp: uint,
    comments: (string-ascii 200)
  }
)

;; Audit Trail
(define-map audit-logs
  { log-id: uint }
  {
    project-id: uint,
    action: (string-ascii 50),
    actor: principal,
    details: (string-ascii 300),
    timestamp: uint
  }
)

(define-data-var audit-counter uint u0)

;; Helper Functions

;; Check if caller is committee member
(define-private (is-committee-member (member principal))
  (match (map-get? committee-members { member: member })
    committee-data (get is-active committee-data)
    false
  )
)

;; Check if caller is contract owner
(define-private (is-contract-owner (caller principal))
  (is-eq caller CONTRACT_OWNER)
)

;; Validate ESG score
(define-private (is-valid-esg-score (score uint))
  (and (>= score u0) (<= score u100))
)

;; Check minimum ESG requirements
(define-private (meets-esg-requirements (score uint))
  (>= score (var-get min-esg-score))
)

;; Add audit log entry
(define-private (add-audit-log (project-id uint) (action (string-ascii 50)) (details (string-ascii 300)))
  (let ((log-id (+ (var-get audit-counter) u1)))
    (var-set audit-counter log-id)
    (map-set audit-logs
      { log-id: log-id }
      {
        project-id: project-id,
        action: action,
        actor: tx-sender,
        details: details,
        timestamp: stacks-block-height
      }
    )
  )
)

;; Public Functions

;; Submit Project Application
(define-public (submit-project-application
  (title (string-ascii 100))
  (description (string-ascii 500))
  (objectives (string-ascii 300))
  (sustainability-metrics (string-ascii 200))
  (team-info (string-ascii 300))
  (esg-score uint)
  (verification-docs-hash (buff 32))
  (funding-requested uint)
)
  (let ((project-id (+ (var-get project-counter) u1)))
    ;; Validate inputs
    (asserts! (is-valid-esg-score esg-score) ERR_INVALID_ESG_SCORE)
    (asserts! (> (len verification-docs-hash) u0) ERR_MISSING_VERIFICATION_DOCS)
    
    ;; Create project entry
    (map-set projects
      { project-id: project-id }
      {
        applicant: tx-sender,
        title: title,
        description: description,
        objectives: objectives,
        sustainability-metrics: sustainability-metrics,
        team-info: team-info,
        esg-score: esg-score,
        verification-docs-hash: verification-docs-hash,
        funding-requested: funding-requested,
        status: STATUS_PENDING,
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      }
    )
    
    ;; Update counter
    (var-set project-counter project-id)
    
    ;; Add audit log
    (add-audit-log project-id "APPLICATION_SUBMITTED" "New project application submitted")
    
    ;; Perform automated vetting
    (try! (automated-vetting-check project-id))
    
    (ok project-id)
  )
)

;; Automated Vetting Check
(define-public (automated-vetting-check (project-id uint))
  (let ((project-data (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    ;; Check if project is in pending status
    (asserts! (is-eq (get status project-data) STATUS_PENDING) ERR_INVALID_STATUS)
    
    ;; Perform automated checks
    (let ((esg-check (meets-esg-requirements (get esg-score project-data)))
          (docs-check (> (len (get verification-docs-hash project-data)) u0)))
      
      (if (and esg-check docs-check)
        ;; Pass automated vetting - move to committee review
        (begin
          (map-set projects
            { project-id: project-id }
            (merge project-data { status: STATUS_UNDER_REVIEW, updated-at: stacks-block-height })
          )
          (add-audit-log project-id "AUTOMATED_VETTING_PASSED" "Project passed automated vetting checks")
          (try! (initiate-committee-voting project-id))
          (ok true)
        )
        ;; Fail automated vetting
        (begin
          (map-set projects
            { project-id: project-id }
            (merge project-data { status: STATUS_REJECTED, updated-at: stacks-block-height })
          )
          (add-audit-log project-id "AUTOMATED_VETTING_FAILED" "Project failed automated vetting checks")
          (ok false)
        )
      )
    )
  )
)

;; Initiate Committee Voting
(define-public (initiate-committee-voting (project-id uint))
  (let ((project-data (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    ;; Check if project is under review
    (asserts! (is-eq (get status project-data) STATUS_UNDER_REVIEW) ERR_INVALID_STATUS)
    
    ;; Check if sufficient committee members
    (asserts! (>= (var-get committee-size) u3) ERR_INSUFFICIENT_COMMITTEE_MEMBERS)
    
    ;; Create voting session
    (map-set voting-sessions
      { project-id: project-id }
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height (var-get voting-period)),
        votes-for: u0,
        votes-against: u0,
        total-votes: u0,
        is-active: true
      }
    )
    
    (add-audit-log project-id "VOTING_INITIATED" "Committee voting session started")
    (ok true)
  )
)

;; Committee Vote
(define-public (cast-committee-vote (project-id uint) (vote bool) (comments (string-ascii 200)))
  (let ((voting-session (unwrap! (map-get? voting-sessions { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    ;; Check if caller is committee member
    (asserts! (is-committee-member tx-sender) ERR_UNAUTHORIZED)
    
    ;; Check if voting is active and within period
    (asserts! (get is-active voting-session) ERR_VOTING_CLOSED)
    (asserts! (<= stacks-block-height (get end-block voting-session)) ERR_VOTING_CLOSED)
    
    ;; Check if already voted
    (asserts! (is-none (map-get? votes { project-id: project-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    ;; Record vote
    (map-set votes
      { project-id: project-id, voter: tx-sender }
      {
        vote: vote,
        timestamp: stacks-block-height,
        comments: comments
      }
    )
    
    ;; Update voting session
    (map-set voting-sessions
      { project-id: project-id }
      (merge voting-session {
        votes-for: (if vote (+ (get votes-for voting-session) u1) (get votes-for voting-session)),
        votes-against: (if vote (get votes-against voting-session) (+ (get votes-against voting-session) u1)),
        total-votes: (+ (get total-votes voting-session) u1)
      })
    )
    
    (add-audit-log project-id "COMMITTEE_VOTE_CAST" 
      (if vote "Committee member voted to approve" "Committee member voted to reject"))
    
    (ok true)
  )
)

;; Finalize Voting
(define-public (finalize-voting (project-id uint))
  (let ((voting-session (unwrap! (map-get? voting-sessions { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
        (project-data (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    
    ;; Check if voting period has ended
    (asserts! (> stacks-block-height (get end-block voting-session)) ERR_VOTING_CLOSED)
    (asserts! (get is-active voting-session) ERR_VOTING_CLOSED)
    
    ;; Determine result (simple majority)
    (let ((votes-for (get votes-for voting-session))
          (votes-against (get votes-against voting-session))
          (approved (> votes-for votes-against)))
      
      ;; Update project status
      (map-set projects
        { project-id: project-id }
        (merge project-data {
          status: (if approved STATUS_APPROVED STATUS_REJECTED),
          updated-at: stacks-block-height
        })
      )
      
      ;; Close voting session
      (map-set voting-sessions
        { project-id: project-id }
        (merge voting-session { is-active: false })
      )
      
      (add-audit-log project-id "VOTING_FINALIZED" 
        (if approved "Project approved by committee" "Project rejected by committee"))
      
      (ok approved)
    )
  )
)

;; Activate Approved Project
(define-public (activate-project (project-id uint))
  (let ((project-data (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    ;; Check if project is approved
    (asserts! (is-eq (get status project-data) STATUS_APPROVED) ERR_INVALID_STATUS)
    
    ;; Only project applicant or contract owner can activate
    (asserts! (or (is-eq tx-sender (get applicant project-data)) 
                  (is-contract-owner tx-sender)) ERR_UNAUTHORIZED)
    
    ;; Update status to active
    (map-set projects
      { project-id: project-id }
      (merge project-data { status: STATUS_ACTIVE, updated-at: stacks-block-height })
    )
    
    (add-audit-log project-id "PROJECT_ACTIVATED" "Project moved to active status")
    (ok true)
  )
)

;; Complete Project
(define-public (complete-project (project-id uint))
  (let ((project-data (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    ;; Check if project is active
    (asserts! (is-eq (get status project-data) STATUS_ACTIVE) ERR_INVALID_STATUS)
    
    ;; Only project applicant or contract owner can complete
    (asserts! (or (is-eq tx-sender (get applicant project-data)) 
                  (is-contract-owner tx-sender)) ERR_UNAUTHORIZED)
    
    ;; Update status to completed
    (map-set projects
      { project-id: project-id }
      (merge project-data { status: STATUS_COMPLETED, updated-at: stacks-block-height })
    )
    
    (add-audit-log project-id "PROJECT_COMPLETED" "Project marked as completed")
    (ok true)
  )
)

;; Admin Functions

;; Add Committee Member
(define-public (add-committee-member (member principal) (expertise (string-ascii 100)))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR_UNAUTHORIZED)
    
    (map-set committee-members
      { member: member }
      {
        is-active: true,
        added-at: stacks-block-height,
        expertise: expertise
      }
    )
    
    (var-set committee-size (+ (var-get committee-size) u1))
    (ok true)
  )
)

;; Remove Committee Member
(define-public (remove-committee-member (member principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR_UNAUTHORIZED)
    
    (match (map-get? committee-members { member: member })
      committee-data
      (begin
        (map-set committee-members
          { member: member }
          (merge committee-data { is-active: false })
        )
        (var-set committee-size (- (var-get committee-size) u1))
        (ok true)
      )
      ERR_PROJECT_NOT_FOUND
    )
  )
)

;; Update ESG Requirements
(define-public (update-min-esg-score (new-score uint))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-valid-esg-score new-score) ERR_INVALID_ESG_SCORE)
    
    (var-set min-esg-score new-score)
    (ok true)
  )
)

;; Read-Only Functions

;; Get Project Details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Get Voting Session
(define-read-only (get-voting-session (project-id uint))
  (map-get? voting-sessions { project-id: project-id })
)

;; Get Committee Member Info
(define-read-only (get-committee-member (member principal))
  (map-get? committee-members { member: member })
)

;; Get Vote
(define-read-only (get-vote (project-id uint) (voter principal))
  (map-get? votes { project-id: project-id, voter: voter })
)

;; Get Audit Log
(define-read-only (get-audit-log (log-id uint))
  (map-get? audit-logs { log-id: log-id })
)

;; Get Project Count
(define-read-only (get-project-count)
  (var-get project-counter)
)

;; Get Committee Size
(define-read-only (get-committee-size)
  (var-get committee-size)
)

;; Get Min ESG Score
(define-read-only (get-min-esg-score)
  (var-get min-esg-score)
)

;; Check Project Status
(define-read-only (get-project-status (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project-data (some (get status project-data))
    none
  )
)
;; Project Registration and Vetting Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-project-exists (err u101))
(define-constant err-project-not-found (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-invalid-criteria (err u104))
(define-constant err-already-vetted (err u105))
(define-constant err-not-ready-for-vetting (err u106))
(define-constant err-vetting-in-progress (err u107))
(define-constant err-invalid-score (err u108))
(define-constant err-already-voted (err u109))

;; Define project status values
(define-constant status-draft "draft")
(define-constant status-submitted "submitted")
(define-constant status-under-review "under-review")
(define-constant status-approved "approved")
(define-constant status-rejected "rejected")
(define-constant status-listed "listed")

;; Define data maps
(define-map projects
  { id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    category: (string-ascii 32),
    location: (string-ascii 64),
    funding-goal: uint,
    impact-metrics: (list 10 (string-ascii 64)),
    status: (string-ascii 16),
    created-at: uint,
    updated-at: uint
  }
)

(define-map project-details
  { project-id: uint }
  {
    team-members: (list 10 (string-ascii 64)),
    timeline: (string-ascii 256),
    milestones: (list 10 (string-ascii 128)),
    documentation: (string-ascii 256),
    social-links: (list 5 (string-ascii 128))
  }
)

(define-map impact-criteria
  { id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    weight: uint,
    min-score: uint,
    max-score: uint
  }
)

(define-map vetting-results
  { project-id: uint, criteria-id: uint, reviewer: principal }
  {
    score: uint,
    comments: (string-ascii 256),
    timestamp: uint
  }
)

(define-map project-vetting-summary
  { project-id: uint }
  {
    total-score: uint,
    average-score: uint,
    reviewer-count: uint,
    status: (string-ascii 16),
    started-at: uint,
    completed-at: (optional uint)
  }
)

(define-map vetters
  { address: principal }
  {
    name: (string-ascii 64),
    active: bool,
    projects-reviewed: uint,
    added-at: uint
  }
)

;; Define variables
(define-data-var project-counter uint u0)
(define-data-var criteria-counter uint u0)
(define-data-var min-vetters-required uint u3)
(define-data-var approval-threshold uint u70) ;; 70% score required for approval

;; Initialize default impact criteria
(begin
  (map-set impact-criteria
    { id: u1 }
    {
      name: "Environmental Impact",
      description: "Measures the project's positive effect on the environment",
      weight: u25,
      min-score: u0,
      max-score: u10
    }
  )
  (map-set impact-criteria
    { id: u2 }
    {
      name: "Social Impact",
      description: "Measures the project's positive effect on communities",
      weight: u25,
      min-score: u0,
      max-score: u10
    }
  )
  (map-set impact-criteria
    { id: u3 }
    {
      name: "Economic Sustainability",
      description: "Measures the project's long-term economic viability",
      weight: u20,
      min-score: u0,
      max-score: u10
    }
  )
  (map-set impact-criteria
    { id: u4 }
    {
      name: "Innovation",
      description: "Measures the project's innovative approach to solving problems",
      weight: u15,
      min-score: u0,
      max-score: u10
    }
  )
  (map-set impact-criteria
    { id: u5 }
    {
      name: "Scalability",
      description: "Measures the project's potential for growth and replication",
      weight: u15,
      min-score: u0,
      max-score: u10
    }
  )
  (var-set criteria-counter u5)
)

;; Check if caller is authorized
(define-private (is-vetter)
  (default-to false (get active (map-get? vetters { address: tx-sender })))
)

;; Register a new project (draft status)
(define-public (register-project (name (string-ascii 64)) (description (string-ascii 256)) (category (string-ascii 32)) (location (string-ascii 64)) (funding-goal uint) (impact-metrics (list 10 (string-ascii 64))))
  (let
    (
      (project-id (+ (var-get project-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> (len impact-metrics) u0) (err err-invalid-criteria))
    
    (map-set projects
      { id: project-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        category: category,
        location: location,
        funding-goal: funding-goal,
        impact-metrics: impact-metrics,
        status: status-draft,
        created-at: current-block,
        updated-at: current-block
      }
    )
    
    (var-set project-counter project-id)
    (ok project-id)
  )
)


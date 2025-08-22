;; DAO Governance Contract
;; Implements decentralized governance for protocol upgrades, fund allocation, and decision-making

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-PROPOSAL-NOT-FOUND u101)
(define-constant ERR-VOTING-PERIOD-ENDED u102)
(define-constant ERR-VOTING-PERIOD-ACTIVE u103)
(define-constant ERR-PROPOSAL-NOT-PASSED u104)
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED u105)
(define-constant ERR-TIMELOCK-NOT-EXPIRED u106)
(define-constant ERR-INSUFFICIENT-TOKENS u107)
(define-constant ERR-ALREADY-VOTED u108)
(define-constant ERR-INVALID-VOTE u109)
(define-constant ERR-QUORUM-NOT-MET u110)
(define-constant ERR-INVALID-PROPOSAL-TYPE u111)
(define-constant ERR-TREASURY-INSUFFICIENT-FUNDS u112)
(define-constant ERR-SELF-DELEGATION u113)
(define-constant ERR-PROPOSAL-EXPIRED u114)

;; Governance Parameters
(define-constant VOTING_PERIOD u1008) ;; ~7 days in blocks (144 blocks/day)
(define-constant TIMELOCK_PERIOD u432) ;; ~3 days in blocks
(define-constant PROPOSAL_THRESHOLD u1000000) ;; 1M tokens to create proposal
(define-constant QUORUM_THRESHOLD u20) ;; 20% of total supply must vote
(define-constant EXECUTION_GRACE_PERIOD u2016) ;; ~14 days to execute after timelock

;; Contract Owner (for initial setup)
(define-constant CONTRACT_OWNER tx-sender)

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var total-token-supply uint u100000000) ;; 100M tokens
(define-data-var governance-token principal CONTRACT_OWNER) ;; Will be set to actual token contract
(define-data-var treasury-balance uint u0)

;; Proposal Types
(define-constant PROPOSAL-TYPE-FUND-ALLOCATION "FUND_ALLOCATION")
(define-constant PROPOSAL-TYPE-PARAMETER-CHANGE "PARAMETER_CHANGE")
(define-constant PROPOSAL-TYPE-PROTOCOL-UPGRADE "PROTOCOL_UPGRADE")
(define-constant PROPOSAL-TYPE-GENERAL "GENERAL")

;; Vote Options
(define-constant VOTE-FOR u1)
(define-constant VOTE-AGAINST u2)
(define-constant VOTE-ABSTAIN u3)

;; Proposal Structure
(define-map proposals
  uint ;; proposal-id
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20),
    target: (optional principal), ;; Target contract/address for execution
    amount: uint, ;; For fund allocation proposals
    votes-for: uint,
    votes-against: uint,
    votes-abstain: uint,
    total-votes: uint,
    created-at: uint,
    voting-ends-at: uint,
    executed: bool,
    execution-eta: (optional uint), ;; When proposal can be executed (after timelock)
    quorum-met: bool
  }
)

;; User Token Balances (for voting power calculation)
(define-map token-balances
  principal
  {
    balance: uint,
    locked-until: uint, ;; For stake duration bonuses
    stake-multiplier: uint ;; 100 = 1x, 150 = 1.5x, etc.
  }
)

;; Vote Records
(define-map votes
  {proposal-id: uint, voter: principal}
  {
    vote: uint, ;; 1=for, 2=against, 3=abstain
    voting-power: uint,
    voted-at: uint
  }
)

;; Delegation Records
(define-map delegations
  principal ;; delegator
  {
    delegate: principal,
    delegated-at: uint,
    active: bool
  }
)

;; Delegated Voting Power (aggregate)
(define-map delegated-power
  principal ;; delegate
  {
    total-power: uint,
    delegator-count: uint
  }
)

;; Treasury Transactions
(define-map treasury-transactions
  uint ;; tx-id
  {
    proposal-id: uint,
    recipient: principal,
    amount: uint,
    executed-at: uint,
    tx-type: (string-ascii 20)
  }
)

(define-data-var treasury-tx-counter uint u0)

;; Proposal Creation
(define-public (create-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 20))
  (target (optional principal))
  (amount uint))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (user-balance (get-voting-power tx-sender))
      (voting-ends-at (+ stacks-block-height VOTING_PERIOD))
    )
    ;; Check if user has enough tokens to create proposal
    (asserts! (>= user-balance PROPOSAL_THRESHOLD) (err ERR-INSUFFICIENT-TOKENS))
    
    ;; Validate proposal type
    (asserts! (or 
      (is-eq proposal-type PROPOSAL-TYPE-FUND-ALLOCATION)
      (is-eq proposal-type PROPOSAL-TYPE-PARAMETER-CHANGE)
      (is-eq proposal-type PROPOSAL-TYPE-PROTOCOL-UPGRADE)
      (is-eq proposal-type PROPOSAL-TYPE-GENERAL)
    ) (err ERR-INVALID-PROPOSAL-TYPE))
    
    ;; For fund allocation, check treasury has sufficient funds
    (if (is-eq proposal-type PROPOSAL-TYPE-FUND-ALLOCATION)
      (asserts! (<= amount (var-get treasury-balance)) (err ERR-TREASURY-INSUFFICIENT-FUNDS))
      true
    )
    
    ;; Create proposal
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      proposal-type: proposal-type,
      target: target,
      amount: amount,
      votes-for: u0,
      votes-against: u0,
      votes-abstain: u0,
      total-votes: u0,
      created-at: stacks-block-height,
      voting-ends-at: voting-ends-at,
      executed: false,
      execution-eta: none,
      quorum-met: false
    })
    
    ;; Update proposal counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
  )
)

;; Voting Function
(define-public (vote (proposal-id uint) (vote-option uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
      (voting-power (get-effective-voting-power tx-sender))
      (existing-vote (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))
    )
    ;; Check if voting period is active
    (asserts! (<= stacks-block-height (get voting-ends-at proposal)) (err ERR-VOTING-PERIOD-ENDED))
    
    ;; Check if user hasn't voted yet
    (asserts! (is-none existing-vote) (err ERR-ALREADY-VOTED))
    
    ;; Validate vote option
    (asserts! (or (is-eq vote-option VOTE-FOR) 
                  (is-eq vote-option VOTE-AGAINST) 
                  (is-eq vote-option VOTE-ABSTAIN)) (err ERR-INVALID-VOTE))
    
    ;; Record vote
    (map-set votes {proposal-id: proposal-id, voter: tx-sender} {
      vote: vote-option,
      voting-power: voting-power,
      voted-at: stacks-block-height
    })
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id 
      (merge proposal {
        votes-for: (if (is-eq vote-option VOTE-FOR) 
                     (+ (get votes-for proposal) voting-power) 
                     (get votes-for proposal)),
        votes-against: (if (is-eq vote-option VOTE-AGAINST) 
                         (+ (get votes-against proposal) voting-power) 
                         (get votes-against proposal)),
        votes-abstain: (if (is-eq vote-option VOTE-ABSTAIN) 
                         (+ (get votes-abstain proposal) voting-power) 
                         (get votes-abstain proposal)),
        total-votes: (+ (get total-votes proposal) voting-power),
        quorum-met: (>= (+ (get total-votes proposal) voting-power) 
                       (get-quorum-threshold))
      })
    )
    
    (ok true)
  )
)

;; Delegation Functions
(define-public (delegate-voting-power (delegate principal))
  (let
    (
      (current-delegation (map-get? delegations tx-sender))
      (user-power (get-voting-power tx-sender))
    )
    ;; Cannot delegate to self
    (asserts! (not (is-eq tx-sender delegate)) (err ERR-SELF-DELEGATION))
    
    ;; Remove existing delegation if any
    (match current-delegation
      existing-del (if (get active existing-del)
                     (unwrap! (remove-delegation-power (get delegate existing-del) user-power) (err ERR-NOT-AUTHORIZED))
                     true)
      true
    )
    
    ;; Create new delegation
    (map-set delegations tx-sender {
      delegate: delegate,
      delegated-at: stacks-block-height,
      active: true
    })
    
    ;; Add power to delegate
    (unwrap! (add-delegation-power delegate user-power) (err ERR-NOT-AUTHORIZED))
    
    (ok true)
  )
)

(define-public (revoke-delegation)
  (let
    (
      (current-delegation (unwrap! (map-get? delegations tx-sender) (err ERR-NOT-AUTHORIZED)))
      (user-power (get-voting-power tx-sender))
    )
    ;; Mark delegation as inactive
    (map-set delegations tx-sender 
      (merge current-delegation {active: false}))
    
    ;; Remove power from delegate
    (unwrap! (remove-delegation-power (get delegate current-delegation) user-power) (err ERR-NOT-AUTHORIZED))
    
    (ok true)
  )
)

;; Proposal Execution (after timelock)
(define-public (queue-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
      (execution-eta (+ stacks-block-height TIMELOCK_PERIOD))
    )
    ;; Check if voting period has ended
    (asserts! (> stacks-block-height (get voting-ends-at proposal)) (err ERR-VOTING-PERIOD-ACTIVE))
    
    ;; Check if proposal passed (more for than against) and quorum met
    (asserts! (and (> (get votes-for proposal) (get votes-against proposal))
                   (get quorum-met proposal)) (err ERR-PROPOSAL-NOT-PASSED))
    
    ;; Check if not already executed
    (asserts! (not (get executed proposal)) (err ERR-PROPOSAL-ALREADY-EXECUTED))
    
    ;; Queue proposal for execution
    (map-set proposals proposal-id 
      (merge proposal {execution-eta: (some execution-eta)}))
    
    (ok execution-eta)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
      (execution-eta (unwrap! (get execution-eta proposal) (err ERR-TIMELOCK-NOT-EXPIRED)))
    )
    ;; Check if timelock has expired
    (asserts! (>= stacks-block-height execution-eta) (err ERR-TIMELOCK-NOT-EXPIRED))
    
    ;; Check if proposal hasn't expired (grace period)
    (asserts! (<= stacks-block-height (+ execution-eta EXECUTION_GRACE_PERIOD)) (err ERR-PROPOSAL-EXPIRED))
    
    ;; Check if not already executed
    (asserts! (not (get executed proposal)) (err ERR-PROPOSAL-ALREADY-EXECUTED))
    
    ;; Execute based on proposal type
    (if (is-eq (get proposal-type proposal) PROPOSAL-TYPE-FUND-ALLOCATION)
      (unwrap! (execute-fund-allocation proposal-id proposal) (err ERR-NOT-AUTHORIZED))
      true ;; Other proposal types would have custom execution logic
    )
    
    ;; Mark as executed
    (map-set proposals proposal-id 
      (merge proposal {executed: true}))
    
    (ok true)
  )
)

;; Treasury Management
(define-public (add-to-treasury (amount uint))
  (begin
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update treasury balance
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    
    (ok true)
  )
)

;; Token Management (simplified - would integrate with actual token contract)
(define-public (stake-tokens (amount uint) (lock-period uint))
  (let
    (
      (current-balance (default-to {balance: u0, locked-until: u0, stake-multiplier: u100} 
                                  (map-get? token-balances tx-sender)))
      (new-locked-until (+ stacks-block-height lock-period))
      (stake-multiplier (calculate-stake-multiplier lock-period))
    )
    ;; Update user balance with staking info
    (map-set token-balances tx-sender {
      balance: (+ (get balance current-balance) amount),
      locked-until: new-locked-until,
      stake-multiplier: stake-multiplier
    })
    
    (ok true)
  )
)

;; Helper Functions

;; Calculate voting power (base tokens + delegation)
(define-read-only (get-effective-voting-power (user principal))
  (let
    (
      (base-power (get-voting-power user))
      (delegated-to-user (default-to {total-power: u0, delegator-count: u0} 
                                    (map-get? delegated-power user)))
      (user-delegation (map-get? delegations user))
    )
    ;; If user has delegated their power, they can't vote directly
    (match user-delegation
      delegation (if (get active delegation)
                   u0
                   (+ base-power (get total-power delegated-to-user)))
      (+ base-power (get total-power delegated-to-user))
    )
  )
)

;; Get base voting power (tokens * multiplier)
(define-read-only (get-voting-power (user principal))
  (let
    (
      (balance-info (default-to {balance: u0, locked-until: u0, stake-multiplier: u100} 
                               (map-get? token-balances user)))
    )
    (/ (* (get balance balance-info) (get stake-multiplier balance-info)) u100)
  )
)

;; Calculate stake multiplier based on lock period
(define-private (calculate-stake-multiplier (lock-period uint))
  (if (<= lock-period u144) ;; Less than 1 day
    u100 ;; 1x multiplier
    (if (<= lock-period u1008) ;; Less than 1 week
      u110 ;; 1.1x multiplier
      (if (<= lock-period u4320) ;; Less than 1 month
        u125 ;; 1.25x multiplier
        u150 ;; 1.5x multiplier for 1+ month
      )
    )
  )
)

;; Get quorum threshold
(define-read-only (get-quorum-threshold)
  (/ (* (var-get total-token-supply) QUORUM_THRESHOLD) u100)
)

;; Execute fund allocation
(define-private (execute-fund-allocation (proposal-id uint) (proposal {proposer: principal, title: (string-ascii 100), description: (string-ascii 500), proposal-type: (string-ascii 20), target: (optional principal), amount: uint, votes-for: uint, votes-against: uint, votes-abstain: uint, total-votes: uint, created-at: uint, voting-ends-at: uint, executed: bool, execution-eta: (optional uint), quorum-met: bool}))
  (let
    (
      (recipient (unwrap! (get target proposal) (err ERR-NOT-AUTHORIZED)))
      (amount (get amount proposal))
      (tx-id (+ (var-get treasury-tx-counter) u1))
    )
    ;; Check treasury has sufficient funds
    (asserts! (>= (var-get treasury-balance) amount) (err ERR-TREASURY-INSUFFICIENT-FUNDS))
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update treasury balance
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    
    ;; Record transaction
    (map-set treasury-transactions tx-id {
      proposal-id: proposal-id,
      recipient: recipient,
      amount: amount,
      executed-at: stacks-block-height,
      tx-type: "FUND_ALLOCATION"
    })
    
    (var-set treasury-tx-counter tx-id)
    
    (ok true)
  )
)

;; Helper functions for delegation power management
(define-private (add-delegation-power (delegate principal) (power uint))
  (let
    (
      (current-power (default-to {total-power: u0, delegator-count: u0} 
                                (map-get? delegated-power delegate)))
    )
    (map-set delegated-power delegate {
      total-power: (+ (get total-power current-power) power),
      delegator-count: (+ (get delegator-count current-power) u1)
    })
    (ok true)
  )
)

(define-private (remove-delegation-power (delegate principal) (power uint))
  (let
    (
      (current-power (default-to {total-power: u0, delegator-count: u0} 
                                (map-get? delegated-power delegate)))
    )
    (map-set delegated-power delegate {
      total-power: (- (get total-power current-power) power),
      delegator-count: (- (get delegator-count current-power) u1)
    })
    (ok true)
  )
)

;; Read-only functions for querying

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-user-delegation (user principal))
  (map-get? delegations user)
)

(define-read-only (get-delegated-power (delegate principal))
  (map-get? delegated-power delegate)
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-user-balance (user principal))
  (map-get? token-balances user)
)

(define-read-only (get-treasury-transaction (tx-id uint))
  (map-get? treasury-transactions tx-id)
)

(define-read-only (get-governance-stats)
  {
    total-proposals: (var-get proposal-counter),
    treasury-balance: (var-get treasury-balance),
    total-token-supply: (var-get total-token-supply),
    quorum-threshold: (get-quorum-threshold),
    voting-period: VOTING_PERIOD,
    timelock-period: TIMELOCK_PERIOD
  }
)
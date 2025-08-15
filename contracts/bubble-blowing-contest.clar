;; Simple Bubble Blowing Contest
;; Core contest registration and management system

;; Error codes
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-UNAUTHORIZED (err u403))
(define-constant ERR-INVALID-PHASE (err u400))

;; Contest phases
(define-constant PHASE-REGISTRATION u0)
(define-constant PHASE-ACTIVE u1)
(define-constant PHASE-JUDGING u2)
(define-constant PHASE-COMPLETED u3)

;; Data structures
(define-map contests
  { contest-id: uint }
  {
    organizer: principal,
    title: (string-ascii 64),
    registration-end: uint,
    contest-end: uint,
    phase: uint,
    winner: (optional principal)
  }
)

(define-map participants
  { contest-id: uint, participant: principal }
  {
    registered-at: uint,
    technique-shared: bool,
    photo-submitted: bool,
    score: uint
  }
)

(define-map recipes
  { recipe-id: uint }
  {
    creator: principal,
    title: (string-ascii 32),
    ingredients: (string-ascii 256),
    instructions: (string-ascii 512)
  }
)

;; State variables
(define-data-var next-contest-id uint u1)
(define-data-var next-recipe-id uint u1)

;; Create new contest
(define-public (create-contest (title (string-ascii 64)) (registration-blocks uint) (contest-blocks uint))
  (let ((contest-id (var-get next-contest-id))
        (current-height stacks-block-height))
    (map-set contests
      { contest-id: contest-id }
      {
        organizer: tx-sender,
        title: title,
        registration-end: (+ current-height registration-blocks),
        contest-end: (+ current-height registration-blocks contest-blocks),
        phase: PHASE-REGISTRATION,
        winner: none
      }
    )
    (var-set next-contest-id (+ contest-id u1))
    (ok contest-id)
  )
)

;; Register for contest
(define-public (register-participant (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR-NOT-FOUND)))
    (asserts! (< stacks-block-height (get registration-end contest)) ERR-INVALID-PHASE)
    (asserts! (is-none (map-get? participants { contest-id: contest-id, participant: tx-sender })) ERR-ALREADY-EXISTS)
    (map-set participants
      { contest-id: contest-id, participant: tx-sender }
      {
        registered-at: stacks-block-height,
        technique-shared: false,
        photo-submitted: false,
        score: u0
      }
    )
    (ok true)
  )
)

;; Share technique
(define-public (share-technique (contest-id uint))
  (let ((participant (unwrap! (map-get? participants { contest-id: contest-id, participant: tx-sender }) ERR-NOT-FOUND)))
    (map-set participants
      { contest-id: contest-id, participant: tx-sender }
      (merge participant { technique-shared: true })
    )
    (ok true)
  )
)

;; Submit photo documentation
(define-public (submit-photo (contest-id uint))
  (let ((participant (unwrap! (map-get? participants { contest-id: contest-id, participant: tx-sender }) ERR-NOT-FOUND)))
    (map-set participants
      { contest-id: contest-id, participant: tx-sender }
      (merge participant { photo-submitted: true })
    )
    (ok true)
  )
)

;; Add bubble solution recipe
(define-public (add-recipe (title (string-ascii 32)) (ingredients (string-ascii 256)) (instructions (string-ascii 512)))
  (let ((recipe-id (var-get next-recipe-id)))
    (map-set recipes
      { recipe-id: recipe-id }
      {
        creator: tx-sender,
        title: title,
        ingredients: ingredients,
        instructions: instructions
      }
    )
    (var-set next-recipe-id (+ recipe-id u1))
    (ok recipe-id)
  )
)

;; Judge participant (organizer only)
(define-public (judge-participant (contest-id uint) (participant principal) (score uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR-NOT-FOUND))
        (participant-data (unwrap! (map-get? participants { contest-id: contest-id, participant: participant }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer contest)) ERR-UNAUTHORIZED)
    (asserts! (> stacks-block-height (get registration-end contest)) ERR-INVALID-PHASE)
    (map-set participants
      { contest-id: contest-id, participant: participant }
      (merge participant-data { score: score })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-contest (contest-id uint))
  (map-get? contests { contest-id: contest-id })
)

(define-read-only (get-participant (contest-id uint) (participant principal))
  (map-get? participants { contest-id: contest-id, participant: participant })
)

(define-read-only (get-recipe (recipe-id uint))
  (map-get? recipes { recipe-id: recipe-id })
)

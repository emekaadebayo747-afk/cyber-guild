;; Cyber Guild - Blockchain Gaming Ecosystem
;; A decentralized autonomous gaming organization platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-amount (err u105))

;; Data Variables
(define-data-var guild-counter uint u0)
(define-data-var skill-dna-counter uint u0)

;; Data Maps
(define-map guilds 
    { guild-id: uint }
    {
        name: (string-ascii 50),
        leader: principal,
        treasury-balance: uint,
        member-count: uint,
        reputation-score: uint,
        created-at: uint
    }
)

(define-map guild-members
    { guild-id: uint, member: principal }
    {
        joined-at: uint,
        contribution-score: uint,
        reputation-stake: uint,
        is-active: bool
    }
)

(define-map skill-dna-nfts
    { token-id: uint }
    {
        owner: principal,
        skill-level: uint,
        experience-points: uint,
        evolution-stage: uint,
        guild-id: uint,
        created-at: uint
    }
)

(define-map member-guild-index
    { member: principal }
    { guild-id: uint }
)

;; Read-only functions
(define-read-only (get-guild (guild-id uint))
    (map-get? guilds { guild-id: guild-id })
)

(define-read-only (get-guild-member (guild-id uint) (member principal))
    (map-get? guild-members { guild-id: guild-id, member: member })
)

(define-read-only (get-skill-dna (token-id uint))
    (map-get? skill-dna-nfts { token-id: token-id })
)

(define-read-only (get-member-guild (member principal))
    (map-get? member-guild-index { member: member })
)

(define-read-only (get-guild-counter)
    (ok (var-get guild-counter))
)

(define-read-only (get-skill-dna-counter)
    (ok (var-get skill-dna-counter))
)

;; Public functions

;; Create a new guild
(define-public (create-guild (name (string-ascii 50)))
    (let
        (
            (new-guild-id (+ (var-get guild-counter) u1))
            (block-height-now block-height)
        )
        (asserts! (is-none (get-member-guild tx-sender)) err-already-exists)
        
        (map-set guilds
            { guild-id: new-guild-id }
            {
                name: name,
                leader: tx-sender,
                treasury-balance: u0,
                member-count: u1,
                reputation-score: u100,
                created-at: block-height-now
            }
        )
        
        (map-set guild-members
            { guild-id: new-guild-id, member: tx-sender }
            {
                joined-at: block-height-now,
                contribution-score: u0,
                reputation-stake: u0,
                is-active: true
            }
        )
        
        (map-set member-guild-index
            { member: tx-sender }
            { guild-id: new-guild-id }
        )
        
        (var-set guild-counter new-guild-id)
        (ok new-guild-id)
    )
)

;; Join an existing guild
(define-public (join-guild (guild-id uint))
    (let
        (
            (guild (unwrap! (get-guild guild-id) err-not-found))
            (block-height-now block-height)
        )
        (asserts! (is-none (get-member-guild tx-sender)) err-already-exists)
        
        (map-set guild-members
            { guild-id: guild-id, member: tx-sender }
            {
                joined-at: block-height-now,
                contribution-score: u0,
                reputation-stake: u0,
                is-active: true
            }
        )
        
        (map-set member-guild-index
            { member: tx-sender }
            { guild-id: guild-id }
        )
        
        (map-set guilds
            { guild-id: guild-id }
            (merge guild { member-count: (+ (get member-count guild) u1) })
        )
        
        (ok true)
    )
)

;; Mint a new Skill DNA NFT
(define-public (mint-skill-dna)
    (let
        (
            (new-token-id (+ (var-get skill-dna-counter) u1))
            (member-guild-data (get-member-guild tx-sender))
            (guild-id (default-to u0 (get guild-id member-guild-data)))
            (block-height-now block-height)
        )
        (map-set skill-dna-nfts
            { token-id: new-token-id }
            {
                owner: tx-sender,
                skill-level: u1,
                experience-points: u0,
                evolution-stage: u1,
                guild-id: guild-id,
                created-at: block-height-now
            }
        )
        
        (var-set skill-dna-counter new-token-id)
        (ok new-token-id)
    )
)

;; Update Skill DNA experience points
(define-public (update-skill-dna-xp (token-id uint) (xp-gained uint))
    (let
        (
            (skill-dna (unwrap! (get-skill-dna token-id) err-not-found))
        )
        (asserts! (is-eq (get owner skill-dna) tx-sender) err-unauthorized)
        
        (let
            (
                (new-xp (+ (get experience-points skill-dna) xp-gained))
                (new-level (+ (get skill-level skill-dna) (/ new-xp u1000)))
                (new-evolution (+ (get evolution-stage skill-dna) (/ new-level u10)))
            )
            (map-set skill-dna-nfts
                { token-id: token-id }
                (merge skill-dna {
                    experience-points: new-xp,
                    skill-level: new-level,
                    evolution-stage: new-evolution
                })
            )
            (ok true)
        )
    )
)

;; Contribute to guild treasury
(define-public (contribute-to-treasury (guild-id uint) (amount uint))
    (let
        (
            (guild (unwrap! (get-guild guild-id) err-not-found))
            (member (unwrap! (get-guild-member guild-id tx-sender) err-unauthorized))
        )
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set guilds
            { guild-id: guild-id }
            (merge guild { treasury-balance: (+ (get treasury-balance guild) amount) })
        )
        
        (map-set guild-members
            { guild-id: guild-id, member: tx-sender }
            (merge member { 
                contribution-score: (+ (get contribution-score member) amount)
            })
        )
        
        (ok true)
    )
)

;; Stake reputation
(define-public (stake-reputation (guild-id uint) (stake-amount uint))
    (let
        (
            (member (unwrap! (get-guild-member guild-id tx-sender) err-unauthorized))
        )
        (asserts! (> stake-amount u0) err-invalid-amount)
        
        (map-set guild-members
            { guild-id: guild-id, member: tx-sender }
            (merge member { 
                reputation-stake: (+ (get reputation-stake member) stake-amount)
            })
        )
        
        (ok true)
    )
)

;; Update guild reputation
(define-public (update-guild-reputation (guild-id uint) (reputation-change uint))
    (let
        (
            (guild (unwrap! (get-guild guild-id) err-not-found))
        )
        (asserts! (is-eq (get leader guild) tx-sender) err-unauthorized)
        
        (map-set guilds
            { guild-id: guild-id }
            (merge guild { 
                reputation-score: (+ (get reputation-score guild) reputation-change)
            })
        )
        
        (ok true)
    )
)

;; Transfer guild leadership
(define-public (transfer-leadership (guild-id uint) (new-leader principal))
    (let
        (
            (guild (unwrap! (get-guild guild-id) err-not-found))
        )
        (asserts! (is-eq (get leader guild) tx-sender) err-unauthorized)
        (asserts! (is-some (get-guild-member guild-id new-leader)) err-not-found)
        
        (map-set guilds
            { guild-id: guild-id }
            (merge guild { leader: new-leader })
        )
        
        (ok true)
    )
)
(namespace "free")
(define-keyset "free.cyberfly_team" (read-keyset "cyberfly_team"))
(module  cyberfly_node GOV
  @doc "Cyberfly node register"
  (use coin)
  (use free.cyberfly)
  (defcap GOV()
  (enforce-keyset "free.cyberfly_team"))

(defconst ADMIN_ACCOUNT "k:f53af5c83e21316f10bdca39c9353fafdb317326f430eb4cd143bdf3faa5ba88")
(defconst STAKING_VAULT_ACCOUNT "cyberfly-staking-bank" "Account holding staked CFLY")
(defconst REWARDS_VAULT_ACCOUNT "cyberfly-reward-bank" "Account holding CFLY rewards")

  (defschema node-schema
      @doc "Node Register schema"
      peer_id:string
      status:string
      multiaddr:string
      account:string
      guard:guard
      registered_at:time
      last_updated:time
     )

     (defschema stake-schema
      @doc "Node staking schema"
      peer_id:string
      active:bool
      account:string
      amount:decimal
      claimed:decimal
      last_claim:time
      stake_time:time
      )

     (defschema stake-count-schema
      total-stakes:integer
      total-staked-amount:decimal
     )

   (deftable node-table:{node-schema})
   (deftable stakes-table:{stake-schema})
   (deftable stake-count-table:{stake-count-schema})

  (defconst STAKE_AMOUNT 50000.0)
  (defconst TOTAL_REWARD 40000000.0) ; 40% of 100,000,000 tokens
  (defconst DISTRIBUTION_DAYS (* 15.0 365.25))
  (defconst TOTAL_DAILY_REWARD (/ TOTAL_REWARD DISTRIBUTION_DAYS))

  
  (defcap NODE_GUARD (peer_id:string)
   (with-read node-table peer_id {"guard":=guard}
     (enforce-guard guard)
     )
  )

  (defcap ACCOUNT_AUTH (account:string)
  @doc "Capability to ensure the caller is the account owner"
  (enforce-guard (at "guard" (coin.details account)))
)

  
  (defcap ADMIN_GUARD ()
   "Only cyberfly node monitor admin can perform actions"
  (enforce-guard (at 'guard (coin.details ADMIN_ACCOUNT)))
  )

  (defcap BANK_DEBIT () true)

  (defcap PRIVATE ()
  true
)

(defun is-node-account (peer_id:string account:string)
(with-read node-table peer_id
  {"account":=node_account}
(= node_account account)
)
)


(defun new-node(peer_id:string status:string multiaddr:string account:string guard:keyset)

(enforce (is-node-active peer_id) "Node already exists")
(enforce (is-principal account)  "Invalid account structure: non-principal account")

(insert node-table peer_id {
  "peer_id": peer_id,
  "multiaddr": multiaddr,
  "status": status,
  "account": account,
  "guard": guard,
  "registered_at": (at "block-time" (chain-data)),
  "last_updated": (at "block-time" (chain-data))
})
)
  
  (defun update-node(peer_id:string
                  multiaddr:string
                  status:string)
  (with-capability (NODE_GUARD peer_id)
  (with-read node-table peer_id {
    "status":=node_status
  }

   (if (and (= status "active") (!= node_status status))
   [  (update stakes-table peer_id {
    "last_claim":  (at "block-time" (chain-data))
  })]
  [

  ]
   )
   (update node-table peer_id {
                  "multiaddr":multiaddr
                 ,"status":status
                 ,"last_updated": (at "block-time" (chain-data))
                })
  )
  )
  )
  
  (defun update-node-admin(peer_id:string 
    multiaddr:string
    status:string)
@doc "node can be updated by monitoring node"
(with-capability (ADMIN_GUARD)
(with-capability (BANK_DEBIT)
(with-read node-table peer_id {
"account" := account,
"status" := node_status
}
(if (is-staked peer_id)
[
(if (and (= status "inactive") (!= node_status status))
[(let* ((calc-result (calculate-days-and-reward peer_id))
  (reward (at "reward" calc-result))
  (total-stakes (at "total-stakes" calc-result))
  (days (at "days" calc-result)))
  (if (> reward 0.0)
  [
  (with-default-read stakes-table peer_id
  { "claimed" : 0.0 }
  { "claimed" := claimed }
  (update stakes-table peer_id {
    "last_claim" : (at "block-time" (chain-data)),
    "claimed" : (+ claimed reward)
  })
  (install-capability (free.cyberfly.TRANSFER REWARDS_VAULT_ACCOUNT account reward))
  (free.cyberfly.transfer REWARDS_VAULT_ACCOUNT account reward)
  (format "Node {} disabled and rewarded {} CFLY for an account {} for ran node for {} days. total stakes - {}" [peer_id reward account days total-stakes])
  )
  ]
  "No pending reward"
  )
  )]
[]
)
(if (!= node_status status)
[ 
(update stakes-table peer_id {
"last_claim" : (at "block-time" (chain-data))
})
]
[]
)
]
[]
)
(update node-table peer_id {
"multiaddr" : multiaddr,
"status" : status,
"last_updated" : (at "block-time" (chain-data))
})
)
)
)
)
  
  (defun update-node-account(peer_id:string
    account:string
    guard:keyset)
    @doc "Monitoring node can update node's reward account and guard"
  (with-capability (ADMIN_GUARD)
  (enforce (not (is-staked peer_id)) "Updates cannot be made if the asset is already staked.")
  (update node-table peer_id {
   "guard":guard,
   "account":account,
   "last_updated": (at "block-time" (chain-data))
  }
  )
  )
  )
  
  (defun get-node(peer_id:string)

  (read node-table peer_id ["multiaddr", "account", "status", "guard"])
   
  )
  
  (defun get-all-active-nodes()
   (select node-table ["peer_id", "multiaddr", "status", "account", "guard"] (where 'status (= "active"))
  ))

  (defun is-node-active(peer_id:string)
  (with-default-read node-table peer_id
    { "status": "inactive" }
    { "status" := status }
    (= status "active")
  )
)
(defun is-staked (peer_id:string)
(with-default-read stakes-table peer_id
  { "active": false }
  { "active" := active }
  active
)
)

(defun is-staked-account (account:string peer_id:string)
(with-read stakes-table peer_id
  { "account" := staked_account }
  (= staked_account account)
)
)



(defun init()
(with-capability(GOV)
 (insert stake-count-table "count" {
  "total-stakes":0,
  "total-staked-amount":0.0
 })
)
)
  (defun stake(account:string peer_id:string)
    (with-capability (ACCOUNT_AUTH account)
    (with-capability (NODE_GUARD peer_id)
      
      
      (let* (
      (node-active (is-node-active peer_id))
      (node-account (is-node-account peer_id account))
      (staked (is-staked peer_id))
      )
      (enforce node-active "Node is not active")
      (enforce node-account "Account is not matching")
      (enforce (not staked) "Already staked on this node")
      )
    

      (with-default-read stakes-table peer_id
      {
      "claimed":0.0
      }
      {
      "claimed":=claimed
      }
      (write stakes-table peer_id {
        "account": account,
        "peer_id": peer_id,
        "active": true,
        "amount":STAKE_AMOUNT,
        "claimed":claimed,
        "last_claim":  (at "block-time" (chain-data)),
        "stake_time":  (at "block-time" (chain-data))
      })
       
      (with-read stake-count-table "count"
      {"total-stakes":=total-stakes,
       "total-staked-amount":=total-staked-amount}
      (update stake-count-table "count" {
        "total-stakes": (+ total-stakes 1),
        "total-staked-amount": (+ total-staked-amount STAKE_AMOUNT)
      }
      )
      )
      )
      (free.cyberfly.transfer account STAKING_VAULT_ACCOUNT STAKE_AMOUNT)
      (format "Staked {} for account {} on node {}" [STAKE_AMOUNT account peer_id])
    )
  )
  )

  (defun unstake(account:string peer_id:string)
  (with-capability (ACCOUNT_AUTH account)
    (with-capability (BANK_DEBIT)
      (let* (
        (staked (is-staked peer_id))
        (staked-account (is-staked-account account peer_id))
        )
        (enforce staked "Not staked on this node")
        (enforce staked-account "Account not matching")
        )

      (with-read stakes-table peer_id
        { "amount" := amount,
          "account":=staked_account}
        (let* (
            (calc-result (calculate-days-and-reward peer_id))
            (reward (at "reward" calc-result))
            (days (at "days" calc-result))
        )
        (enforce (= reward 0.0) "Please claim reward before unstake")
          (update stakes-table peer_id
            { "active": false, "amount": 0.0, "last_claim":  (at "block-time" (chain-data))
          }
          )
          (with-read stake-count-table "count"
            {"total-stakes":= total-stakes,
             "total-staked-amount":=total-staked-amount
            }
            (update stake-count-table "count" {
              "total-stakes": (- total-stakes 1),
              "total-staked-amount": (- total-staked-amount STAKE_AMOUNT)
            })
          )
          (install-capability (free.cyberfly.TRANSFER STAKING_VAULT_ACCOUNT staked_account amount))
          (free.cyberfly.transfer STAKING_VAULT_ACCOUNT staked_account amount)
          (format "Unstaked {} CFLY for account {} from node {}" [amount staked_account peer_id])
        )
      )
    )
  )
)

(defun calculate-days-and-reward (peer_id:string)
    (with-default-read stakes-table peer_id
      { "last_claim" : (at "block-time" (chain-data)), 
        "active" : false }
      { "last_claim" := last_claim, 
         "active" := active }
   (let (
        (node-active (is-node-active peer_id))
   )

   (with-read stake-count-table "count" {
    "total-stakes":=total-stakes
   }
   (let* (
    (current-time (at "block-time" (chain-data)))
    (days-since-last-claim (round (/ (diff-time current-time last_claim) 86400.0) 2))
    (reward 
      (if (>= days-since-last-claim 1.0)
        (round (* (/ TOTAL_DAILY_REWARD total-stakes) days-since-last-claim) 2)
        0.0
      )
      )
  )

  (if (and active node-active)
  
    {
      "days": days-since-last-claim,
      "reward": reward,
      "current-time": current-time,
      "total-stakes":total-stakes
    }
    {
      "days": days-since-last-claim,
      "reward": 0.0,
      "current-time": current-time,
      "total-stakes":total-stakes 
    }
  )

  )
  
  )
   )
    )
  )


(defun claim-reward (account:string peer_id:string)
    (with-capability (ACCOUNT_AUTH account)
    (with-capability (BANK_DEBIT)
        (let* (
          (calc-result (calculate-days-and-reward peer_id))
          (reward (at "reward" calc-result))
          (days (at "days" calc-result))
          (total-stakes (at "total-stakes" calc-result))
          (staked (is-staked peer_id))
          (staked-account (is-staked-account account peer_id))
          (node-active (is-node-active peer_id))
        )
        (enforce staked "Not staked on this node")
        (enforce staked-account  "Account not matching")
        (enforce node-active "Node does not active")
        (enforce (> reward 0.0) "No rewards to claim")
        (with-read stakes-table peer_id
           {
            "claimed":= claimed
           }
          (update stakes-table peer_id {
            "last_claim": (at "block-time" (chain-data)),
            "claimed": (+ claimed reward)
          })
          (install-capability (free.cyberfly.TRANSFER REWARDS_VAULT_ACCOUNT account reward))
          (free.cyberfly.transfer REWARDS_VAULT_ACCOUNT account reward)
          (format "Claimed {} CFLY node rewards for account {} from node {} running for {} days. total stakes - {}" [reward account peer_id days total-stakes])
          )
)
    )
    )
  )


  (defun calculate-apy ()
  (with-read stake-count-table "count"
    { "total-stakes" := num-stakes }
    (let*
      (
        (daily-reward-per-stake 
          (if (= num-stakes 0)
              (/ (* TOTAL_DAILY_REWARD 10000) 1.0)
              (/ (* TOTAL_DAILY_REWARD 10000) num-stakes) ; Multiply by 10000 for precision
              
              )) 
        (annual-reward-per-stake (/ (* daily-reward-per-stake 365.25) 10000))
        (apy (/ (* annual-reward-per-stake 10000) STAKE_AMOUNT))
      )
      (round (/ apy 100.0) 2)
    )
  )
) 

(defun get-node-stake(peer_id:string)
(read stakes-table peer_id
)
)

(defun get-active-stakes ()
(select stakes-table (where "active" (= true)))
)

(defun get-account-nodes (account:string)
(select node-table (where "account" (= account)))
)

(defun get-stakes-stats()
(read stake-count-table "count" ["total-stakes" "total-staked-amount"])
)

(defun get-user-stakes(account:string)
@doc "Get all stakes for a specific user"
(select stakes-table ["peer_id", "active", "amount", "claimed", "last_claim", "stake_time"]
  (where 'account (= account)))
)
  



  (defun create-cyberfly-user-guard (funder:string amount:decimal account:string)
  (free.cyberfly.transfer-create funder account 
    (create-BANK_DEBIT-guard) amount)
)

;; Capability user guard: capability predicate function
(defun require-BANK_DEBIT () 
  (require-capability (BANK_DEBIT))
)

;; Capability user guard: guard constructor
(defun create-BANK_DEBIT-guard ()
  (create-user-guard (require-BANK_DEBIT))
)


)
(create-table node-table)
(create-table stakes-table)
(create-table stake-count-table)

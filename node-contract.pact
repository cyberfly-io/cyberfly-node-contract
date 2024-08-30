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
   (with-read node-table peer_id{"guard":=guard}
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
                  account:string
                  status:string)
  (with-capability (NODE_GUARD peer_id)
  (with-read node-table peer_id {
    "account":=account,
    "status":=node_status
  }

   (if (and (= status "active") (!= node_status status))
   [  (update stakes-table (format "{}:{}" [account peer_id]) {
    "last-claim":  (at "block-time" (chain-data))
  })]
  [

  ]
   )
   (update node-table peer_id {
                  "multiaddr":multiaddr
                 ,"account":account
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
  (with-capability(BANK_DEBIT)
  (with-read node-table peer_id {
    "account":=account,
    "status":=node_status
  }
  (if(is-staked account peer_id)
  
  (if(!= node_status status)
  [ 
   
    (update stakes-table (format "{}:{}" [account peer_id]) {
      "last-claim": (at "block-time" (chain-data))
    }) 
  ]
  []
  )
  (if(and (= status "inactive") (!= node_status status))
  (let* (
    (calc-result (calculate-days-and-reward account peer_id))
    (reward (at "reward" calc-result))
)
  (if (> reward 0.0)
  [
    (with-default-read stakes-table (format "{}:{}" [account peer_id])
    {
      "claimed":0.0
     }
     {
      "claimed":=claimed
     }
    (update stakes-table (format "{}:{}" [account, peer_id]){
      "last-claimed":  (at "block-time" (chain-data)),
      "claimed": (+ claimed reward)
    })
    (free.cyberfly.transfer REWARDS_VAULT_ACCOUNT account reward)
    (format "Node {} disabled and rewarded {} for account {}" [peer_id reward account ])
    )
   

  ]
  "No pending reward"
  )
)
  )
  )
  (update node-table peer_id {
    "multiaddr":multiaddr
   ,"status":status
   ,"last_updated": (at "block-time" (chain-data))
})
  )
  )
  ))
  
  (defun update-node-guard(peer_id:string
    guard:keyset)
  (with-capability (NODE_GUARD peer_id)
  (update node-table peer_id {
   "guard":guard,
   "last_updated": (at "block-time" (chain-data))
  })
  ))
  
  (defun get-node(peer_id:string)
   (with-default-read node-table peer_id
    {
      "last_updated": (at "block-time" (chain-data))
    }
    {
           "multiaddr":=multiaddr
           ,"account":=account
           ,"status":=status
           , "guard":=guard
           , "last_updated":=last_updated
   }
   {"peer_id":peer_id , "multiaddr":multiaddr, "status":status, "guard":guard, "account":account, "last_updated":last_updated  })
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
(defun is-staked (account:string peer_id:string)
(with-default-read stakes-table (format "{}:{}" [account peer_id])
  { "active": false }
  { "active" := active }
  active
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
      (enforce (is-node-active peer_id) "Node does not active")
      (enforce (not (is-staked account peer_id)) "Already staked on this node")
      (with-default-read stakes-table (format "{}:{}" [account peer_id])
      {
      "claimed":0.0
      }
      {
      "claimed":=claimed
      }
      (write stakes-table (format "{}:{}" [account peer_id]) {
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
      (free.cyberfly.transfer account STAKING_VAULT_ACCOUNT STAKE_AMOUNT)
      (format "Staked {} for account {} on node {}" [STAKE_AMOUNT account peer_id])
      )
      )
     
      )
    
    )
  )
  )

  (defun unstake(account:string peer_id:string)
  (with-capability (ACCOUNT_AUTH account)
    (with-capability (BANK_DEBIT)
      (enforce (is-staked account peer_id) "Not staked on this node")
      (with-read stakes-table (format "{}:{}" [account peer_id])
        { "amount" := amount}
        (let* (
            (calc-result (calculate-days-and-reward account peer_id))
            (reward (at "reward" calc-result))
        )
          (update stakes-table (format "{}:{}" [account peer_id])
            { "active": false, "amount": 0.0 }
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
          (if (> reward 0.0)
          [
            (with-read stakes-table (format "{}:{}" [account peer_id])
            
             {
              "claimed":=claimed
             }
            (update stakes-table (format "{}:{}" [account, peer_id]){
              "last-claimed":  (at "block-time" (chain-data)),
              "claimed": (+ claimed reward)
            })
            (free.cyberfly.transfer REWARDS_VAULT_ACCOUNT account reward)
            )

          ]
          "No pending reward"
          )
          (free.cyberfly.transfer STAKING_VAULT_ACCOUNT account amount)
          (format "Unstaked {} and rewarded {} for account {} from node {}" [amount reward account peer_id])
        )
      )
    )
  )
)

(defun calculate-days-and-reward (account:string peer_id:string)
    (with-default-read stakes-table (format "{}:{}" [account peer_id])
      { "last_claim" : (at "block-time" (chain-data)), 
        "active" : false }
      { "last_claim" := last_claim, 
         "active" := active }
   (enforce active "Stake is not active")
   (enforce (is-node-active peer_id) "Node is not active")
   (with-read stake-count-table "count" {
    "total-stakes":=total-stakes
   }
   (let* (
    (current-time (at "block-time" (chain-data)))
    (days-since-last-claim (/ (diff-time current-time last_claim) 86400.0))
    (reward 
      (if (>= days-since-last-claim 1.0)
        (* (/ TOTAL_DAILY_REWARD total-stakes) days-since-last-claim)
        0.0
      )
      )
  )
  {
    "days": days-since-last-claim,
    "reward": reward,
    "current-time": current-time
  })
   )
    )
  )


(defun claim-reward (account:string peer_id:string)
    (with-capability (ACCOUNT_AUTH account)
    (with-capability (BANK_DEBIT)
      (enforce (is-staked account peer_id) "Not staked on this node")
      (enforce (is-node-active peer_id) "Node does not active")
        (let* (
          (calc-result (calculate-days-and-reward account peer_id))
          (reward (at "reward" calc-result))
        )
 (enforce (> reward 0.0) "No rewards to claim")
   (with-read stakes-table (format "{}:{}" [account peer_id])
           {
            "claimed":= claimed
           }
          (update stakes-table (format "{}:{}" [account, peer_id]){
            "last-claimed": (at "block-time" (chain-data)),
            "claimed": (+ claimed reward)
          })
          (free.cyberfly.transfer REWARDS_VAULT_ACCOUNT account reward)
          (format "Claimed {} rewards for account {} from node {}" [reward account peer_id])
          )
)
    )
    )
  )


  (defun calculate-apy ()
  (with-read stake-count-table "count"
    { "total-stakes" := num-stakes }
    (let*
      ((daily-reward-per-stake 
        (if (= num-stakes 0)
            0.0
            (/ (* TOTAL_DAILY_REWARD 10000) num-stakes))) ; Multiply by 10000 for precision
       (annual-reward-per-stake (/ (* daily-reward-per-stake * 365.25) 10000))
       (apy (/ (* annual-reward-per-stake 10000) STAKE_AMOUNT)))
      (/ apy 100.0)))) ; Divide by 100.0 to get percentage

(defun get-active-stakes ()
(select stakes-table (where "active" (= true)))
)

(defun get-stakes-stats()
(read stake-count-table "count" ["total-stakes" "total-staked-amount"])
)

(defun get-user-stakes(account:string)
@doc "Get all stakes for a specific user"
(select stakes-table ["peer_id", "active", "amount", "claimed", "last_claim", "stake_time"]
  (where 'account (= account)))
)
  

  (defun withdraw-funds(account:string amount:decimal)
  (with-capability (ADMIN_GUARD)
  (with-capability (BANK_DEBIT)
    (free.cyberfly.transfer account ADMIN_ACCOUNT amount)
  ))
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

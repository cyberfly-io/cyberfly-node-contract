(namespace "free")
(module  cyberfly_node GOV
  @doc " Cyberfly node register"
  (use coin)
  (use free.cyberfly)
  (defcap GOV()
  (enforce-keyset "free.cyberfly_team"))

(defconst ADMIN_ACCOUNT "k:f53af5c83e21316f10bdca39c9353fafdb317326f430eb4cd143bdf3faa5ba88")

  (defschema node-schema
      @doc "Node Register schema"
      peer_id:string
      status:string
      multiaddr:string
      account:string
      guard:guard
     )
   (deftable node-table:{node-schema})
  
  (defcap NODE_GUARD (peer_id:string)
   (with-read node-table peer_id{"guard":=guard}
     (enforce-guard guard)
     )
  )
  
  
  
  (defcap ADMIN_GUARD ()
   "Only cyberfly node monitor admin can perform actions"
  (enforce-guard (at 'guard (coin.details ADMIN_ACCOUNT)))
  )

  (defcap BANK_DEBIT () true)

  (defcap PRIVATE ()
  true
)

  (defun new-node(peer_id:string
           status:string
           multiaddr:string
           account:string
           guard:keyset)
  
  
  (insert node-table peer_id {
    "peer_id":peer_id
    ,"multiaddr":multiaddr
    ,"status":status
    ,"account":account
    ,"guard":guard
  })
  )
  
  (defun update-node(peer_id:string
                  multiaddr:string
                  account:string
                  status:string)
  (with-capability (NODE_GUARD peer_id)
   (update node-table peer_id {
                  "multiaddr":multiaddr
                 ,"account":account
                 ,"status":status
         })
  ))
  
  (defun update-node-admin(peer_id:string ;node can be updated by monitoring node
                  multiaddr:string
                  status:string)
  (with-capability (ADMIN_GUARD)
   (update node-table peer_id {
                  "multiaddr":multiaddr
                 ,"status":status
         })
  ))
  
  (defun update-node-guard(peer_id:string
    guard:keyset)
  (with-capability (NODE_GUARD peer_id)
  (update node-table peer_id {
   "guard":guard
  })
  ))
  
  (defun get-node(peer_id:string)
   (with-read node-table peer_id {
           "multiaddr":=multiaddr
           ,"account":=account
           ,"status":=status
           , "guard":=guard
   }
   {"peer_id":peer_id , "multiaddr":multiaddr, "status":status, "guard":guard, "account":account  })
  )
  
  (defun get-all-nodes()
   (select node-table ["peer_id", "multiaddr", "status", "account", "guard"] (where 'status (= "active"))
  ))
  

  (defun withdraw-funds(account:string amount:decimal)
  (with-capability (ADMIN_GUARD)
  (with-capability (BANK_DEBIT)
    (free.cyberfly.transfer account ADMIN_ACCOUNT amount)
  ))
)


  (defun create-simple-user-guard (funder:string amount:decimal account:string)
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
  

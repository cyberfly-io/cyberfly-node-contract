(namespace "free")
(module  cyberfly_node GOV
@doc " Cyberfly node register"
(use coin)
(defcap GOV()
(enforce-keyset "free.cyberfly_team"))
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
(enforce-guard (at 'guard (coin.details "k:f53af5c83e21316f10bdca39c9353fafdb317326f430eb4cd143bdf3faa5ba88")))
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
(with-capability (ADMIN_GUARD) ; need to implement admin guard capability
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

)

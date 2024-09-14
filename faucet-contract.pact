(namespace "free")
(define-keyset "free.cyberfly_team" (read-keyset "cyberfly_team"))

(module cyberfly_faucet GOVERNANCE

  "'cyberfly_faucet' represents cyberfly's token Faucet Contract."

  (use free.cyberfly_token)

  (defcap GOVERNANCE()
    (enforce-keyset "free.cyberfly_team")
  )

  
  (defcap ALLOW_FUNDING () true)
  (defcap BANK_DEBIT () true)


  (defconst FAUCET_ACCOUNT "cyberfly-faucet")

 
  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (defschema history
    @doc "Table to record the behavior of addresses. Last transaction time,       \
    \ total coins earned, and total coins returned are inserted or updated at     \
    \ transaction. "
    total-coins-earned:decimal
    total-coins-returned:decimal
    last-request-time:time
    )

  (deftable history-table: {history})

  ; --------------------------------------------------------------------------
  ; Constants
  ; --------------------------------------------------------------------------

  (defconst MAX_COIN_PER_REQUEST:decimal 50000.0)
  (defconst WAIT_TIME_PER_REQUEST 1800.0)
  (defconst EPOCH (time "1970-01-01T00:00:00Z"))

  ; --------------------------------------------------------------------------
  ; Coin Faucet Contract
  ; --------------------------------------------------------------------------
  (defun request-coin:string (address:string amount:decimal)

    (enforce (<= amount MAX_COIN_PER_REQUEST)
      "Has reached maximum coin amount per request")

    (with-capability (ALLOW_FUNDING)
    (with-capability (BANK_DEBIT)
    (install-capability(TRANSFER FAUCET_ACCOUNT address amount))
      (transfer FAUCET_ACCOUNT address amount)))

    (with-default-read history-table address
      { "total-coins-earned": 0.0,
      "total-coins-returned": 0.0,
      "last-request-time": EPOCH
      }
      { "total-coins-earned":= total-coins-earned,
      "total-coins-returned":= total-coins-returned,
      "last-request-time":= last-request-time
      }

      (enforce (>= (diff-time (curr-time) last-request-time) WAIT_TIME_PER_REQUEST)
        "Coin can be requested every 30 minutes")

      (let  (( total-coins (+ amount total-coins-earned)))

        (write history-table address {
          "total-coins-earned": total-coins,
          "total-coins-returned": total-coins-returned,
          "last-request-time": (curr-time) }))))

  (defun create-and-request-coin:string (address:string address-guard:guard amount:decimal)
    @doc "Transfers AMOUNT of coins up to MAX_COIN_PER_REQUEST from the faucet    \
    \ account to the requester account at ADDRESS. Inserts or updates the         \
    \ transaction of the account at ADDRESS in history-table. Limits the number   \
    \ of coin requests by time, WAIT_TIME_PER_REQUEST "
    @model [(property (<= amount MAX_COIN_PER_REQUEST))]

    (enforce (<= amount MAX_COIN_PER_REQUEST)
      "Has reached maximum coin amount per request")

      (with-capability (ALLOW_FUNDING)
      (with-capability (BANK_DEBIT)
      (install-capability(TRANSFER FAUCET_ACCOUNT address amount))
        (transfer-create FAUCET_ACCOUNT address address-guard amount)))
      (insert history-table address {
        "total-coins-earned": amount,
        "total-coins-returned": 0.0,
        "last-request-time": (curr-time) }))

  (defun return-coin:string (address:string amount:decimal)
    @doc "Returns the AMOUNT of coin from account at ADDRESS back to the faucet   \
    \ account after use. Updates the transaction of the account at ADDRESS in     \
    \ history-table keep track of behavior. "
    @model [(property (> amount 0.0))]

    (with-read history-table address
      {"total-coins-returned":= coins-returned}
      (transfer address FAUCET_ACCOUNT amount)
      (update history-table address
        {"total-coins-returned": (+ amount coins-returned)})))

  (defun read-history:object{history} (address:string)
    @doc "Returns history of the account at ADDRESS"
    (read history-table address))

  (defun curr-time ()
    (at 'block-time (chain-data)))


    (defun create-cyberfly-faucet-user-guard (funder:string amount:decimal account:string)
    (free.cyberfly_token.transfer-create funder account 
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

(create-table history-table)


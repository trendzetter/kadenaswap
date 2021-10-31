(namespace (read-msg 'ns))

(module delegated-bonding GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'delegated-bonding-admin))
  )

  (defconst POOL "delegated-bonding-pool")
  (defconst MAX_AMOUNT 50000)

  (use util.guards)

  (defschema tranche
    account:string     ;; KDA account for the full bond
    amount:decimal     ;; tranche amount
  )

  (defschema multi
   size:decimal                 ;; bond size
   tranches:[object{tranche}]   ;; tranches
  )

  (deftable multis:{multi}) ;; stored by multi KDA account

  (deftable tranches:{tranche})

  (defun wrap-new-bond:string
    ( pool:string      ;; Bond pool name
      account:string   ;; KDA account
      guard:guard      ;; bond administration guard
    )
    (test.pool.new-bond pool account (create-module-guard "bond-wrapper"))
  )

  (defun tranches-keys () (keys tranches))

  (defun new-multibond:string
    ( multi:object{multi}               ;; multi tranches
      account:string                    ;; KDA account for multi/multi ID
    )
    ;; debit from each tranche
    ;(map (at 'tranches multi) (debit-tranche account))
    (map (debit-tranche account) (at 'tranches multi) )

    ;; store the multi
    (insert multis account multi)
    ;; allow the autonomous transfer to relay bank
    (install-capability
      (coin.TRANSFER account "relay-bank" (at 'size multi)))    ;; create the bond
    (test.pool.new-bond "kda-relay-pool" account
      (create-module-guard "multibond"))
  )

  (defun debit-tranche (account:string tranche:object{tranche})
    (coin.transfer-create
      (at 'account tranche)
      account
      (create-module-guard "tranche")
      (at 'amount tranche))
  )

  (defun renew-multibond:string (account:string)
    ;; track the old balance
    (let ( (old-balance (coin.get-balance account))
           (multi (read multis account)) )
      ;; renew, will credit account
      (test.pool.renew account)
      ;; compute new amount
      (let ( (amount (- (coin.get-balance account) old-balance)) )
        ;; allocate
        (map
          (allocate account amount (at 'size multi))
          (at 'tranches multi))))
  )

  (defun allocate
    ( account:string           ;; multi account
      amount:decimal           ;; total amount to allocate
      size:decimal             ;; bond size
      tranche:object{tranche}  ;; tranche
    )
    (let ( (to (at 'account tranche))
           ;; compute tranche amount
           (tranche-amount (* amount (/ (at 'amount tranche) size))) )
      (install-capability
        (coin.TRANSFER account to tranche-amount))
      (coin.transfer account to tranche-amount))
  )

)

(create-table tranches)
(create-table multis)

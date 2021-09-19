(namespace (read-msg 'ns)) 

(module delegated-bonding GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'delegated-bonding-admin))
  )

  (defconst POOL "delegated-bonding-pool")

  (use util.guards)

  (defschema reservation-schema
    account:string
    guard:guard
    slot:string
    time:time
    amount:decimal
    status:string
  )

  (deftable reservations:{reservation-schema})

  (defschema slot-schema
    amount:decimal
    status:string
  )

  (deftable slots:{slot-schema})


  (defschema tranche-schema
    multi:string       ;; Name of the multi to subscribe
    account:string     ;; KDA account
    amount:decimal     ;; tranche amount      
  )
  
  (defschema multi-schema
   account:string               ;; KDA account
   size:decimal                 ;; bond size
   tranches:[object{tranche-schema}]   ;; tranches
  )
  
  (deftable multis:{multi-schema}) ;; stored by multi KDA account

  (deftable tranches:{tranche-schema}) 

  (defun wrap-new-bond:string
    ( pool:string    ;; Bond pool name
    account:string   ;; KDA account
    guard:guard      ;; bond administration guard
    )
    (test.pool.new-bond pool account (create-module-guard "bond-wrapper"))
  )

  (defun new-slot:string
    (
      slot:string
      amount:decimal
    )
    (insert slots slot {
      'amount:amount,
      'status:'NEW
    })
  )

  (defun new-reservation:string
    (
      account:string
      guard:guard
      slot:string
      amount:decimal
    )
    (insert reservations (format "{}:{}" [account slot]) {
      'account: account,
      'guard: guard,
      'slot: slot,
      'time: (chain-time),
      'amount: amount,
      'status:'NEW
    })
  )

  (defun new-tranche:string (
    multi:string
    account:string
    amount:decimal
   )
   (with-read multis multi
    { 'account:= account
    , 'size:= size
    , 'amount:=multi-amount
    }
    (let*
      ( (date (chain-time))
        (tranche (format "{}:{}" [account (format-time "%F" date)]))
        (new-amount (+ amount multi-amount))
      )
      (insert tranches tranche {
        'multi:multi,
        'account:account,
        'amount:amount
      })
    )
   )
  )

  (defun new-multibond:string
    ( multi:object{multi-schema}        ;; multi tranches
      account:string             ;; KDA account for multi/multi ID
    )
    ;; debit from each tranche
    (map (debit-tranche (account)) (at 'tranches multi ))    
    ;; store the multi
    (insert multis account multi)    
    ;; allow the autonomous transfer to relay bank
    (install-capability 
      (coin.TRANSFER account "relay-bank" (at 'size multi)))    ;; create the bond
    (test.pool.new-bond "kda-relay-pool" account
      (create-module-guard "multibond"))
  )

  (defun debit-tranche (account:string tranche:object{tranche-schema})
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
      tranche:object{tranche-schema}  ;; tranche
    )
    (let ( (to (at 'account tranche))
    
           ;; compute tranche amount
           (tranche-amount (* amount (/ (at 'amount tranche) size))) )
           
      (install-capability
        (coin.TRANSFER account to tranche-amount))
      (coin.transfer account to tranche-amount))
  )

)
(create-table slots)
(create-table reservations)
;(if (read-msg 'upgrade)
;  ["upgrade"]
  ;[ 
  ;  (create-table slots)
  ;]
;)
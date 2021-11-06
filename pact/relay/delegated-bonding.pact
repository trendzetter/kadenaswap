(namespace (read-msg 'ns))

(module delegated-bonding GOVERNANCE

  @doc " slots for shared bonds"
  @model
  [ (defproperty valid-account-id (accountId:string)
      (and
        (>= (length accountId) 3)
        (<= (length accountId) 256))) ]

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'delegated-bonding-admin))
  )


  ;; TODO: remove
  (defconst POOL 'delegated-bonding-pool)
  (defconst MAX_AMOUNT 50000)

  (defschema last-id-schema
    last-id:integer)
  (deftable last-id-table:{last-id-schema})

  (use util.guards)

  (defschema tranche
    account:string     ;; account paying the tranche and receiving the rewards
    slot:string        ;; KDA account for the full bond
    amount:decimal     ;; tranche amount
    guard:guard        ;; keyset controlling the tranche (future rotation)
    status:string      ;; current status of the tranche
  )

  (defschema slot
    ;; key:  accountname for the autonomous controlled account
    amount:decimal     ;; total and max amount
    operator:guard     ;; keyset that will operator the app
    fee:decimal        ;; percentage operator fee
  )

  (defschema multi
   size:decimal                 ;; bond size
   tranches:[object{tranche}]   ;; tranches
  )

  (deftable multis:{multi}) ;; stored by multi KDA account

  (deftable tranches:{tranche})

  (deftable slots:{slot})

  (defun wrap-new-bond:string
    ( pool:string      ;; Bond pool name
      account:string   ;; KDA account
      guard:guard      ;; bond administration guard
    )
    (test.pool.new-bond pool account (create-module-guard 'bond-wrapper))
  )

  (defun get-all-slots ()
  @doc " Return all slots. "
  (format "{}:{}" [(map (read slots) (keys slots)) (keys slots)] ))

  (defun get-all-tranches ()
  (map (read tranches) (keys tranches)))

  (defun new-slot:string
    ( account:string
      amount:decimal
      operator:guard
      fee:decimal
    )
    (insert slots account { 'amount: amount, 'operator: operator, 'fee: fee })
    (coin.create-account account (create-module-guard 'reservations))
  )

  (defun get-slot-tranche-amounts
  (slot:string)
  @doc " Return trache amounts for slot "
  (select tranches [ 'amount ] (where 'slot (= slot))))

  (defun get-slot-total-amount:decimal
  (slot:string)
  @doc " Return to total amount of tranches "
    ;(fold (+) 0.0 (map (at 'amount) [{"amount": 2000.0} {"amount": 3000.0}] ) )
    (fold (+) 0.0 (map (at 'amount) (get-slot-tranche-amounts slot) ) )
  )

  (defun get-slot-amount:decimal
    (slot:string)
    @doc " Return the slot maximum amount "
    (at 'amount (read slots slot ['amount]))
    )

  (defun new-tranche:string
    ( account:string
      slot:string
      amount:decimal
      guard:guard
    )
    @doc " Prepare a new tranche and transfer the funds to the shared account "
    @model [ (property (valid-account-id account))]
  (let ((total (get-slot-total-amount slot)))
    (with-read slots slot
      {'amount := maximum }
      (enforce (>= maximum (+ amount total))
       "Tranche cannot be bigger than the remaining amount for the slot" )
      (with-default-read last-id-table ""
        { 'last-id : 0}
        { 'last-id := last }
      (let ((id: integer ( + last 1 )))
        (insert tranches (format "{}" [id]) {
            'account: account,
            'slot: slot,
            'amount: amount,
            'guard: guard,
            'status: "NEW"
            })
        (coin.transfer account slot amount)
        (write last-id-table "" {"last-id": id}))))))

  (defun get-slot-tranches
  (slot:string)
  @doc " Return trache amounts for slot "
  (select tranches [ 'account, 'slot, 'amount, 'guard, 'status ] (where 'slot (= slot))))

  (defun new-multibond:string
    ( slot:string )  ;; KDA account for multi/multi ID
    ;; debit from each tranche
    ;(map (debit-tranche account) (at 'tranches multi) )
    (let ((multi {
      'size: (get-slot-amount slot),
      'tranches: (get-slot-tranches slot)
      }))
      ;; store the multi
      (insert multis slot multi)
      ;; allow the autonomous transfer to relay bank
      (install-capability
        (coin.TRANSFER slot 'relay-bank (at 'size multi)))    ;; create the bond
      (test.pool.new-bond "kda-relay-pool" slot
        (create-module-guard slot))
      )
  )

  ; (defun debit-tranche (account:string tranche:object{tranche})
  ;   (coin.transfer-create
  ;     (at 'account tranche)
  ;     account
  ;     (create-module-guard "tranche")
  ;     (at 'amount tranche))
  ; )

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

(create-table last-id-table)
(create-table slots)
(create-table tranches)
(create-table multis)

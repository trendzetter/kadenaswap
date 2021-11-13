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

  (defcap RESERVE
   ( account:string
     amount:decimal)
   @doc "Reserve event for tranche reservation"
   @event true
  )

  (defconst POOL 'kda-relay-pool)

  (defschema last-id-schema
    last-id:integer)
  (deftable last-id-table:{last-id-schema})

  (use util.guards)

  (defschema tranche
    account:string     ;; account paying the tranche and receiving the rewards
    slot:string        ;; KDA account for the full bond
    amount:decimal     ;; tranche amount
    guard:guard        ;; keyset operator for running app
    status:string      ;; current status of the tranche
  )

  (defschema slot
    ;; key:  accountname for the autonomous controlled account
    amount:decimal     ;; total and max amount
    operator:guard     ;; keyset that will operator the app
    fee:decimal        ;; percentage operator fee
    bondId:string        ;; Id of the bond
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
    (insert slots account { 'amount: amount, 'operator: operator, 'fee: fee, 'bondId: "" })
    (coin.create-account account (create-module-guard 'reservations))
    (format "Slot {} added" [account])
  )

  (defun get-slot-tranche-amounts
  (slot:string)
  @doc " Return trache amounts for slot "
  (select tranches [ 'amount ] (where 'slot (= slot))))
; todo: get-remaining-amount
  (defun get-slot-total-amount:decimal
  (slot:string)
  @doc " Return to total amount of tranches "
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
  (with-capability (RESERVE account amount) 1
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
        (write last-id-table "" {"last-id": id})
        (format "Tranche {} reserved" [id])))))))

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
        (coin.TRANSFER slot 'relay-bank (at 'size multi)))

        ;; create the bond
      (update slots slot {
        'bondId: (test.pool.new-bond "kda-relay-pool" slot (create-module-guard "multibond"))
        }))

      (with-read slots slot
        { 'operator := operator,
          'bondId := bondId }
        (install-capability (test.pool.ROTATE bondId))
        (test.pool.rotate bondId operator)
        (format "{}" [bondId])))

  (defun renew-multibond:string (account:string)
    ;; track the old balance
    (let ( (old-balance (coin.get-balance account))
           (multi (read multis account))
           (slot (read slots account)))
      ;; renew, will credit account

      (install-capability (test.pool.BONDER (at 'bondId slot)))
      (test.pool.renew (at 'bondId slot))
      ;; compute new amount
      (let ( (amount (- (coin.get-balance account) old-balance)) )
        ;; allocate
        (map
          (allocate account amount (at 'size multi))
          (at 'tranches multi))))
  )

;testing

(defun test-test:string
  ( slot:string )
  (with-read slots slot
    { 'operator := operator,
      'bondId := bondId }
  (format "{} {}" [operator bondId])))

  (defun allocate
      ( account:string           ;; multi account
        amount:decimal           ;; total amount to allocate
        size:decimal             ;; bond size
        tranche:object{tranche}  ;; tranche
      )
      (format "{} {} {} {}" [account amount size tranche])
  )

  ; (defun allocate
  ;   ( account:string           ;; multi account
  ;     amount:decimal           ;; total amount to allocate
  ;     size:decimal             ;; bond size
  ;     tranche:object{tranche}  ;; tranche
  ;   )
  ;   (let ( (to (at 'account tranche))
  ;          ;; compute tranche amount
  ;          (tranche-amount (* amount (/ (at 'amount tranche) size))) )
  ;     (install-capability
  ;       (coin.TRANSFER account to tranche-amount))
  ;     (coin.transfer account to tranche-amount))
  ; )
)

(create-table last-id-table)
(create-table slots)
(create-table tranches)
(create-table multis)

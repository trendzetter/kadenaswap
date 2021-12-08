(namespace (read-msg 'ns))

(module delegated-bonding GOVERNANCE
  @doc " slots for shared bonds"
  @model
  [ (defproperty valid-account-id (account-id:string)
      (and
        (>= (length account-id) 3)
        (<= (length account-id) 256))) ]

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'delegated-bonding-admin))
  )

  (defcap RESERVE
   ( account:string
     size:decimal)
   @doc "Reserve event for tranche reservation"
   @event true
  )

  (defcap TRANCHE_GUARD
    ( tranche-id:string )
    (enforce-guard (at 'guard (read tranches tranche-id ['guard])))
  )

  (defcap OPERATOR
    ( slot:string )
    (enforce-guard (at 'operator (read slots slot ['operator])))
  )

  (defconst POOL 'kda-relay-pool)

  (use util.guards)

  (defschema tranche
    account:string     ;; account paying the tranche and receiving the rewards
    slot:string        ;; KDA account for the full bond
    size:decimal     ;; tranche amount
    guard:guard        ;; keyset operator for running app
    status:string      ;; current status of the tranche
  )

  (defschema slot
    ;; key:  accountname for the autonomous controlled account
    size:decimal     ;; total and max amount
    operator:guard     ;; keyset that will operator the app
    operator-account:string
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

  (defun get-all-slots ()
  @doc " Return all slots. "
  (format "{}:{}" [(map (read slots) (keys slots)) (keys slots)] ))

  (defun get-all-tranches ()
  (map (read tranches) (keys tranches)))

  (defun new-slot:string
    ( account:string
      size:decimal
      operator-account:string
      operator:guard
      fee:decimal
    )
    ;; check valid-account
    @model [ (property (valid-account-id account)), (property (valid-account-id operator-account))]
    (insert slots account { 'size: size, 'operator-account: operator-account, 'operator: operator, 'fee: fee, 'bondId: "" })
    (coin.create-account account (create-module-guard 'reservations))
    (format "Slot {} added" [account])
  )

  (defun get-slot-tranche-sizes
  (slot:string)
  @doc " Return trache sizes for slot "
  (select tranches [ 'size ] (where 'slot (= slot))))
; todo: get-remaining-size
  (defun get-slot-total-size:decimal
  (slot:string)
  @doc " Return to total size of tranches "
    (fold (+) 0.0 (map (at 'size) (get-slot-tranche-sizes slot) ) )
  )

  (defun get-slot: {slot}
    (slot:string)
    @doc " returns the requested slot "
    (with-read slots slot
      { 'size := size,     ;; total and max amount
        'operator := operator,     ;; keyset that will operator the app
        'operator-account := operator-account,
        'fee := fee,        ;; percentage operator fee
        'bondId := bondId }
  ;; return slot
      (format "{} {} {} {} {}" [size operator operator-account fee bondId])
    ))

  (defun new-tranche:string
    ( account:string
      slot:string
      size:decimal
      guard:guard
    )
    @doc " Prepare a new tranche and transfer the funds to the shared account "
    @model [ (property (valid-account-id account))]
  (with-capability (RESERVE account size) 1
  (let ((total (get-slot-total-size slot)))
    (with-read slots slot
      {'size := maximum,
       'operator-account := operatoraccount }
      (enforce (!= account operatoraccount)
       "Operator account cannot join the bond" )
      (enforce (>= maximum (+ size total))
       "Tranche cannot be bigger than the remaining size for the slot" )
      (let ((id: string (format "{}:{}" [slot account])))
        (insert tranches id {
            'account: account,
            'slot: slot,
            'size: size,
            'guard: guard,
            'status: "NEW"
            })
        (coin.transfer account slot size)
        (format "{}" [id]))))))

  (defun get-slot-tranches
  (slot:string)
  @doc " Return trache sizes for slot "
  (select tranches [ 'account, 'slot, 'size, 'guard, 'status ] (where 'slot (= slot))))

  (defun new-multibond:string
    ( slot:string )  ;; KDA account for multi/multi ID
    (with-capability (OPERATOR slot) 1
    (with-read slots slot
      {'size := size}
    (let ((multi {
      'size: size,
      'tranches: (get-slot-tranches slot)
      }))
      ;; store the multi
      (insert multis slot multi)
      ;; allow the autonomous transfer to relay bank
      (install-capability
        (coin.TRANSFER slot 'relay-bank (at 'size multi)))

        ;; create the bond
      (let ((bondId: string (test.pool.new-bond "kda-relay-pool" slot (create-module-guard "multibond"))))
      (update slots slot {
        'bondId: bondId
        })

      ;; Rotate operation to the operator
      (rotate slot)
      (format "{}" [bondId]))))))

  (defun renew-multibond:string (account:string)
    ;; track the old balance
    (let* ( (old-balance (coin.get-balance account))
           (multi (read multis account))
           (slot (read slots account))
           (bondId (at 'bondId slot)))
      ;; renew, will credit account
      (test.pool.renew (at 'bondId slot))
      ;; compute new size
      (let* ( (amount (- (coin.get-balance account) old-balance))
             (operator-fee (* (/ amount 100) (at 'fee slot)))
             (rewards (- amount operator-fee))
             (operator-account (at 'operator-account slot)) )
        ;; allocate
        (install-capability (coin.TRANSFER account operator-account operator-fee))
        (coin.transfer-create account operator-account (at 'operator slot) operator-fee)
        (map
          (allocate account rewards (at 'size multi)) (at 'tranches multi)))
    ))


  (defun rotate
    ( slot:string)
    (with-capability (OPERATOR slot) 1
    (with-read slots slot
      { 'operator := operator,
        'bondId := bondId }
      (install-capability (test.pool.ROTATE bondId))
      (test.pool.rotate bondId operator)
      (format "operator:{}" [operator]))))

  ; wrapping this call allows for reselling the tranches
  (defun rotate-tranche
    (tranche-id:string
     new-guard:guard )
    (with-capability (TRANCHE_GUARD tranche-id) 1
      (update tranches tranche-id {'guard: new-guard}))
    )

  (defun update-tranche-account
    (tranche-id:string
     account:string )
     @model [ (property (valid-account-id account))]
     (with-capability (TRANCHE_GUARD tranche-id) 1
       (update tranches tranche-id {'account: account})))

; idea: vote to unbond. If 60% of the tranches want to unbond the operator cannot renew

  (defun unbond
    (slot:string)
    (with-capability (OPERATOR slot) 1
    (with-read slots slot
      { 'bondId := bondId,
        'size := size,
        'fee := operator-fee,
        'operator-account := operator-account,
        'operator := operator-guard }
      (let* ( (old-balance (coin.get-balance slot))
              (multi (read multis slot)))
        (test.pool.unbond bondId)
        (let* ( (amount (- (coin.get-balance slot) old-balance))
                (fees (- amount size))
                (operator-fee (* (/ fees 100) operator-fee))
                (rewards (- amount operator-fee)) )
          ;; allocate
          (if (> operator-fee 0.0)
            (let ((msg "p"))
              (install-capability (coin.TRANSFER slot operator-account operator-fee))
              (coin.transfer-create slot operator-account operator-guard operator-fee)
            )
            "No operator payment"
          )
          (map
            (allocate slot (- amount operator-fee) (at 'size multi)) (at 'tranches multi)))
        ))))

  (defun allocate
    ( account:string           ;; multi account
      amount:decimal           ;; total amount to allocate
      size:decimal             ;; bond size
      tranche:object{tranche}  ;; tranche
    )
    (with-capability (OPERATOR account) 1
    (let ( (to (at 'account tranche))
           ;; compute tranche amount
           (tranche-amount (* amount (/ (at 'size tranche) size))) )
       (install-capability
         (coin.TRANSFER account to tranche-amount))
       (coin.transfer account to tranche-amount)
      (format "{}:{}" [to tranche-amount]))) ))

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table slots)
    (create-table tranches)
    (create-table multis) ])

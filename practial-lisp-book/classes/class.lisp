(defparameter *account-number* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "customer-name must be specified.")
    :accessor customer-name
    :documentation "The name of the customer.")
   (balance
    :initarg :balance
    :initform .0
    :reader balance
    :documentation "Current balance of the customer.")
   (account-number
    :initform (incf *account-number*)
    :reader account-number
    :documentation "Unique ID of the customer in the bank.")
   (account-type
    :reader account-type
    :documentation "Type of the account, can be :gold, :silver or :bronze.")))

(defmethod initialize-instance :after ((account bank-account) &key)
  (with-slots (balance account-type) account
    (setf account-type (cond
                         ((>= balance 100000) :gold)
                         ((>= balance 50000) :silver)
                         (t :bronze)))))

(defclass checking-account (bank-account)
  ((debit
    :initarg :debit
    :initform .0
    :accessor debit
    :documentation "Debit of the account.")
   (debit-transaction
    :initform 0
    :accessor debit-transaction
    :documentation "Number of times the account require a debit.")))

(defclass saving-account (bank-account) ())

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the given amount from account balance. Signal an
  error if balance is less than amount."))

(defgeneric deposit (account amount)
  (:documentation "Increments balance account by the given amount. Signal an
  error if deposit is less or equal to 0."))

(defmethod withdraw ((account bank-account) amount)
  (with-slots (balance) account
    (when (< balance amount)
      (error "Insufficient balance."))
    (setf balance (- balance amount))))

(defmethod withdraw :before ((account checking-account) amount)
  (with-slots (balance debit) account
    (let ((dif (- amount balance)))
      (when (plusp dif)
        (request-debit account dif)
        (incf balance amount)))))

(defmethod request-debit ((account checking-account) amount)
  (with-accessors ((debit debit)
                   (debit-transaction debit-transaction))
      account
    (incf debit amount)
    (incf debit-transaction)
    (when (zerop (mod debit-transaction 5))
      (incf debit (* debit .05)))))

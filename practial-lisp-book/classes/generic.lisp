;;; methods belong to a generic function and not to a class or object

;;; the hierarchy in these examples is bank-account superclass, and
;;; then checking-account and savings-account as subclasses.

(defparameter *account-number* 0)

(defclass bank-account () ; no direct superclass, implicit to STANDARD-OBJECT
  ;; slot, i.e. fields
  ((customer-name
    :initarg :customer-name ; used as key in MAKE-INSTANCE
    :initform (error "Name must be supplied.")
    ;; :accessor customer-name ; condenses reader and writer
    :reader customer-name
    ;; writer creates the setf method
    :writer (setf customer-name)
    :documentation "The complete name of the customer.")
   (balance
    :initarg :balance
    :initform 0.0
    ;; reader creates the getter for this method
    :reader balance
    :documentation "The current balance of the account.")
   (account-number
    :initform (incf *account-number*)
    :documentation "Incremental number to uniquely identify the account in the bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, :bronze.")))

(defclass checking-account (bank-account)
  ;; no slot, they're inheritated
  ())

(defclass saving-account (bank-account)
  ;; no slot, they're inheritated
  ())

;; accessor function
(defgeneric account-number (account)
  (:documentation "Getter for slot account-number."))

(defmethod account-number ((account bank-account))
  (slot-value account 'account-number))

;; INITIALIZE-INSTANCE is a way to init some slots that depend on other object
;; values that cannot be know statically. It's better to use :after and let the
;; main implementation do the init for :initarg and :initform.
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

(make-instance 'bank-account :customer-name "Mister Bob"
                             :balance 1200.0)
;; implicit balance to 0
(make-instance 'bank-account :customer-name "Mister Bob")


(defparameter *b-acc* (make-instance 'bank-account))
;; access slots via SLOT-VALUE, which is SETFable
(setf (slot-value *b-acc* 'customer-name) "John Pippo")
(setf (slot-value *b-acc* 'balance) 1000.0)

(slot-value *b-acc* 'customer-name)

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the amount from account's balance.
Signal an error if balance is less than amount."))

;; specialize the first parameter on a specific object.
;; The second parameter is indirectly assigned to T.
(defmethod withdraw ((account bank-account) amount)
  ;; balance is a method of account, take it as granted
  (when (< (balance account) amount)
    (error "Insufficient credits."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((dif (- amount (balance account))))
    ;; the account can't cover the difference
    (when (plusp dif)
      ;; take overdraft-account as granted, return a bank-account
      (withdraw (overdraft-account account) dif)
      (incf (balance account) dif)))
  ;; this call the next withdraw method in the chain of specialized methods.
  ;; Sort of super in Java.
  (call-next-method))

;; EQL specializes the function on a particual object
(defmethod withdraw :before ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      ;; assume embezzle and *bank* to be a method and the istance of
      ;; the entire bank
      (incf (balance account) (embezzle *bank* overdraft)))))

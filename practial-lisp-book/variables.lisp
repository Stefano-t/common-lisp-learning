(defparameter *fn*
  (let ((count 0))
    ;; creates a closure, where the function has access to the binding
    ;; of the variable, so it can do whatever it wants to do with it.
    ;; The binding will persist until a reference to the closure
    ;; exists.
    #'(lambda () (setf count (1+ count)))))

;; dynamic (special) bindings
(defvar *x* 10)
(defun foo ()
  (format t "Before increment: ~a~%" *x*)
  (setf *x* (1+ *x*)) ; changes dynamic bindings
  (format t "After increment: ~a~%" *x*))

(defun bar ()
  (foo) ; ordinary call to FOO
  (let ((*x* 20)) (foo)) ; here *x* is rebound to 20 only in this scope
  (foo)) ; *x* here returns to the preovious value

;; defines a new costant, which can't be rebound.
(defconstant +const+ 1234 "My costant")

;; ROTATEF swaps two variables, and return NIL
(let ((x 10)
      (y 20))
  (format t "Before rotate: x=~a, y=~a~%" x y)
  (rotatef x y)
  (format t "After rotate: x=~a, y=~a~%" x y))

;; SHIFTF shifts elements from left to right, and return the first
(let ((x 10)
      (y 20))
  (format t "Before shift: x=~a, y=~a~%" x y)
  (shiftf x y 50) ; x = y, y = 50, return old x
  (format t "After shift: x=~a, y=~a~%" x y))

;; INCF and DECF are shorthands for ++ and --, and they use SETF internally
(let ((x 10)
      (y 20))
  (format t "Increment x: ~a~%" (incf x))
  (format t "Decrement y: ~a~%" (decf y)))

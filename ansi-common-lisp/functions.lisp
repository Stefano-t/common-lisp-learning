;;; setf
;; to make some function setf-able, it necessary to create a defun
;; with name of the form (setf function-name) and the first parameter
;; is the value passed to setf, the rest are all the argument for
;; function-name.

(defun primo (lst)
  (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(let ((x '(1 2 3)))
  ;; now primo is setf-able
  (setf (primo x) 69)
  x)

;;; DOCUMENTATION

(documentation '+ 'function)

;;; local function

(labels ((add10 (x) (+ x 10))
         (consa (lst) (cons 'a lst)))
  (consa (add10 59)))
;; here add10 and consa do not exist

;; labels creates function that are visibile during the definition
(labels ((len (lst)
           (if (null lst)
               0
               (+ 1 (len (cdr lst))))))
  (len '(1 2 3 4)))


;;; rest, optional, key

;; &rest collects the remaining arguments in a list

(defun try-rest (r1 &rest rest)
  (list r1 rest))

(defun our-funcall (fn &rest args)
  (apply fn args))

(defun say-true (thing &optional (fact 'fun))
  (list thing 'is fact))

(say-true 'play) ;=> uses 'fun as default
(say-true 'play 'boring) ;=> sets `fact parameter

(defun keylist (a &key x y (z 'default))
  (list a x y z))

(keylist 1 :x 'a) ; => (1 a nil default)
(keylist 1 :y 3 :x 9) ; => (1 9 3 default)

;; collect key arguments in rest, and pass them to another function
(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))


;;; closures

;; we can return function as well

(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
         args))

;; the context where a function is created continues to be valid
;; as long as the function refers to it
(defun make-adder (n)
  ;; n is a FREE VARIABLE
  #'(lambda (x) (+ x n)))

(setf add3 (make-adder 3))
(funcall add3 10) ; => 13
(funcall add3 4) ; => 7
(setf add10 (make-adder 10))
(funcall add10 10) ; => 20
(funcall add10 4) ; => 14

;; multiple functions can share the same free variable
(let ((counter 0))
  (defun reset-counter ()
    (setf counter 0))
  (defun stamp-counter ()
    (setf counter (+ 1 counter))))

(list (stamp-counter) (stamp-counter)
      (stamp-counter) (reset-counter) (stamp-counter)) ;=> (1 2 3 0 1)

(defun our-complement (fn)
  ;; fn is a free variable
  #'(lambda (&rest args)
      (not (apply fn args))))

;;; lexical and special scope

;; lexical scope define a variable and bound it to the value it referes to
(let ((x 10))
  (defun foo ()
    x))

(foo) ; prints 10

(let ((x 20))
  (foo)) ; prints always 10

;; dynamic variable refers to the variable defined in the enclosing environment
(let ((x 10))
  (defun foo-dynamic ()
    ;; `declare special` changes the behaviour of the program
    (declare (special x))
    x)) ; here x is not defined, since the enclosing x is lexical and not special

(defparameter x 10) ; here x is dynamic

(foo-dynamic) ; prints 10

(let ((x 20))
  (foo-dynamic)) ; prints 20

(foo-dynamic) ; prints 10

;; some built-in dynamic variable
(let ((*print-base* 16))
  (princ 32))
;; here *print-base* returns to the default value

;;; compilation

(defun my-first (lst)
  (car lst))

(compiled-function-p #'my-first)

;; also this kind of functions (the one defined in a different lexical
;; scope) can be compiled
(let ((counter 0))
  (defun reset-counter ()
    (setf counter 0))
  (defun stamp-counter ()
    (setf counter (+ 1 counter))))

;; compile a file, all the functions in it will pass `compile-function-p'
(compile-file "./high-order-fn.lisp")

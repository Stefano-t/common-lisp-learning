;; PROGN creates a block where all forms are executed in turn
(if (= 0 0)
    (progn
      (format t "zero is equal to zero~%")
      (format t "and this is another sentence in the same block")))

;; the previous construct is the WHEN macros, defined as
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

;; the contrary of WHEN is UNTIL
(defmacro my-until (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; COND macro
(defun sign (x)
  (cond
    ((> x 0) 1)
    ((< x 0) -1)
    ;; T is used as else
    (t (format t "the value is zero: ~a~%" x)
       0))) ;; each clause has a default PROGN

;; DOTIMES is a counting loop
(dotimes (x 10) (print x))

;; DOLIST loops over each element of the list
(dolist (x '(1 2 3 4)) (print x))

(dolist (x '(1 2 3 4))
  (print x)
  (when (evenp x)
    (return))) ; RETURN breaks the loop

;; DO macro of the form:
;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   statement*)
;;
;; where each variables has the form:
;; (var init-form step-form)

(defun fibonacci (n)
  "Computes the n-th Fibonacci number"
  ;; the update step is performed for all variables before assign them
  (do ((i 0 (1+ i))
       (cur 0 next)
       (next 1 (+ cur next))) ; variable form
      ((>= i n) cur) ; test form
    )) ;; no body of do

;; LOOP macro

(loop for x from 1 to 10 summing (expt x 2))
;; collect in a list numbers from 1 to 10
(loop for i from 1 to 10 collecting i)
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou")) ; counts vowels in the string
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return  a)) ;; 10th fib number

;;; MACRO DEFINITION
;;; 1. Write a sample call to the macro and the code it should expand into, or vice versa.
;;; 2. Write code that generates the handwritten expansion from the arguments in the sample call.
;;; 3. Make sure the macro abstraction doesn't "leak."

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; (var start end) destruct the parameter list
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym))) ; GENSYM creates a unique symbol name
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          ;; assigning this variable prevents further evaluation
          (,ending-value-name ,end)) ; evaluates variable in order
         ((> ,var ,ending-value-name))
       (progn
         ,@body))))

(do-primes (p 0 10)
  (format t "~a " p)
  (format t "~a " p))

(defmacro do-fib ((var end) &body body)
  (let ((i-name (gensym))
        (cur-name (gensym))
        (end-name (gensym)))
    `(do ((,i-name 0 (1+ ,i-name))
          (,cur-name 0 ,var)
          (,var 1 (+ ,cur-name ,var))
          (,end-name ,end))
         ((>= ,i-name ,end))
       (progn ,@body))))


(do-fib (p 10)
  (format t "~a " p))


(defmacro with-gensym (symbol-list &body body)
  `(let (,@(loop for s in symbol-list collect `(,s (gensym))))
     ,@body))

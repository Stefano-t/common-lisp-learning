(progn
  (format t "a")
  (format t "b")
  (+ 11 22)) ; returns the last value


;; block creates an exit point with a symbol, and we can exit the body by
;; return-from and the name of the block name
(block head
  (format t "This will be printed!~%")
  (return-from head 'exit) ; return value for block
  (format t "Never be printed!~%"))

;; return implicit returns from a nil named block
(block nil
  (return 10)) ; this is a macro

;; operators with body have an implicit nil block
(dolist (x '(a b c d e f))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))

;; functions has a block with the function name
(defun read-integer (str)
  (let ((acc 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
            (setf acc (+ (* acc 10) i))
            (return-from read-integer nil))))
    acc))

;; `tagbody` create a label to use with `go`
(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~A " x)
   (if (< x 10) (go top)))

;;; let

;; `let*` is equivalent to nested `let`
(let* ((x 1)
       (y (+ 1 x)))
  (+ x y))

;; `destructuring-bind` takes a pattern to match the argument
;; `. z` takes the rest of the input as list
(destructuring-bind (w y . z) '(a (b c) d e f)
  (list w y z))

(destructuring-bind (w (x y) . z) '(a (b c) d e f)
  (list w x y z))

;;;  cond
;; `cond` has an implicit progn after the test clause
(defun our-member (obj lst)
  (cond ((atom lst) nil)
        ((eql (car lst) obj) lst)
        (t (our-member obj (cdr lst)))))

;; by default, returns the value of the evaluated condition if no
;; expression follows
(cond (69))

;;; case
;; case compares with eql
(defun day-of-month (month)
  (let ((leap-year nil))
    (case month
      ((jan mar may jul aug oct dec) 31)
      ((apr jun sep nov) 30)
      (feb (if leap-year 29 28))
      (otherwise "unknown month"))))

;; typecase uses typep to compare the type of x
(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

;;; DO

(let ((x 1))
  (do ((x x (+ x 1))
       (y x x)) ; update refers to the value of the previuos iteration
      ((> x 5) x)
    (format t "(~a, ~a) " x y)))

(let ((x 1))
  ;; do* binds sequencially
  (do* ((x x (+ x 1))
        (y x x))
       ((> x 5) x)
    (format t "(~a, ~a) " x y)))

(dolist (x '(a b c d e) 'done)
  (format t "~A " x))

(dotimes (i 5 i)
  (format t "~a " i))

;; mapc doens't cons a new list, so it's only for side-effect.
;; better than dolist because works with multile lists
(mapc (lambda (x y) (format t "~a ~a~%" x y))
      '(hip flip slip)
      '(hop flop slop)) ; returns the last argument


;;; multiple values

;; returns all the values
(values 1 'a nil (+ 4 7))

(let ((x (values 1 2))) ; values not bound are discarded
  x)

(let ((x (values))) ; default to NIL
  x)

;; binds all the values returned
(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))

;; exceeding variables are set to nil
(multiple-value-bind (x y z) (values 1 2)
  (list x y z))

(defun print-time ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format t "~a:~a:~a~%" h m s)))

;; applies function to multiple arguments
(multiple-value-call #'+ (values 1 2 3))

;; same as before, but with function #'list implied
(multiple-value-list (values 1 2 3))

;;; catch - throw

(defun super ()
  (catch 'abort
    (sub)
    (format t "Never called~%"))
  (format t "Called~%"))

(defun sub ()
  ;; goes to the first cathing block with the same tag
  (throw 'abort 69))

(defun toplevel ()
  ;; directly goes to Lisp error handler
  (error "Raise error!!")
  (format t "After the error~%"))

;; unwind-protect performes clean-up expression

(progn
  (setf x 1)
  (catch 'abort
    (unwind-protect
         (throw 'abort 69)
      (setf x 2)))
  x) ; => 2

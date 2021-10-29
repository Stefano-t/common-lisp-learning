;;; LIST as CONS of cells

;; dotted-list
(defparameter *l* (cons 1 2))
;; list
(setf *l* (cons 1 (cons 2 nil)))

;; CAR = FIRST
(car *l*)
(first *l*)

;; CDR = REST
(cdr *l*)
(rest *l*)

;; cdr and car SETF places
(setf (rest *l*) (list 2 3 4))

;; can contain different types
(list "foo" 'bar 1 2)

;; APPEND conses the last argument
(let* ((l1 '(1 2))
       (l2 '(3 4))
       (la (append l1 l2)))
  (format t "~a~%" la)
  (setf (car l2) 'a)
  (format t "~a~%" la))

(defparameter *l1* (loop for i upto 10 collect (random 10)))

(mapcan #'(lambda (x) (when (> x 5) (list x))) *l1*)
;; same as before
(remove-if-not #'(lambda (x) (> x 5)) *l1*)

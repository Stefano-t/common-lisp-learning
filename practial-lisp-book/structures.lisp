;;; TREE

(defparameter *t* (list '(1 2) 3 '(4 (5 6))))

(mapl #'(lambda (x) (format t "~a~%"
                            (if (consp x)
                                (car x)
                                x)))
      *t*) ; prints the subtree

(mapl #'(lambda (x)
          (format t "~a - ~a~%" (car x) (cdr x)))
      *t*) ; prints the root of the subtree and the rest to be read

;;; SET

(defparameter *s* ())
(adjoin 1 *s*) ; no-destructive add
(pushnew 1 *s*) ; destructive add
(pushnew 2 *s*)
(pushnew 2 *s*) ; no add


;;; ALIST
(defparameter *al* ())
;; fill an alist from two given lists
(setf *al* (pairlis '(a b c) '(1 2 3)))

(acons 'a 1 *al*)
;; same as before
(cons (cons 'a 1) *al*)

(setf *al* (acons 'a 1 *al*))
(setf *al* (acons 'b 2 *al*))

;; lookup

(assoc 'a *al*) ; (A . 1)
(cdr (assoc 'b '((a . 1) (b . 2) (c . 3)))) ; 2

;;; PLIST
;; uses EQ as test equality
(defparameter *pl* '(:a 1 :b 2 :c 3))
(getf *pl* :c) ; 3
(setf (getf *pl* :c) 10) ; are SETFable


(destructuring-bind (a b &rest rest) '(1 2 3 4)
  (format t "~a ~a" a b))

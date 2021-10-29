(vector 1 2 3) ; cretes vector of fixed-size
#(1 2 3) ; same vector but liteal form

;; creates a vector of size 5, with all elements set to 1
(make-array 5 :initial-element 1)

;; resizable vector
(defparameter *x* (make-array 3 :fill-pointer 0))

;; function to add element to resizable vect, return the new
;; fill-pointer
(vector-push 1 *x*)
(vector-push 'a *x*)
(vector-push #\b *x*)

;; return most recenet pushed argument
(vector-pop *x*) ; => #\b

;; make adjustable array
(defparameter *x-ad* (make-array 3 :fill-pointer 0 :adjustable t))

;; automatic adjust the size of the array, this method can be used
;; only when we have a fill-pointer
(vector-push-extend 'a *x-ad*)


;;; specialize a vector to faster access

;; hold only character (same as string, but this time adjustable)
(make-array 5 :fill-pointer 0
              :adjustable t
              :element-type 'character)

;; length
(length #(1 2 3)) ; => 3
(length (make-array 5 :fill-pointer 0)) ; => 0

;; ELT gets the element
(elt #(1 2 3) 1) ; => 2
;; ELT is settable
(defparameter *v* (make-array 5 :initial-element 1))
(setf (elt *v* 1) 4)
*v* ; => #(1 4 1 1 1)

(defun verbose-search (elem)
  "Custom function to use as :key"
  (format t "Searching ~a~%" elem)
  (first elem))

(find 'a '((a . 1) (b . 2) (c . 3)) :key #'first)

(count #\a "ciao come stai?" :test #'char=)

(find 'a '((a . 1) (b . 2) (c . 3)) :key #'verbose-search
                                    :from-end t)

(find 'a '((a . 1) (b . 2) (c . 3)) :key #'verbose-search)

(substitute #\A #\a "blablabla" :test #'char=)

(every #'> #(10 20 30 40) #(1 2 3))
(some #'< #(10 20 30 40) #(1 2 3))
(notany #'< '(10 20 30 40) '(1 2 3))

(map 'list #'list #(1 2 3) #(a b c d))
(map 'list (let ((i 0)) (lambda (x) (incf i) (list i x))) #(a b c))
(map 'list (lambda (x) x) #(a b c))

(remove-if-not #'alpha-char-p
  #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))

;; &key (del ",") (output-type 'string)
(defun join (seq &key (out-type 'string) (del ","))
  (reduce #'(lambda (x y) (concatenate out-type x del y)) seq))

;;; hash table

(defparameter *h* (make-hash-table)) ; creates and empty table
(gethash 'foo *h*) ; returns value and presence
(setf (gethash 'foo *h*) 'foo-value)
(setf (gethash 'bar *h*) 'bar-value)
(remhash 'bar *h*)


(defun show-value (key hash-table)
  ;; binds multiple returned values
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format t "Key ~a present with value ~a~%" key value)
        (format t "Key ~a not present" key))))

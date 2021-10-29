;;; VECTOR

;; two-dimensional array
(setf x (make-array '(2 2) :element-type 'fixnum
                           :initial-element 0))

;; general function to access array
(aref x 0 0)
(setf (aref x 0 0) 1)

;; simple vector is a vector of a single dimension
(let ((vec (make-array 5 :initial-element nil)))
  (format t "~a~%" vec)
  ;; specialized function for simple vector
  (svref vec 0)
  (setf (svref vec 0) 10)
  (format t "~a~%" vec))

;;; STRING AND CHAR

(let ((str "elbow") ; string type
      (c #\a))      ; char type
  (format t "~a~%" (sort str #'char<))
  ;; ``char`` is the specialized function to access an element in string
  (format t "~a~%" (char str 1))
  ;; string-equal is case-insensitive
  (format t "~a~%" (string-equal "elbow" "ElBow")))

;; nil destination means return a string
(format nil "~a" '(1 2 3))
(concatenate 'string "ciao " "come " "stai?")

;;; SEQUENCES

(position #\a "cane")
(position 'a '((b c) (a d)) :key #'car)
(position 3 '(1 0 7 4 9) :test #'<)

(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))

(find #\a "cat")
(find-if #'characterp "cat")

(remove-duplicates "abracadabra")

;; reduce is useful when you want to extend the domain of a function
;; to multiple arguments
(reduce #'union '((a b c) (a d e) (e f g a)))


;;; STRUCTURE

;; defstruct creates functions for you: copy-objname, make-objname,
;; objname-p and also accessor functions.

;; :print-object is the function to print it
(defstruct (point (:print-object print-point))
  ;; if there isn't a default value, remove paranthesis
  (x 0) ; argument name and default value
  (y 0))

;; p is the object to print, stream is where to print
(defun print-point (p stream)
  (format stream "#<~a, ~a>" (point-x p) (point-y p)))

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~a>" (node-elt n)))))
  elt
  (l nil)
  (r nil))



;;; HASH TABLE

(setf table (make-hash-table))

;; returns two values: the value associated with key and if present or not
(gethash 'color table)

(setf (gethash 'color table) 'red)
(gethash 'color table)

;; returns T when key is removed
(remhash 'color table)

(setf (gethash 'shape table) 'sphere
      (gethash 'size table) 'giant
      (gethash 'color table) 'blue)

;; maphash iterate over an hash table by key-value
(maphash (lambda (k v) (format t "~A: ~A~%" k v))
         table)

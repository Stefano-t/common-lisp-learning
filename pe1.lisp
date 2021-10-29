;;; es 1
(defun multiple-of-3-5 (bound)
  (loop for i from 3 below bound
        when (or (zerop (mod i 3)) (zerop (mod i 5)))
          summing i))
;;; es 2
(defun sum-fib (bound)
  (loop for i = 1 then j
        and j = 1 then (+ i j)
        while (< j bound)
        when (evenp j) sum j))

;;; es 3

(defun prime-factors (n)
  (do ((n n) (only-odd nil) (d 2) (factors ()))
      ((>= 1 n) factors)
    (if (zerop (rem n d))
        (progn
          (push d factors)
          (setf n (/ n d)))
        (progn
          (unless only-odd
            (decf d)
            (setf only-odd t))
          (setf d (+ d 2))))))

;; es 4

;; (defun power (n)
;;   (loop for base = -1 then (1- base)
;;         and x = n then (* (expt 10 base) x)
;;         when (< x 10)
;;           return (abs base)))

;; (defun palindrome-p (n)
;;   (do ((x n) (rev 0) (step (1- (power n)) (1- step)))
;;       ((zerop x) (values rev (= n rev)))
;;     (incf rev (* (expt 10 step) (mod x 10)))
;;     (setf x (truncate (/ x 10)))))

(defun palindrome-p (n)
  (let ((s (princ-to-string n)))
    (string= s (reverse s))))

(defun largest-palindrome ()
  (do ((n1 999)
       (n2 999)
       (palindrome 0))
      ((= n1 n2 100) palindrome)
    (when (palindrome-p (* n1 n2))
      (setf palindrome (max palindrome (* n1 n2))))
    (decf n2)
    (when (= n2 99)
      (setf n2 (decf n1)))))

;; es 5

(defun smallest-divisible ()
  (do ((div 20)
       (number 40))
      ((> 1 div) number)
    (if (zerop (rem number div))
        (decf div)
        (progn (incf number 20) (setf div 20)))))

;; es 6

(defun es6 (number)
  (- (expt (* 1/2 (* (1+ number) number)) 2)
     (loop for i from 1 to number summing (* i i))))

;; es 7

;; TODO: change test, too expansive

;; (defun factorial (n)
;;   (do ((prod 1 (* prod limit))
;;        (limit n (1- limit)))
;;       ((> 1 limit) prod)))

;; (defun wilson-p (n)
;;   (= 1 (mod (factorial (- n 2)) n)))

(defun prime-p (n)
  (loop with limit = (truncate (sqrt n))
        for i from 2 to limit
        when (> (gcd n i) 1)
          do (return-from prime-p nil))
  t)

(defun es7 (n)
  (do ((i 2 (if odd-p (+ 2 i) (1+ i)))
       (odd-p nil t) (ith 1 (if (prime-p i) (1+ ith) ith)))
      ((= ith n) i)))

;; es 8

(let ((last 0))
  (defun add-value (value seq &optional (size 13))
    (setf (aref seq last) value)
    (setf last (mod (1+ last) size)))
  (defun reset ()
    (setf last 0)))

(defun check-max (seq max-found)
  (let ((max (reduce #'* seq)))
    (when (> max (first max-found))
      (setf (first max-found) max)
      (setf (rest max-found) (copy-seq seq)))))

(defun es8 ()
  (let ((seq (make-array 13 :initial-element 0))
        (max-found (list 0 '())))
    (with-open-file (in "number.txt" :element-type '(integer 0 9))
      (loop for i = (read-byte in nil nil)
            while i
            do (progn
                 (add-value i seq)
                 (check-max seq max-found))))
  max-found))

;; es 9

(defun reduce-with-sieve (limit)
  (flet ((reduce-with-index (seq)
           (loop for i from 2 below (length seq)
                 when (plusp (bit seq i))
                   sum i)))
    (let ((bool-array (make-array (+ 1 limit) :element-type 'bit
                                              :initial-element 1)))
      (setf (bit bool-array 0) 0
            (bit bool-array 1) 0)
      (do ((final-value (truncate (sqrt limit)))
           (i 2 (position 1 bool-array :start (1+ i))))
          ((< final-value i) (reduce-with-index bool-array))
        (loop for j = (* i i) then (+ j i)
              until (> j limit)
              do (setf (bit bool-array j) 0))))))

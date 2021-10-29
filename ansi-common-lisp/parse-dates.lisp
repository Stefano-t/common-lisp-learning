(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        ;; find the first non matching char in the string
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

;; returns T if c is a printable char excluding whitespace
(defun constituent-p (c)
  (and (graphic-char-p c)
       (not (char= #\  c))))

(defun parse-date (str)
  (let ((toks (tokens str #'constituent-p 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

(defconstant +month-names+
  ;; literal vector
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str +month-names+
                     :test #'string-equal)))
    (if p
        (1+ p)
        nil)))

;; naive implementation of `parse-integer'
(defun read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))))
        accum)
      nil))

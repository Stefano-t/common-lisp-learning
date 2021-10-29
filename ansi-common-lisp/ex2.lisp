(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~a ~a~%" i (* i i))))

(defun fourth-elem (l)
  (car (cdr (cdr (cdr l)))))

(defun greater (x y)
  (if (> x y)
      x
      y))

(defun nested? (lst)
  "Returns true iff lst contains a list as element."
  (if (null lst)
      nil
      (or (listp (car lst)) (nested? (cdr lst)))))

(defun print-dots-it (times)
  "Prints times dot."
  (do ((i 0 (+ i 1)))
      ((> i times) (format t "~%"))
    (format t ".")))

(defun print-dots-rec (times)
  "Prints times dot."
  (if (< times 1)
      (format t "~%")
      (progn
        (format t ".")
        (print-dots-rec (- times 1)))))

(defun count-a (lst)
  "Counts how many times the symbol 'a appears in the list."
  (let ((times 0))
    (dolist (elt lst)
      (if (eql 'a elt)
          (setf times (+ 1 times))))
    times))

(defun count-a-rec (lst)
  "Counts how many times the symbol 'a appears in the list."
  (if (null lst)
      0
      (if (eql 'a (car lst))
          (+ 1 (count-a-rec (cdr lstp)))
          (count-a-rec (cdr lst)))))

(union '(a b c) '(b a d))

;; preserve the order of the two lists
(defun new-union (s1 s2)
  (if (null s2)
      s1
      (let ((elt (car s2)))
        (if (member elt s1)
            (new-union s1 (cdr s2))
            (new-union (append s1 (list elt)) (cdr s2))))))


(defun occurences (lst)
  (sort (count-occur lst) #'> :key #'cdr))

(defun count-occur (lst)
  (if (null lst)
      nil
      (let ((elt (car lst)))
        (let ((rest (remove elt lst)))
          (cons (cons elt (- (length lst) (length rest)))
                (count-occur rest))))))

(defun plus+-rec (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (inc-pos 0 lst)))

(defun inc-pos (inc lst)
  (if (null lst)
      lst
      (cons (+ inc (car lst))
            (inc-pos (+ inc 1) (cdr lst)))))

(defun plus+-iter (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push (+ elt (length acc)) acc))
    (reverse acc)))

(defun plus+-map (lst)
  (mapcar #'+
          lst
          (reverse (range (length lst)))))

(defun range (end)
  (if (zerop end)
      '(0)
      (cons end (range (- end 1)))))


;;;; swap cdr and car

(defun gov-cons (obj1 obj2)
  (cons obj2 obj1))

(defun gov-length (lst)
  (if (null lst)
      0
      (+ 1 (gov-length (car lst)))))

(defun gov-member (elt lst)
  (if (null lst)
      nil
      (if (eql elt (cdr lst))
          lst
          (gov-member elt (car lst)))))

(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (progn
        (format t "(~a . " (car lst))
        (showdots (cdr lst))
        (format t ")"))))

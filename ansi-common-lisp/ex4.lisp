;; rotates the array 90 degrees clockwise
(defun quarter-turn (array)
  (let ((dim (array-dimensions array)))
    (if (/= (car dim) (cadr dim))
        nil
        (let ((rotated (make-array dim
                                   :initial-element 0)))
          (dotimes (i (car dim))
            (dotimes (j (car dim))
              (setf (aref rotated j (- (- (car dim) 1) i)) (aref array i j))))
          rotated))))

(defun copy-list-red (l)
  (reduce #'cons l :from-end t :initial-value '()))

(defun reverse-red (l)
  (reduce (lambda (x y) (cons y x)) l :initial-value '()))

(defun alist->hash (alist)
  (let ((table (make-hash-table :size (length alist))))
    (dolist (v alist)
      (setf (gethash (car v) table) (cdr v)))
    table))

(defun hash->alist (htable)
  (let ((alist ()))
    (maphash (lambda (k v) (push (cons k v) alist)) htable)
    alist))

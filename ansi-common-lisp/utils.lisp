(defun single? (lst)
  "Returns true if lst is single element list."
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  "Appends obj at the end of lst."
  (append lst (list obj)))

(defun map-int (fn n)
  "Retuns a list of the results of calling fn on numbers from 0 up to n-1."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  "Collects the non nil results of calling `fn on each element of `lst."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (when val (push val acc))))
    (nreverse acc)))

(defun most (fn lst)
  "Returns the most scoring object in `lst` togheter with its score accoring to `fn`. "
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

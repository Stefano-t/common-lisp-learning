(defun args-len (&rest args)
  (length args))

(let ((greatest nil))
  (defun greatest-call (x)
    (when (null greatest)
      (setf greatest x))
    (when (> x greatest)
      (setf greatest x))
    greatest))

(let ((last-value nil))
  (defun greater-last-call (x)
    (if (null last-value)
        (progn
          (setf last-value x)
          nil)
        (let ((greater (if (> last-value x) last-value x)))
          (setf last-value x)
          greater))))

;; ex 8
(defun expensive (n)
  (random (+ (* n n) 12)))

(defun memoize (fn)
  (let ((cache (make-hash-table)))
    #'(lambda (x)
        (multiple-value-bind (value present-p)
            (gethash x cache)
          (format t "~a ~a" value present-p)
          (if present-p
              value
              (let ((result (funcall fn x)))
                (setf (gethash x cache) result)
                result))))))

(setf (symbol-function 'frugal)
      (memoize #'expensive))

;; ex 9
(defun octal-apply (fn &rest args)
  (let ((*print-base* 8))
    (princ (apply fn (car args)))))

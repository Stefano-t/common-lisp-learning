;;; substitue old string into new string

;;; first, we implement a ring buffer
(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun bref (buf index)
  (svref (buf-vec buf)
         (mod index (length (buf-vec buf)))))

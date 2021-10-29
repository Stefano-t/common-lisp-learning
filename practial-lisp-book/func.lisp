;; &optional (variable-name default-value supplied-p)
;; or &optional variable-name

(defun my-list (a b &optional c d)
  (list a b c d))

(defun my-list-2 (a b &optional (c 'c) (d 'd))
  (list a b c d))

(defun print-if-supplied (&optional (a nil a-supplied-p))
  (if a-supplied-p
      (format t "~a~%" a)
      (format t "no a supplied")))

;; &rest wraps all supplied elements in a list
(defun print-all-values (&rest xs)
  (dolist (x xs)
    (format t "~a " x)))

;; &key allows the parameters to be supplied via keyboard ':'
;; we can specify a defualt value, and if the value has supplied
(defun keyboard-base (&key a (b 10) (c 20 c-supplied-p))
  (list a b c c-supplied-p))

;; these two calls return the same objecet, i.e. a function object
(function keyboard-base)
#'keyboard-base

;; to call an object function, use FUNCALL or APPLY
(defun plot (fn min max step)
  "fn must be a function object which accept one argument"
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))

;; use APPLY when your argument are in a list, so you don't need to unpack them.
(apply #'plot '(exp 0 4 1/2))

;; or use the list as a rest part of the call
(apply #'plot #'exp '(0 4 1/2))

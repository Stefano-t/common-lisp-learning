(defun compose (&rest fns)
  "Creates a function which is the composition of the given ones."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  "Composes a function of disjoint predicates."
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  "Composes a function of conjoint predicates."
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  "Left curry the function with the given arguments."
  #'(lambda (&rest more-args)
      (apply fn (append args more-args))))

(defun rcurry (fn &rest args)
  "Right curry the function with the given arguments."
  #'(lambda (&rest more-args)
      (apply fn (append more-args args))))

(defun always (x)
  "Creates a function that always returns the given argument."
  #'(lambda (&rest args)
      (declare (ignore args))
      x))

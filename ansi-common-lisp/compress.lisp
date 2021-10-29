;;;; run-length compression

;; compress executes the run-length compression algorithm, and it's
;; the entry point

(defun compress (x)
  "Performes run-length encoding."
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  "Computes the main body of the compress method."
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql elt next)
            (compr elt (1+ n) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  "Packs elt and n in a list if n > 1."
  (if (> n 1)
      (cons n elt)
      elt))

(defun uncompress (lst)
  "Uncompresses the list compressed by `compress'."
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (atom elt)
            (cons elt rest)
            (append (apply #'list-of elt)
                    rest)))))

(defun list-of (n elt)
  "Repeats n times elt in a list."
  (if (zerop n)
      nil
      (cons elt (list-of (1- n) elt))))

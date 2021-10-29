(defun precedes (c str)
  (if (< (length str) 2)
      nil
      (do ((p (position c str :test #'char= :start 1)
               (position c str :test #'char= :start (+ p 1)))
            (lst nil (cons (char str (- p 1)) lst)))
          ((null p) (nreverse lst)))))

(defun precedes-rec (c str)
  (if (< (length str) 2)
      nil
      (let ((p (position c str :test #'char= :start 1)))
        (cons (char str (- p 1))
              (precedes-rec c (subseq str (+ p 1)))))))

(defun intersperse (obj lst)
  (if (or (atom lst) (null (cdr lst)))
      lst
      (let ((inter-lst (list (car lst))))
        (dolist (x (cdr lst) (nreverse inter-lst))
          (push obj inter-lst)
          (push x inter-lst)))))

(defun intersperse-rec (obj lst)
  (cond ((atom lst) nil)
        ((null (cdr lst)) (list (car lst)))
        (t (append (list (car lst) obj)
                   (intersperse-rec obj (cdr lst))))))

(let ((x (car y)))
  (cons x x))

((lambda (x) (cons x x)) (car y))

(let* ((w (car x))
        (y (+ w z)))
  (cons w y))

(let ((w (car x)))
  (let (y (+ w z))
    (cons w y)))

((lambda (w z) ((lambda (y) (cons w y)) (+ w z))) (car x) z)

(defun square-if (n)
  (when (> n 5)
    (* n n)))

(defun unitary-conseq-diff (lst)
  (if (null (cdr lst))
      t
      (and (= 1 (abs (- (car lst) (cadr lst))))
           (unitary-conseq-diff (cdr lst)))))

(defun unitary-conseq-diff-do (lst)
  (if (null (cdr lst))
      t
      (let ((l (length lst)))
        (do ((p 0 (+ p 1))
             (e1 (car lst) (nth p lst))
             (e2 (cadr lst) (nth (+ 1 p) lst)))
            ((> p (- l 1)) t)
          (when (/= 1 (abs (- e1 e2)))
            (return nil))))))

(defun unitary-conseq-diff-mapc (lst)
  (mapc #'(lambda (x y)
            (when (/= 1 (abs (- x y)))
              (return-from unitary-conseq-diff-mapc nil)))
        lst
        (cdr lst)))


(defun max-and-min (vec)
  (cond ((zerop (length vec)) (values nil nil))
        ((= 1 (length vec)) (values (svref vec 0)
                                    (svref vec 0)))
        (t (find-max-min (svref vec 0) (svref vec 0) vec 0))))

(defun find-max-min (min max vec index)
  (cond ((>= index (length vec)) (values max min))
        ((> (svref vec index) max)
         (find-max-min min (svref vec index) vec (+ 1 index)))
        ((< (svref vec index) min)
         (find-max-min (svref vec index) max vec (+ 1 index)))
        (t (find-max-min min max vec (+ index 1)))))

;;;; Unit test framework

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Creates a test suite. It can hold single boolean expressions to
   pass to check, or can hold multiple test cases defined with
   deftest"
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defun report-result (result form)
  "Report the result of calling a single test case, and return
  result."
  (let ((r result))
    (format t "~:[FAIL~;pass~] ... ~a ~a~%" r *test-name* form)
    r))

(defmacro check (&body exprs)
  "Call report-result for each exprs in turn, and return the total
  boolean result."
  `(combine-exprs
     ,@(loop for expr in exprs
             collect `(report-result ,expr ',expr))))

(defmacro combine-exprs (&body exprs)
  "Evaluates all the exprs in turn, and collect the final boolean
  result as AND."
  (with-gensyms ("" result)
    `(let ((,result t))
       ,@(loop for expr in exprs
               collect `(unless ,expr (setf ,result nil)))
       ,result)))

;;; use

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) 4)))

(deftest test-* ()
  (check
    (= (* 1 2) 2)
    (= (* 10 0) 0)
    (= (* -1 3) -3)))

(deftest test-arithmetic ()
  (combine-exprs
    (test-*)
    (test-+)))

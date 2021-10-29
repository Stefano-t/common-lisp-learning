;; define a new condition with DEFINE-CONDITION. To use it as error,
;; it should inherit from ERROR
(define-condition malformed-log-entry-error (error)
  ((text :initarg :text
         :reader text)))

;; to signal an error, use ERROR
(error 'malformed-log-entry-error :text "Some text")

;; to handle an error, sorround the code with HANDLER-CASE
(handler-case
    ;; expression which might rise an error
    (progn
      (format t "Now rise an error...~%")
      (error 'malformed-log-entry-error :text "[eee] error"))
  ;; handler, the name of handled error, the error as argument an then
  ;; the code
  (malformed-log-entry-error (e)
    (format t "Text of the error: ~a~%" (text e))))

(defun print-with-error (times)
  (dotimes (i times)
    (format t "~a: now run error..." i)
    ;; bind a restart, so the function can handle the problem locally,
    ;; without unwind the stack
    (restart-case (error 'malformed-log-entry-error :text i)
      (print-and-skip (e) (format t "restart got ~a~%" (text e)))
      ;; this is a default restarter
      (use-value (c) c))))

(defun skip (e)
  (declare (ignore e))
  (let ((restart (find-restart 'skip)))
    (when restart (invoke-restart restart nil))))

;; convention of how to to create a restart bind
(defun print-and-skip (c)
  (let ((restart (find-restart 'print-and-skip)))
    (when restart (invoke-restart restart c))))
;; HANDLER-BIND allows to call a restart function for a particual type
;; of error
(handler-bind ((malformed-log-entry-error #'print-and-skip))
  (print-with-error 5))
(handler-bind ((malformed-log-entry-error
                 #'(lambda (c)
                     (declare (ignore c))
                     (use-value nil))))
  (print-with-error 5))

(handler-case (print-with-error 5)
  (malformed-log-entry-error (c) (format t "~a~%" (text c))))

(defun warn-on-number (n times)
  (dotimes (i times)
    (when (zerop (mod i n)) (warn "mod 0 found"))
    (format t "~a " i)))

(defun cerr-on-number (n times)
  (dotimes (i times)
    (when (zerop (mod i n))
      (cerror "continue?" "found mod 0"))
    (format t "~a " i)))

(defmacro lambda-restart (restarter)
  (with-gensyms ("" c r)
    `(function (lambda (,c)
       (let ((,r (find-restart ',restarter ,c)))
         (when ,r (invoke-restart ,r)))))))

(handler-bind ((error (lambda-restart continue)))
  (cerr-on-number 4 10))

(let ((x 10))
  ((lambda (x) (* x x)) x))

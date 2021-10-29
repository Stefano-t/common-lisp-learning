(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  ;; this is a property list, where there is a symbol associated with
  ;; a value
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  ;; binds cd to each value of *db* in turn
  (dolist (cd *db*)
    ;; some format directives:
    ;; ~a = print a symbol in a human-readable way
    ;; ~t = tabulate a value
    ;; ~{ ~} = loop over a list
    ;; ~% = insert a newline
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  ;; *query-io* is the stdin
  (format *query-io* "~a: " prompt)
  ;; force LISP to do not wait for newline
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t)
       0)
   (y-or-n-p "Ripped:")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another?: "))
            (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  ;; by default has :input direction
  (with-open-file (in filename)
    (with-standard-io-syntax
      ;; set *db* with the content of what has been read
      (setf *db* (read in)))))

(defun select (select-fn)
  ;; select-fn must be a function, and so must have #' in front
  (remove-if-not select-fn *db*))

;; ;; return a function
;; (defun artist-selector (artist)
;;   ;; #' means to select the function namespace
;;   #'(lambda (cd) (equal (getf cd :artist) artist)))

;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (cd)
;;       (and
;;        (if title (equal (getf cd :title) title) t)
;;        (if artist (equal (getf cd :artist) artist) t)
;;        (if rating (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar #'(lambda (row)
                    (when (funcall selector-fn row)
                      (if title (setf (getf row :title) title))
                      (if artist (setf (getf row :artist) artist))
                      (if rating (setf (getf row :rating) rating))
                      (if ripped-p (setf (getf row :ripped) ripped)))
                    row)
                *db*)))

(defun delete-rows (selector-fn)
  "deletes all entries that match selector-fn"
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  ;; use QUASIQUOTE to unquote variables
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  ;; fields are of the form :key value
  (loop while fields
        ;; POP removes an element from the list
        collecting (make-comparison-expr (pop fields) (pop fields))))

;; creates a macro
(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))

;; `make-path` creates a pathname, which is portable
(defvar path (make-pathname :name "output.txt"))

;; `with-open-file` is a macro with `open` and `close` a file
(with-open-file (str path :direction :output
                          :if-exists :supersede)
  ;; specify the output source for `format`
  (format str "Hello everyone~%"))

(with-open-file (str path)
  (read-line str))

(defun my-cat (file)
  (with-open-file (f file :direction :input)
    ;; specify to not throw an error in case of end of file, but return 'eof instead
    (do ((line (read-line f nil 'eof) (read-line f nil 'eof)))
        ((eql line 'eof))
      (format t "~a~%" line))))

;;; OUTPUT: prin1, princ, terpri

;; output for programs
(prin1 "hello") ; leaves ""
;; output for people
(princ "hello") ; prints the content of the string
(terpri) ; prints a newline

;; `format`
(format t "~A~%" "Ciao come stai") ; ~A uses princ to print
;; with `nil` the output goes to a string
(format nil "~S" '(1 2 3)) ; ~S uses prin1 to print
;; ~F directive formats a floating-point number the arguments are:
;; total number of chars to print, number of decimals, multipling
;; factor by 10, character to use if the formatted output is too long
;; for the given space, character for left padding
(format nil "~10,2,0,'*,' F" 26.21875)


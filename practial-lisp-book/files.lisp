(with-open-file (out "./myfile.txt" :direction :output
                                    :if-exists :supersede)
  (write-line "eccomi qua" out))

(with-open-file (in "./myfile.txt" :if-does-not-exist nil)
  (when in
    (format t "~a~%" (read-line in))))

;; how to misure the length of a file, use binary format to read it
(with-open-file (in "./database.db" :element-type '(unsigned-byte 8))
  (file-length in))

(with-input-from-string (in "1.23")
  (read in))

(with-output-to-string (out)
  (format out "hello, world~%")
  (prin1 '(list 1 2 3) out))

;; echo stream, it is a input stream
(let ((e (make-echo-stream *standard-input* *standard-output*)))
  (unwind-protect (read-line e)
    (close e)))

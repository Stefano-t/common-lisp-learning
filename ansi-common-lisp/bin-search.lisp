;;;; binary search with simple vector

(defun binary-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

;;; finder uses svref, so it considers the vector as simple vector

(defun finder (obj vec start end)
  ;(format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range) ; only one element
        (if (eql obj (svref vec start))
            start
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (svref vec mid)))
            (if (< obj obj2)
                ;; recur in the lower part
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    ;; recur in the upper part
                    (finder obj vec (+ mid 1) end)
                    ;; we have found the object
                    mid)))))))

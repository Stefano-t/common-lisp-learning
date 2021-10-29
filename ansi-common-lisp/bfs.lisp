;;;; shortest path in a graph

;;; encode the adjacent nodes with the convention (node . neighbours)

(setf network '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; queue will be a list of path, not a list of unexplored nodes
(defun bfs (end queue net)
  (if (null queue) ; empty queue, then no path exists
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path) ; found the path
              (bfs end
                   ;; add new paths to the end of the queue
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

;;; new-paths mantains a lists of visited path

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

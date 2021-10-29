;; consp returns true iff its argument is a cons
(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))

;; eql returns t iff the objects are the same in memory
;; equal returns t iff the objects print the same
(eql 'a 'a) ; T
(eql (cons 1 ()) (cons 1 ())) ; NIL
(equal 'a 'a) ; T
(equal (cons 1 ()) (cons 1 ())) ; T

(defun our-equal (x y)
  (or (eql x y) ; same object
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

;; copy-list creates a new cons with the same element of the copied list
(setf x '(1 2 3)
      y (copy-list x))

;; append concats two or more lists, copying all but the last list
(append '(a b) '(c d e) '(f))


;; nth accesses a specific element of a list
(nth 0 '(1 2 3)) ; => 1
(nth 3 '(1 2 3)) ; => nil

;; nthcdr returns the nth cdr of the list
(nthcdr 0 '(1 2 3)) ; => (1 2 3)
(nthcdr 2 '(1 2 3)) ; => (3)

;; mapcar maps each car of the lists in turn accordig to function
(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
;; behaves has zip
(mapcar #'list
        '(1 2 3)
        '(a b c))

(defun id (x) x)

;; maplist works on successive cdr
(maplist #'id '(a b c)) ; => ((A B C) (B C) (C))

;;; TREE
;;; they are in essence lists of list.

;; we can view a list as a binary tree where the car and cdr are both edges of a tree
(setf my-tree '(a (b c) d))

;; copy-tree make a "deep copy" because it follows also car lists
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

;; subst recursive descent into lists to substitute elements
(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))

;;; SETS

;; member returns the first cdr whose car is eql to the given argument
(member 'a '(b d a d))
(member '(a) '((a) (b))) ; => NIL
;; test keyword changes the equality test
(member '(a) '((a) (b)) :test #'equal) ; ((A) (B))
;; key keyword changes the retrive function for the value to compare
(member 'a '((a b) (c d)) :key #'car) ; ((a b) (c d))

;; member-if returns the first cdr whose car satisfies the predicate function
(member-if #'evenp '(1 2 3)) ; => (2 3)

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

;; adjoin adds an object to a list iff it is not present in the list
(adjoin 'a '(b a d)) ; => (b a d)
(adjoin 'z '(b a d)) ; => (z b a d)

;; set operations
(union '(a b c) '(b c d e)) ; => (e d a b c)
(intersection '(a b c) '(a d e)) ; => (a)
(set-difference '(a b c d e) '(a b)) ; => (e d c)


;;; SEQUENCES

(length '(a b c)) ; 3

(subseq '(a b c d e) 1) ; (b c d e)
(subseq '(a b c d e) 1 3) ; (b c)

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  ;; reverse the sequence
                  (reverse (subseq s mid)))))))

(sort '(4 0 1 9 7 3) #'>)

(defun nthmost (n seq)
  "Returns the n greatest number in seq."
  (nth (- n 1)
       ;; use copy-list because sort is destructive
       (sort (copy-list seq) #'>)))

;; predicate all over the list
(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3))
(every #'> '(2 4 6) '(1 3 5))

;;; STACK

(setf x '(a))
(push 'b x)
(push 'c x)
(pop x)

;; pushnew pushes an element if it is not present
(let ((x '(a b)))
  (pushnew 'c x)
  (pushnew 'b x)
  x)

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

;;; DOTTED LIST

(cons 'a 'b) ; => (a . b)
(cons 'a (cons 'b (cons 'c 'd))) ; => (a b c . d)
(cons 'a (cons 'b nil)) ; => (a b) proper list

;; ASSOC-LIST

;; lists of pairs
(setf trans '((+ . "add") (- . "subtract")))

;; assoc retrieves the pair if key matches the car
(assoc '+ trans)
(assoc '* trans)

(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (equal key (car pair))
             pair
             (our-assoc key (cdr alist))))))


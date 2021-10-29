(in-package :st.binary-data)

;; big-endian representation
(defconstant +null+ (code-char 0))

(defun read-u2 (in)
  "Reads 2 unsigned byte in big-endian order."
  (let ((u2 0))
    ;; LDB load a given number of bits starting from the rightmost given bit
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
          until (char= char +null+)
          do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop for char across string
        do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

;;; MACRO GOAL

;; (define-binary-class id3-tag
;;   ((file-identifier (iso-8859-1-string :length 3))
;;    (major-version   u1)
;;    (revision        u1)
;;    (flags           u1)
;;    (size            id3-tag-size)
;;    (frames          (id3-frames :tag-size size))))


;; (major-version :initarg :major-version :accessor major-version)

(defun as-keyword (sym)
  "Intern the symbol in keyboard namespace."
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of given type form the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value of the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defun normalize-slot-spec (spec)
  (list (first spec) (if (listp (second spec))
                         (second spec)
                         (list (second spec)))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;; associate a plist to the symbol corresping to class name, in this way all the superclasses and
;;; slots are availabe at compile and runtime (with EVAL-WHEN).

(defun direct-slots (name)
  "Get the associated plist of symbol name."
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  "Recursively finds all slots inherited from superclasses."
  (loop for super in (get name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  "Gets all information attached to name."
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  "Returns all the information associated to symbols in slots and superclasses."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;;; define tagged structures

;; common factor for plain and tagged structures
(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
        (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
          (let ((,objectvar (make-instance ,@(or (cdr (assoc :dispatch options))
                                                 (error "Must supply :dispatch form."))
                                           ,@(mapcan #'slot->keyword-arg slots))))
            (read-object ,objectvar ,streamvar)
            ,objectvar))))))

;;; generate function for read-write primitive types

(defun mklist (elem)
  (if (listp elem)
      elem
      (list elem)))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
              (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))

;; (defmacro define-binary-class (name (&rest superclasses) slots)
;;   (with-gensyms (objectvar streamvar)
;;     `(progn ; we need to define multiple s-expressions
;;        (eval-when (:compile-toplevel :load-toplevel :execute)
;;          ;; associate a plist to symbol name
;;          (setf (get ',name 'slots) ,(mapcar #'first slots))
;;          (setf (get ',name 'superclasses) ',superclasses))

;;        (defclass ,name ,superclasses
;;          ,(mapcar #'slot->defclass-slot slots))

;;        (defmethod read-object progn ((,objectvar ,name) ,streamvar)
;;          (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
;;            ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))

;;        (defmethod write-object progn ((,objectvar ,name) ,streamvar)
;;          (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
;;            ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

       ;; (defmethod read-value (,typevar (eql ',name) ,streamvar &key)
       ;;   (let ((,objectvar (make-instance ',name)))
       ;;     (with-slots (,(mapcar #'first slots)) ,objectvar
       ;;       ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
       ;;     ,objectvar))

       ;; (defmethod write-value (,typevar (eql ',name) ,streamvar ,objectvar &key)
       ;;   (with-slots ,(mapcar #'first slots) ,objectvar
       ;;     ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))


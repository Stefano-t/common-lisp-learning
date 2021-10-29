(in-package :cl-user)

(defpackage :st.binary-data
  (:use :common-lisp)
  (:export
   :define-binary-class
   :+null+
   :define-tagged-binary-class
   :define-binary-type
   :write-object
   :read-object))

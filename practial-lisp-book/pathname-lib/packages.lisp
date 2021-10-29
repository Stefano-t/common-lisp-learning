(in-package :cl-user)

(defpackage :st.path ; this evaluates to a flat name as a string
  (:use :common-lisp) ; the base package that contains std function
  (:export ; what function should the package export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p
   :file-size))

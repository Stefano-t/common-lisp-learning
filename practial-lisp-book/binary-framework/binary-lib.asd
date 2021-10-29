(defsystem binary-lib
  :version "0.1.0"
  :author "Stefano Taverni"
  :license ""
  :description "Utility library to manipulate binary data."
  :depends-on ()
  :components ((:file "package")
               (:file "binary-lib" :depends-on ("package"))))

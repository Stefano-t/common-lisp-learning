(defsystem pathname-lib
  :version "0.1.0"
  :author "Stefano Taverni"
  :license ""
  :description "A portable file system utility library"
  :depends-on ()
  :components ((:file "packages")
               (:file "path-framework" :depends-on ("packages"))))

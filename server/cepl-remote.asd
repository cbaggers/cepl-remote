;;;; cepl-remote.asd

(asdf:defsystem #:cepl-remote
  :description "Describe cepl-remote here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:usocket #:bordeaux-threads #:ieee-floats #:binary-types
                         #:chanl #:trivial-garbage #:defstruct-plus-methods
                         #:defclass-triv)
  :components ((:file "package")
               (:file "cepl-remote")))

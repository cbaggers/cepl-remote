;;;; package.lisp

(defpackage #:cepl-remote
  (:use #:cl #:defstruct-plus-methods #:defclass-triv)
  (:export :make-cepl-remote-server :read-all-remote-messages))

(cl:in-package :cl-user)

(defpackage :simple-io.doc
  (:use :cl))
(in-package :simple-io.doc)

(defun write-docs (&key
                     (pathname "SIMPLE-IO.md")
                     (packages '(simple-io/io)))
  ;; (ql:quickload "coalton/doc")
  ;; (ql:quickload "simple-io")

  (coalton/doc:write-documentation
    pathname
    packages
    :backend :hugo))
(write-docs)

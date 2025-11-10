(cl:in-package :cl-user)

(defpackage :io.doc
  (:use :cl))
(in-package :io.doc)

(defun write-docs (&key
                     (pathname "SIMPLE-IO.md")
                     (packages '(io/io)))
  ;; (ql:quickload "coalton/doc")
  ;; (ql:quickload "io")

  (coalton/doc:write-documentation
    pathname
    packages
    :backend :hugo))
(write-docs)

(cl:in-package :cl-user)
(defpackage :io/utils
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:compose2
   ))
(in-package :io/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare compose2 ((:c -> :d) -> (:a -> :b -> :c) -> :a -> :b -> :d))
  (define (compose2 fcd fabc a b)
    (fcd (fabc a b))))

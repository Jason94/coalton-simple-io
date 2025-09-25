(cl:in-package :cl-user)
(defpackage :simple-io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io)
  (:local-nicknames)
  (:export
   ))
(in-package :simple-io/unique)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (repr :transparent)
  (define-type Unique (Unique% Integer))

  (define-instance (Ord Unique)
    (define (<=> (Unique% a) (Unique% b))
      (<=> a b)))

  ;; NOTE: If the heuristic inliner decides to inline this, it'll break.
  (declare counter% (IORef Integer))
  (define counter% (run! (new-io-ref 0)))

  (declare new-unique (IO Unique))
  (define new-unique
    (do
     (x <- (modify counter% (+ 1)))
     (pure (Unique% x))))

  )

(cl:in-package :cl-user)
(defpackage :simple-io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io)
  (:local-nicknames)
  (:export
   #:MonadIoUnique
   #:Unique
   #:new-unique
   #:to-int
   ))
(in-package :simple-io/unique)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Monad :m => MonadIoUnique :m)
    (new-unique (:m Unique)))

  (derive Eq)
  (repr :transparent)
  (define-type Unique (Unique% Integer))

  (define-instance (Ord Unique)
    (define (<=> (Unique% a) (Unique% b))
      (<=> a b)))

  ;; NOTE: If the heuristic inliner decides to inline this, it'll break.
  (declare counter% (IORef Integer))
  (define counter% (run! (new-io-ref 0)))

  (declare new-unique% (IO Unique))
  (define new-unique%
    (do
     (x <- (modify counter% (+ 1)))
     (pure (Unique% x))))

  (inline)
  (declare to-int (Unique -> Integer))
  (define (to-int (Unique% i))
    i)

  (define-instance (MonadIoUnique IO)
    (define new-unique new-unique%))
  )

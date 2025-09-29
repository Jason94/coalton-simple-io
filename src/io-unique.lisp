(cl:in-package :cl-user)
(defpackage :simple-io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
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

  (declare counter% (c:Cell Integer))
  (define counter% (c:new 0))

  (declare new-unique% (IO Unique))
  (define new-unique%
    (wrap-io (Unique% (c:increment! counter%))))

  (inline)
  (declare to-int (Unique -> Integer))
  (define (to-int (Unique% i))
    i)

  (define-instance (MonadIoUnique IO)
    (define new-unique new-unique%))

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (define-instance ((MonadIoUnique :m) => MonadIoUnique (st:StateT :s :m))
    (define new-unique (lift new-unique)))

  (define-instance ((MonadIoUnique :m) => MonadIoUnique (env:EnvT :e :m))
    (define new-unique (lift new-unique))))

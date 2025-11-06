(cl:in-package :cl-user)
(defpackage :simple-io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   #:MonadIoUnique
   #:derive-monad-io-unique
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
    (define new-unique new-unique%)))

(cl:defmacro derive-monad-io-unique (monad-param monadT-form)
  "Automatically derive an instance of MonadIoUnique for a monad transformer.

Example:
  (derive-monad-io-unique :m (st:StateT :s :m))"
  `(define-instance (MonadIoUnique ,monad-param => MonadIoUnique ,monadT-form)
     (define new-unique (lift new-unique))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-unique :m (st:StateT :s :m))
  (derive-monad-io-unique :m (env:EnvT :e :m))
  (derive-monad-io-unique :m (LoopT :m)))

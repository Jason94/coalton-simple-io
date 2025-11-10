(cl:in-package :cl-user)
(defpackage :simple-io/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/monad-io)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:at #:simple-io/atomics_)
   (:io #:simple-io/io)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   #:Var
   #:derive-monad-var
   #:MonadIoVar
   #:new-var
   #:read
   #:write
   #:modify
   
   #:implement-monad-io-var
   ))
(in-package :simple-io/mut)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (Var :a)
    (Var% (Cell :a)))

  (define-class (Monad :m => MonadIoVar :m)
    (new-var
     "Create a new variable with an initial value."
     (:a -> :m (Var :a)))
    (read
     "Read the current value stored in a variable."
     (Var :a -> :m :a))
    (write
     "Set the value in a variable and return the old value."
     (Var :a -> :a -> :m :a))
    (modify
     "Modify the value in a variable by applying F, and return the old value."
     (Var :a -> (:a -> :a) -> :m :a)))

  (inline)
  (declare new-var% (MonadIo :m => :a -> :m (Var :a)))
  (define (new-var% val)
    (wrap-io (Var% (c:new val))))

  (inline)
  (declare read% (MonadIo :m => Var :a -> :m :a))
  (define (read% (Var% cel))
    (wrap-io (c:read cel)))

  (inline)
  (declare write% (MonadIo :m => Var :a -> :a -> :m :a))
  (define (write% (Var% cel) val)
    "Set the value in an Var and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (inline)
  (declare modify% (MonadIo :m => Var :a -> (:a -> :a) -> :m :a))
  (define (modify% (Var% cel) f)
    "Modify the value in an Var and return the old value."
    (wrap-io (c:update-swap! f cel))))

(cl:defmacro implement-monad-io-var (monad)
  `(define-instance (MonadIoVar ,monad)
     (define new-var new-var%)
    (define read read%)
    (define write write%)
    (define modify modify%)))

(cl:defmacro derive-monad-var (monad-param monadT-form)
  "Automatically derive an instance of MonadIoVar for a monad transformer.

Example:
  (derive-monad-var :m (st:StateT :s :m))"
  `(define-instance (MonadIoVar ,monad-param => MonadIoVar ,monadT-form)
     (define new-var (compose lift new-var))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-var :m (st:StateT :s :m))
  (derive-monad-var :m (env:EnvT :e :m))
  (derive-monad-var :m (LoopT :m)))

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-var io:IO))

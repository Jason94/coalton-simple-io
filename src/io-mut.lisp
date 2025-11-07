(cl:in-package :cl-user)
(defpackage :simple-io/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:at #:simple-io/atomics_)
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
   ))
(in-package :simple-io/mut)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (Var :a)
    (Var% (Cell :a)))

  (define-class (Monad :m => MonadIoVar :m)
    (new-var (:a -> :m (Var :a)))
    (read (Var :a -> :m :a))
    (write (Var :a -> :a -> :m :a))
    (modify (Var :a -> (:a -> :a) -> :m :a)))

  (inline)
  (declare new-var% (:a -> IO (Var :a)))
  (define (new-var% val)
    (wrap-io (Var% (c:new val))))

  (inline)
  (declare read% (Var :a -> IO :a))
  (define (read% (Var% cel))
    (wrap-io (c:read cel)))

  (inline)
  (declare write% (Var :a -> :a -> IO :a))
  (define (write% (Var% cel) val)
    "Set the value in an Var and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (inline)
  (declare modify% (Var :a -> (:a -> :a) -> IO :a))
  (define (modify% (Var% cel) f)
    "Modify the value in an Var and return the old value."
    (wrap-io (c:update-swap! f cel)))

  (define-instance (MonadIoVar IO)
    (define new-var new-var%)
    (define read read%)
    (define write write%)
    (define modify modify%))
  )

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

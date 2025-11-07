(cl:in-package :cl-user)
(defpackage :simple-io/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:at #:simple-io/atomics_)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   #:AtVar
   #:MonadAtVar
   #:derive-monad-at-var
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   ))
(in-package :simple-io/atomic)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (AtVar :a)
    (AtVar% (at:Atomic :a)))

  (inline)
  (declare unwrap-atvar (AtVar :a -> at:Atomic :a))
  (define (unwrap-atvar (AtVar% atm))
    atm)

  (define-class (Monad :m => MonadAtVar :m)
    (new-at-var
     "Create a new atomic variable with an initial value."
     (:a -> :m (AtVar :a)))
    (read
     "Read the value from an atomic variable."
     (AtVar :a -> :m :a))
    (write
     "Write a new value to an atomic variable."
     (AtVar :a -> :a -> :m Unit))
    (modify
     "Atomically modify the value of an atomic variable
by applying F, then return the *new* value of the variable.
F may be called multiple times, and must be a pure function."
     (AtVar :a -> (:a -> :a) -> :m :a))
    (modify-swap
     "Atomically modify the value of an atomic variable
by applying F, then return the *old* value of the variable.
F may be called multiple times, and must be a pure function."
     (AtVar :a -> (:a -> :a) -> :m :a))
    (push
     "Atomically push a value onto an atomic list."
     (AtVar (List :a) -> :a -> :m (List :a)))
    (pop
     "Atomically pop and retrieve the head of an atomic list."
     (AtVar (List :a) -> :m (Optional :a))))

  (inline)
  (declare new-at-var% (:a -> IO (AtVar :a)))
  (define (new-at-var% val)
    (wrap-io (AtVar% (at:new val))))

  (inline)
  (declare read% (AtVar :a -> IO :a))
  (define (read% atm)
    (wrap-io (at:read (unwrap-atvar atm))))

  (inline)
  (declare write% (AtVar :a -> :a -> IO Unit))
  (define (write% atm val)
    (wrap-io (at:atomic-write (unwrap-atvar atm) val)))

  (inline)
  (declare modify% (AtVar :a -> (:a -> :a) -> IO :a))
  (define (modify% atm f)
    (wrap-io (at:atomic-update (unwrap-atvar atm) f)))

  (inline)
  (declare modify-swap% (AtVar :a -> (:a -> :a) -> IO :a))
  (define (modify-swap% atm f)
    (wrap-io (at:atomic-update-swap (unwrap-atvar atm) f)))

  (inline)
  (declare push% (AtVar (List :a) -> :a -> IO (List :a)))
  (define (push% atm elt)
    (wrap-io (at:atomic-push (unwrap-atvar atm) elt)))

  (inline)
  (declare pop% (AtVar (List :a) -> IO (Optional :a)))
  (define (pop% atm)
    (wrap-io (at:atomic-pop (unwrap-atvar atm))))

  (define-instance (MonadAtVar IO)
    (define new-at-var new-at-var%)
    (define read read%)
    (define write write%)
    (define modify modify%)
    (define modify-swap modify-swap%)
    (define push push%)
    (define pop pop%))
  )

(cl:defmacro derive-monad-at-var (monad-param monadT-form)
  "Automatically derive an instance of MonadAtVar for a monad transformer.

Example:
  (derive-monad-at-var :m (st:StateT :s :m))"
  `(define-instance (MonadAtVar ,monad-param => MonadAtVar ,monadT-form)
     (define new-at-var (compose lift new-at-var))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))
     (define modify-swap (compose2 lift modify-swap))
     (define push (compose2 lift push))
     (define pop (compose lift pop))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-at-var :m (st:StateT :s :m))
  (derive-monad-at-var :m (env:EnvT :e :m))
  (derive-monad-at-var :m (LoopT :m)))

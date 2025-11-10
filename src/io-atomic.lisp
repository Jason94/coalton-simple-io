(cl:in-package :cl-user)
(defpackage :simple-io/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/monad-io)
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
   
   #:implement-monad-io-atomic
   ))
(in-package :simple-io/atomic)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (AtVar :a)
    (AtVar% (at:Atomic :a)))

  (inline)
  (declare unwrap-atvar (MonadIo :m => AtVar :a -> at:Atomic :a))
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
  (declare new-at-var% (MonadIo :m => :a -> :m (AtVar :a)))
  (define (new-at-var% val)
    (wrap-io (AtVar% (at:new val))))

  (inline)
  (declare read% (MonadIo :m => AtVar :a -> :m :a))
  (define (read% atm)
    (wrap-io (at:read (unwrap-atvar atm))))

  (inline)
  (declare write% (MonadIo :m => AtVar :a -> :a -> :m Unit))
  (define (write% atm val)
    (wrap-io (at:atomic-write (unwrap-atvar atm) val)))

  (inline)
  (declare modify% (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify% atm f)
    (wrap-io (at:atomic-update (unwrap-atvar atm) f)))

  (inline)
  (declare modify-swap% (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify-swap% atm f)
    (wrap-io (at:atomic-update-swap (unwrap-atvar atm) f)))

  (inline)
  (declare push% (MonadIo :m => AtVar (List :a) -> :a -> :m (List :a)))
  (define (push% atm elt)
    (wrap-io (at:atomic-push (unwrap-atvar atm) elt)))

  (inline)
  (declare pop% (MonadIo :m => AtVar (List :a) -> :m (Optional :a)))
  (define (pop% atm)
    (wrap-io (at:atomic-pop (unwrap-atvar atm)))))

(cl:defmacro implement-monad-io-atomic (monad)
  `(define-instance (MonadAtVar ,monad)
     (define new-at-var new-at-var%)
     (define read read%)
     (define write write%)
     (define modify modify%)
     (define modify-swap modify-swap%)
     (define push push%)
     (define pop pop%)))

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

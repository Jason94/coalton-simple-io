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
   #:AtRef
   #:MonadAtRef
   #:derive-monad-at-ref
   #:new-at-ref
   #:read
   #:write
   #:modify
   #:modify-swap
   ))
(in-package :simple-io/atomic)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (AtRef :a)
    (AtRef% (at:Atomic :a)))

  (inline)
  (declare unwrap-atref (AtRef :a -> at:Atomic :a))
  (define (unwrap-atref (AtRef% atm))
    atm)

  (define-class (Monad :m => MonadAtRef :m)
    (new-at-ref
     "Create a new atomic reference with an initial value."
     (:a -> :m (AtRef :a)))
    (read
     "Read the value from an atomic reference."
     (AtRef :a -> :m :a))
    (write
     "Write a new value to an atomic reference."
     (AtRef :a -> :a -> :m Unit))
    (modify
     "Atomically modify the value of an atomic reference
by applying F, then return the *new* value of the reference.
F may be called multiple times, and must be a pure function."
     (AtRef :a -> (:a -> :a) -> :m :a))
    (modify-swap
     "Atomically modify the value of an atomic reference
by applying F, then return the *old* value of the reference.
F may be called multiple times, and must be a pure function."
     (AtRef :a -> (:a -> :a) -> :m :a))
    (push
     "Atomically push a value onto an atomic list."
     (AtRef (List :a) -> :a -> :m (List :a)))
    (pop
     "Atomically pop and retrieve the head of an atomic list."
     (AtRef (List :a) -> :m (Optional :a))))

  (inline)
  (declare new-at-ref% (:a -> IO (AtRef :a)))
  (define (new-at-ref% val)
    (wrap-io (AtRef% (at:new val))))

  (inline)
  (declare read% (AtRef :a -> IO :a))
  (define (read% atm)
    (wrap-io (at:read (unwrap-atref atm))))

  (inline)
  (declare write% (AtRef :a -> :a -> IO Unit))
  (define (write% atm val)
    (wrap-io (at:atomic-write (unwrap-atref atm) val)))

  (inline)
  (declare modify% (AtRef :a -> (:a -> :a) -> IO :a))
  (define (modify% atm f)
    (wrap-io (at:atomic-update (unwrap-atref atm) f)))

  (inline)
  (declare modify-swap% (AtRef :a -> (:a -> :a) -> IO :a))
  (define (modify-swap% atm f)
    (wrap-io (at:atomic-update-swap (unwrap-atref atm) f)))

  (inline)
  (declare push% (AtRef (List :a) -> :a -> IO (List :a)))
  (define (push% atm elt)
    (wrap-io (at:atomic-push (unwrap-atref atm) elt)))

  (inline)
  (declare pop% (AtRef (List :a) -> IO (Optional :a)))
  (define (pop% atm)
    (wrap-io (at:atomic-pop (unwrap-atref atm))))

  (define-instance (MonadAtRef IO)
    (define new-at-ref new-at-ref%)
    (define read read%)
    (define write write%)
    (define modify modify%)
    (define modify-swap modify-swap%)
    (define push push%)
    (define pop pop%))
  )

(cl:defmacro derive-monad-at-ref (monad-param monadT-form)
  "Automatically derive an instance of MonadAtRef for a monad transformer.

Example:
  (derive-monad-at-ref :m (st:StateT :s :m))"
  `(define-instance (MonadAtRef ,monad-param => MonadAtRef ,monadT-form)
     (define new-at-ref (compose lift new-at-ref))
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

  (derive-monad-at-ref :m (st:StateT :s :m))
  (derive-monad-at-ref :m (env:EnvT :e :m))
  (derive-monad-at-ref :m (LoopT :m)))

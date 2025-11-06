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
   #:IORef
   #:MonadIoRef
   #:new-io-ref
   #:read
   #:write
   #:modify
   ))
(in-package :simple-io/mut)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (IORef :a)
    (IORef% (Cell :a)))

  (define-class (Monad :m => MonadIoRef :m)
    (new-io-ref (:a -> :m (IORef :a)))
    (read (IORef :a -> :m :a))
    (write (IORef :a -> :a -> :m :a))
    (modify (IORef :a -> (:a -> :a) -> :m :a)))

  (inline)
  (declare new-io-ref% (:a -> IO (IORef :a)))
  (define (new-io-ref% val)
    (wrap-io (IORef% (c:new val))))

  (inline)
  (declare read% (IORef :a -> IO :a))
  (define (read% (IORef% cel))
    (wrap-io (c:read cel)))

  (inline)
  (declare write% (IORef :a -> :a -> IO :a))
  (define (write% (IORef% cel) val)
    "Set the value in an IORef and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (inline)
  (declare modify% (IORef :a -> (:a -> :a) -> IO :a))
  (define (modify% (IORef% cel) f)
    "Modify the value in an IORef and return the old value."
    (wrap-io (c:update-swap! f cel)))

  (define-instance (MonadIoRef IO)
    (define new-io-ref new-io-ref%)
    (define read read%)
    (define write write%)
    (define modify modify%))
  )

(cl:defmacro derive-monad-io-ref (monad-param monadT-form)
  "Automatically derive an instance of MonadIoRef for a monad transformer.

Example:
  (derive-monad-io-ref :m (st:StateT :s :m))"
  `(define-instance (MonadIoRef ,monad-param => MonadIoRef ,monadT-form)
     (define new-io-ref (compose lift new-io-ref))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-ref :m (st:StateT :s :m))
  (derive-monad-io-ref :m (env:EnvT :e :m))
  (derive-monad-io-ref :m (LoopT :m)))

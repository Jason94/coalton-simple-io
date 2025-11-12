(cl:in-package :cl-user)
(defpackage :io/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/utils
   #:io/monad-io
   #:io/thread
   #:io/mvar)
  (:local-nicknames
   (:io #:io/simple-io))
  (:export
   #:Future
   #:fork-future_
   #:fork-future
   #:await
   #:try-read-future

   #:do-fork-future_
   #:do-fork-future
   ))
(in-package :io/future)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Futures - Note: Unlike most other packages, Futures can be defined
;;; using MVars and Threads. Therefore, Future doesn't need a separate
;;; capability class or deriving/implementing macros.
;;;

(coalton-toplevel
  (repr :transparent)
  (define-type (Future :a)
    (Future% (MVar :a)))

  (inline)
  (declare value-mvar (Future :a -> MVar :a))
  (define (value-mvar (Future% mvar))
    mvar)

  (declare fork-future_ ((MonadIoThread :m) (MonadIoMVar :m) (RunIo :r)
                         => :r :a -> :m (Future :a)))
  (define (fork-future_ task)
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    (do
     (value-var <- new-empty-mvar)
     (do-fork
       (result <- (wrap-io (run! task)))
       (put-mvar value-var result))
     (pure (Future% value-var))))

  (declare fork-future ((MonadIoThread :m) (MonadIoMVar :m)
                        => io:IO :a -> :m (Future :a)))
  (define fork-future
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    fork-future_)

  (inline)
  (declare await (MonadIoMVar :m => Future :a -> :m :a))
  (define (await future)
    "Read the value from FUTURE, blocking until it is available."
    (read-mvar (value-mvar future)))

  (inline)
  (declare try-read-future (MonadIoMvar :m => Future :a -> :m (Optional :a)))
  (define (try-read-future future)
    "Try to read the current value from FUTURE, returning NONE
if it is not available."
    (try-read-mvar (value-mvar future)))
  )

(cl:defmacro do-fork-future_ (cl:&body body)
  `(fork-future_
    (do
     ,@body)))

(cl:defmacro do-fork-future (cl:&body body)
  `(fork-future
    (do
     ,@body)))

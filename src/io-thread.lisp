(cl:in-package :cl-user)
(defpackage :io/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/monad-io
   #:io/unlift)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:t #:coalton-threads/thread)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io))
  (:export
   #:IoThread
   #:MonadIoThread
   #:derive-monad-io-thread
   #:fork_
   #:fork
   #:do-fork_
   #:do-fork
   #:sleep
   
   #:implement-monad-io-thread
   ))
(in-package :io/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type IoThread
    (IoThread% (t:Thread Unit)))

  (define-class (MonadIo :m => MonadIoThread :m)
    (fork_
     "Spawn a new thread, which starts running immediately.
Returns the handle to the thread. This version can accept
any underlying BaseIo, which can be useful, but causes inference
issues in some cases."
     ((UnliftIo :r :i) (LiftTo :r :m) => :r :a -> :m IoThread))
    (sleep
     "Sleep the current thread for MSECS milliseconds."
     (UFix -> :m Unit)))

  (inline)
  (declare fork% ((MonadIo :m) (UnliftIo :r :i) (LiftTo :r :m) => :r :a -> :m IoThread))
  (define (fork% op)
    (lift-to
     (with-run-in-io
         (fn (run)
            (wrap-io (IoThread%
                      (t:spawn (fn ()
                                 (run! (run op))
                                 Unit))))))))
    ;; (wrap-io (IoThread%
    ;;           (t:spawn (fn ()
    ;;                      (run! op)
    ;;                      Unit)))))

  (inline)
  (declare sleep% (MonadIo :m => UFix -> :m Unit))
  (define (sleep% msecs)
    (wrap-io
      (lisp :a (msecs)
        (cl:sleep (cl:/ msecs 1000)))
      Unit)))

(cl:defmacro implement-monad-io-thread (monad)
  `(define-instance (MonadIoThread ,monad)
     (define fork_ fork%)
     (define sleep sleep%)))

(cl:defmacro derive-monad-io-thread (monad-param monadT-form)
  "Automatically derive an instance of MonadIoThread for a monad transformer.

Example:
  (derive-monad-io-thread :m (st:StateT :s :m))"
  `(define-instance (MonadIoThread ,monad-param => MonadIoThread ,monadT-form)
     (define fork_ fork%)
     (define sleep (compose lift sleep))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-thread :m (st:StateT :s :m))
  (derive-monad-io-thread :m (env:EnvT :e :m))
  (derive-monad-io-thread :m (LoopT :m)))

(cl:defmacro do-fork_ (cl:&body body)
  `(fork_
    (do
     ,@body)))

(cl:defmacro do-fork (cl:&body body)
  `(fork
    (do
     ,@body)))

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-thread io:IO)

  (declare fork ((MonadIoThread :m) (UnliftIo :m io:IO) (LiftTo io:IO :m) => io:IO :a -> :m IoThread))
  (define fork fork_)
  )

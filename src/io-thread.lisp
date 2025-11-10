(cl:in-package :cl-user)
(defpackage :simple-io/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:t #:coalton-threads/thread)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   #:IoThread
   #:MonadIoThread
   #:derive-monad-io-thread
   #:fork
   #:do-fork
   #:sleep
   
   #:implement-monad-io-thread
   ))
(in-package :simple-io/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type IoThread
    (IoThread% (t:Thread Unit)))

  (define-class (Monad :m => MonadIoThread :m)
    (fork
     "Spawn a new thread, which starts running immediately.
Returns the handle to the thread."
     (IO :a -> :m IoThread))
    (sleep
     "Sleep the current thread for MSECS milliseconds."
     (UFix -> :m Unit)))

  (inline)
  (declare fork% (MonadIo :m => (:m :a -> :m IoThread)))
  (define (fork% op)
    (wrap-io (IoThread%
              (t:spawn (fn ()
                         (run! op)
                         Unit)))))

  (inline)
  (declare sleep% (MonadIo :m => (UFix -> :m Unit)))
  (define (sleep% msecs)
    (wrap-io
      (lisp :a (msecs)
        (cl:sleep (cl:/ msecs 1000)))
      Unit))

  

(cl:defmacro implement-monad-io-thread (monad)
  `(define-instance (MonadIoThread ,monad)
     (define fork fork%)
    (define sleep sleep%))


  )

(cl:defmacro derive-monad-io-thread (monad-param monadT-form)
  "Automatically derive an instance of MonadIoThread for a monad transformer.

Example:
  (derive-monad-io-thread :m (st:StateT :s :m))"
  `(define-instance (MonadIoThread ,monad-param => MonadIoThread ,monadT-form)
     (define fork (compose lift fork))
     (define sleep (compose lift sleep))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-thread :m (st:StateT :s :m))
  (derive-monad-io-thread :m (env:EnvT :e :m))
  (derive-monad-io-thread :m (LoopT :m)))

(cl:defmacro do-fork (cl:&body body)
  `(fork
    (do
     ,@body)))

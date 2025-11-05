(cl:in-package :cl-user)
(defpackage :simple-io/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
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
   ))
(in-package :simple-io/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type IoThread
    (IoThread% (t:Thread Unit)))

  (define-class (Monad :m => MonadIoThread :m)
    (fork (IO :a -> :m IoThread))
    (sleep (UFix -> :m Unit)))

  (inline)
  (declare fork% (IO :a -> IO IoThread))
  (define (fork% op)
    (wrap-io (IoThread%
              (t:spawn (fn ()
                         (run! op)
                         Unit)))))

  (inline)
  (declare sleep% (UFix -> IO Unit))
  (define (sleep% secs)
    (wrap-io
      (lisp :a (secs)
        (cl:sleep secs))
      Unit))

  (define-instance (MonadIoThread IO)
    (define fork fork%)
    (define sleep sleep%))

  )

(cl:defmacro derive-monad-io-thread (monad-param monadT-form)
  "Automatically derive an instance of MonadAtRef for a monad transformer.

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
  (derive-monad-io-thread :m (env:EnvT :e :m)))

(cl:defmacro do-fork (cl:&body body)
  `(fork
    (do
     ,@body)))

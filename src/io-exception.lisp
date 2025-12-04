(cl:in-package :cl-user)
(defpackage :io/exception
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io)
   (:st #:coalton-library/monad/statet)
   (:e #:coalton-library/monad/environment))
  (:export
   ))
(in-package :io/exception)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: Unlike most IO classes, this one can't be implemented on top of
;; standard MonadIo functionality. As such, it has no implement macro.
;;
;; Also, `handle` can't be simply lifted with `lift`, so there's no
;; derive monad either. See the implementations for the Std. Library
;; transformers below for examples.

(coalton-toplevel

  (define-class (MonadIo :m => MonadIoException :m)
    (raise
     "Raise an error."
     (String -> :m :a))
    (try
     "Try an operation, returning (Err e) if it raised an exception
or (Ok :a) if it completed successfully."
     (:m :a -> :m (Result String :a)))
    (handle
     "Run an operation, immediately handling if it threw an exception."
     (:m :a -> (String -> :m :a) -> :m :a)))

  ;; TODO: I think this can just use the MonadIoException class...
  (inline)
  (declare raise-result (MonadIoException :m => :m (Result String :a) -> :m :a))
  (define (raise-result io-res)
    (matchM io-res
      ((Ok a)
       (pure a))
      ((Err e)
       (raise e))))
  )

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel

  (inline)
  (declare handle-stateT (MonadIoException :m
                          => st:StateT :s :m :a -> (String -> st:StateT :s :m :a)
                          -> st:StateT :s :m :a))
  (define (handle-stateT st-op st-catch-op)
    (st:StateT
     (fn (s)
       (handle
        (st:run-stateT st-op s)
        (fn (e)
          (st:run-stateT
           (st-catch-op e)
           s))))))

  (define-instance (MonadIoException :m => MonadIoException (st:StateT :s :m))
    (define raise (compose lift raise))
    (define (try a)
      (handle
       (map Ok a)
       (compose pure Err)))
    (define handle handle-stateT))

  (inline)
  (declare handle-envT (MonadIoException :m
                        => e:EnvT :e :m :a -> (String -> e:EnvT :e :m :a)
                        -> e:EnvT :e :m :a))
  (define (handle-envT env-op env-catch-op)
    (e:EnvT
     (fn (env)
       (handle
        (e:run-envT env-op env)
        (fn (err)
          (e:run-envT
           (env-catch-op err)
           env))))))

  (define-instance (MonadIoException :m => MonadIoException (e:EnvT :e :m))
    (define raise (compose lift raise))
    (define (try a)
      (handle
       (map Ok a)
       (compose pure Err)))
    (define handle handle-envT))

  ;; TODO: Add instance for LoopT
  )

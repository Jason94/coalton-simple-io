(cl:in-package :cl-user)
(defpackage :io/exception
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io)
   (:st #:coalton-library/monad/statet)
   (:e #:coalton-library/monad/environment))
  (:export
   #:MonadIoException
   #:raise
   #:try
   #:handle
   #:handle-all
   #:do-handle
   #:do-handle-all

   #:raise-result
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
     "Raise an exception."
     (RuntimeRepr :e => :e -> :m :a))
    (handle
     "Run an operation, immediately handling if it raised an exception
that matches :e."
     (RuntimeRepr :e => :m :a -> (:e -> :m :a) -> :m :a))
    ;; NOTE: The second argument could just be :m :a, but wrapping it
    ;; in a function call allows transformer instances to avoid running-down
    ;; to the base MonadIoException layer in the stack, even if no
    ;; exceptions are raised.
    (handle-all
     "Run an operation, immediately handling any exceptions raised."
     (:m :a -> (Unit -> :m :a) -> :m :a)))

  (inline)
  (declare try ((MonadIoException :m) (RuntimeRepr :e) => :m :a -> :m (Result :e :a)))
  (define (try a)
     "Bring any unhandled exceptions of type :e up into a Result.
Continues to carry any unhandeld exceptions not of type :e."
    (handle
     (map Ok a)
     (compose pure Err)))

  (inline)
  (declare raise-result ((MonadIoException :m) (RuntimeRepr :e) => :m (Result :e :a) -> :m :a))
  (define (raise-result io-res)
    "Raise any (Err :e) into :m. Useful if (Err :e) represents any unhandleable, fatal
exception to the program."
    (matchM io-res
      ((Ok a)
       (pure a))
      ((Err e)
       (raise e))))
  )

(cl:defmacro do-handle (op (err-sym) cl:&body body)
  "Convenience macro for handle."
  `(handle ,op
    (fn (,err-sym)
      (do
       ,@body))))

(cl:defmacro do-handle-all (op cl:&body body)
  "Convenience macro for handle-all.

Example:

(do-handle-all add-three-ints
  (modify (Cons 2))
  (pure 10))
===>
(handle-all add-three-ints
  (const
    (do
     (modify (cons 2))
     (pure 10))))
"
  `(handle-all ,op
    (const
     (do
      ,@body))))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel

  (inline)
  (declare handle-stateT ((MonadIoException :m) (RuntimeRepr :e)
                          => st:StateT :s :m :a -> (:e -> st:StateT :s :m :a)
                          -> st:StateT :s :m :a))
  (define (handle-stateT st-op st-handle-op)
    (st:StateT
     (fn (s)
       (handle
        (st:run-stateT st-op s)
        (fn (e)
          (st:run-stateT
           (st-handle-op e)
           s))))))

  (inline)
  (declare handle-all-stateT (MonadIoException :m
                              => st:StateT :s :m :a -> (Unit -> st:StateT :s :m :a)
                              -> st:StateT :s :m :a))
  (define (handle-all-stateT st-op st-handle-op)
    (st:StateT
     (fn (s)
       (handle-all
        (st:run-stateT st-op s)
        (fn ()
         (st:run-stateT (st-handle-op) s))))))

  (define-instance (MonadIoException :m => MonadIoException (st:StateT :s :m))
    (define raise (compose lift raise))
    (inline)
    (define handle handle-stateT)
    (define handle-all handle-all-statet))

  (inline)
  (declare handle-envT ((MonadIoException :m) (RuntimeRepr :err)
                        => e:EnvT :e :m :a -> (:err -> e:EnvT :e :m :a)
                        -> e:EnvT :e :m :a))
  (define (handle-envT env-op env-handle-op)
    (e:EnvT
     (fn (env)
       (handle
        (e:run-envT env-op env)
        (fn (err)
          (e:run-envT
           (env-handle-op err)
           env))))))

  (inline)
  (declare handle-all-envT (MonadIoException :m
                            => e:EnvT :e :m :a -> (Unit -> e:EnvT :e :m :a)
                            -> e:EnvT :e :m :a))
  (define (handle-all-envT env-op env-handle-op)
    (e:EnvT
     (fn (env)
       (handle-all
        (e:run-envT env-op env)
        (fn ()
          (e:run-envT
           (env-handle-op)
           env))))))

  (define-instance (MonadIoException :m => MonadIoException (e:EnvT :e :m))
    (define raise (compose lift raise))
    (define handle handle-envT)
    (define handle-all handle-all-envT))

  ;; TODO: Add instance for LoopT
  )

;;;
;;; Simple IO Instance
;;;

(coalton-toplevel
  (define-instance (MonadIoException io:IO)
    (define raise io:raise-io)
    (define handle io:handle-io)
    (define handle-all io:handle-all-io))
  )

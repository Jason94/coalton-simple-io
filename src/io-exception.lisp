(cl:in-package :cl-user)
(defpackage :io/exception
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
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
   #:raise-dynamic
   #:reraise
   #:try
   #:try-dynamic
   #:handle
   #:handle-all
   #:do-reraise
   #:do-handle
   #:do-handle-all

   #:raise-result
   ;; Re-export for convenience
   #:UnhandledError
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

  ;; NOTE: The second argument for several of these could be :m :a. Wrapping
  ;; in a function call allows transformer instances to avoid running-down
  ;; to the base MonadIoException layer in the stack, even if no
  ;; exceptions are raised. The alternative is to limit those functions to
  ;; just instances of UnliftIo.
  (define-class (MonadIo :m => MonadIoException :m)
    "A MonadIo that can raise and handle exceptions. IMPORTANT: Any MonadIoException
must catch and wrap all unhandled errors inside a wrap-io call as an UnhandledError.
See utils/catch-thunk."
    (raise
     "Raise an exception."
     ((RuntimeRepr :e) (Signalable :e) => :e -> :m :a))
    (raise-dynamic
     "Raise an exception wrapped in a Dynamic. Mainly useful to hand-off eexceptions
between IO instances."
     (Dynamic -> :m :a))
    (reraise
     "Run an operation, run a catch operation if the first operation raised,
then re-raise the exception. If the catch operation raises, that exception will
be emitted instead of the original exception."
     (:m :a -> (Unit -> :m :b) -> :m :a))
    (handle
     "Run an operation, immediately handling if it raised an exception
that matches :e."
     (RuntimeRepr :e => :m :a -> (:e -> :m :a) -> :m :a))
    (handle-all
     "Run an operation, immediately handling any exceptions raised."
     (:m :a -> (Unit -> :m :a) -> :m :a))
    (try-dynamic
     "Bring any unhandled exceptions into a Result wrapped in Dynamic."
     (:m :a -> :m (Result Dynamic :a))))

  (inline)
  (declare try ((MonadIoException :m) (RuntimeRepr :e) => :m :a -> :m (Result :e :a)))
  (define (try a)
     "Bring any unhandled exceptions of type :e up into a Result.
Continues to carry any unhandeld exceptions not of type :e."
    (handle
     (map Ok a)
     (compose pure Err)))

  (inline)
  (declare raise-result ((MonadIoException :m) (RuntimeRepr :e) (Signalable :e)
                         => :m (Result :e :a) -> :m :a))
  (define (raise-result io-res)
    "Raise any (Err :e) into :m. Useful if (Err :e) represents any unhandleable, fatal
exception to the program."
    (matchM io-res
      ((Ok a)
       (pure a))
      ((Err e)
       (raise e))))
  )

(cl:defmacro do-reraise (op cl:&body body)
  "Convenience macro for reraise."
  `(reraise ,op
    (fn ()
      (do
       ,@body))))

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

  (inline)
  (declare reraise-stateT (MonadIoException :m
                           => st:StateT :s :m :a
                           -> (Unit -> st:StateT :s :m :b)
                           -> st:StateT :s :m :a))
  (define (reraise-stateT st-op st-catch-op)
    (st:StateT
     (fn (s)
       (reraise
        (st:run-stateT st-op s)
        (fn ()
          (st:run-stateT (st-catch-op) s))))))

  (inline)
  (declare try-dynamic-stateT (MonadIoException :m
                               => st:StateT :s :m :a
                               -> st:StateT :s :m (Result Dynamic :a)))
  (define (try-dynamic-stateT st-op)
    (st:StateT
     (fn (s)
       (let result? =
         (try-dynamic
          (st:run-stateT st-op s)))
       (matchM result?
         ((Ok (Tuple s2 x))
          (pure (Tuple s2 (Ok x))))
         ((Err dyn-e)
          (pure (Tuple s (Err dyn-e))))))))

  (define-instance (MonadIoException :m => MonadIoException (st:StateT :s :m))
    (define raise (compose lift raise))
    (define raise-dynamic (compose lift raise-dynamic))
    (define reraise reraise-stateT)
    (define handle handle-stateT)
    (define handle-all handle-all-statet)
    (define try-dynamic try-dynamic-stateT))

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

  (inline)
  (declare reraise-envT (MonadIoException :m
                            => e:EnvT :e :m :a -> (Unit -> e:EnvT :e :m :b)
                            -> e:EnvT :e :m :a))
  (define (reraise-envT env-op env-handle-op)
    (e:EnvT
     (fn (env)
       (reraise
        (e:run-envT env-op env)
        (fn ()
          (e:run-envT
           (env-handle-op)
           env))))))

  (inline)
  (declare try-dynamic-envT (MonadIoException :m
                             => e:EnvT :e :m :a
                             -> e:EnvT :e :m (Result Dynamic :a)))
  (define (try-dynamic-envT env-op)
    (e:EnvT
     (fn (env)
       (let result? =
         (try-dynamic
          (e:run-envT env-op env)))
       (matchM result?
         ((Ok x)
          (pure (Ok x)))
         ((Err dyn-e)
          (pure (Err dyn-e)))))))

  (define-instance (MonadIoException :m => MonadIoException (e:EnvT :e :m))
    (define raise (compose lift raise))
    (define raise-dynamic (compose lift raise-dynamic))
    (define reraise reraise-envT)
    (define handle handle-envT)
    (define handle-all handle-all-envT)
    (define try-dynamic try-dynamic-envT))

  ;; TODO: Add instance for LoopT
  )

;;;
;;; Simple IO Instance
;;;

(coalton-toplevel
  (define-instance (MonadIoException io:IO)
    (define raise io:raise-io)
    (define raise-dynamic io:raise-dynamic-io)
    (define reraise io:reraise-io)
    (define handle io:handle-io)
    (define handle-all io:handle-all-io)
    (define try-dynamic io:try-dynamic-io))
  )

(cl:in-package :cl-user)
(defpackage :io/unlift
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:e #:coalton-library/monad/environment)
   (:st #:coalton-library/monad/statet)
   )
  (:export
   #:LiftIo
   #:lift-io_
   #:lift-io
   #:derive-lift-io

   #:UnliftIo
   #:with-run-in-io
   ))

(in-package :io/unlift)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Lift IO
;;;

(coalton-toplevel

  (define-class ((Monad :m) (BaseIo :i) => LiftIo :i :m)
    (lift-io (BaseIo :i => :i :a -> :m :a)))

  (define-instance (BaseIo :i => LiftIo :i :i)
    (inline)
    (define lift-io id)))

(cl:defmacro derive-lift-io (monad-param monadT-form)
  "Automatically derive an instance of LiftIo for a monad transformer.

Example:
  (derive-lift-io :m (env:EnvT :e :m))"
  `(define-instance ((LiftIo :i ,monad-param) => LiftIo :i ,monadT-form)
     (define lift-io (compose lift lift-io))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-lift-io :m (st:StateT :s :m))
  (derive-lift-io :m (e:EnvT :env :m))
  (derive-lift-io :m (LoopT :m))
  )

;;;
;;; Unlift IO
;;;

(coalton-toplevel
  ;; NOTE: Defining a "wrapper" around with-run-in-io so that we can specialize on it.
  (define-class ((MonadIo :m) (LiftIo :i :m) => UnliftIo :m :i (:m -> :i))
    (with-run-in-io (((:m :a -> :i :a) -> :i :b) -> :m :b)))

  (define-instance ((BaseIo :r) (UnliftIo :m :r) => UnliftIo (e:EnvT :env :m) :r)
    (inline)
    (define (with-run-in-io enva->ioa-->iob)
      (e:EnvT
       (fn (env)
         (with-run-in-io
           (fn (ma->ioa-->iob)
             (enva->ioa-->iob
              (fn (m-env)
               (ma->ioa-->iob
                (e:run-envT m-env env))))))))))
  )

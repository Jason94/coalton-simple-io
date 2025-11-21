(cl:in-package :cl-user)
(defpackage :io/unlift
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/utils
   #:io/monad-io
   #:io/simple-io)
  (:local-nicknames
   (:e #:coalton-library/monad/environment)
   )
  (:export
   #:MonadUnliftIo
   #:with-run-in-io
   ))

(in-package :io/unlift)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Unlift IO
;;;

(coalton-toplevel

  ;; TODO: Can we get this to work with any monad IO, not just IO?
  (define-class (MonadIO :m => MonadUnliftIO :m)
    (with-run-in-io (((:m :a -> IO :a) -> IO :b) -> :m :b)))

  (define-instance (MonadUnliftIO IO)
    (inline)
    (define (with-run-in-io inner)
      (inner id)))

  (define-instance (MonadUnliftIO :m => MonadUnliftIO (e:EnvT :env :m))
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

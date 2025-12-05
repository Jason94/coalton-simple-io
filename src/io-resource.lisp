(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io
   #:io/exception)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io)
   )
  (:export
   #:ExitCase
   #:Completed
   #:Errored

   #:with-resource
   #:with-resource_
   ))
(in-package :io/resource)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: This package is largely based on the Cats bracket & resource types.
;; See https://typelevel.org/cats-effect/docs/std/resource.

(coalton-toplevel

  (derive Eq)
  (repr :lisp)
  (define-type (ExitCase :e)
    "Signals the exit condition for an effectful computation using some resource."
    Completed
    (Errored :e))

  ;; NOTE: AKA `bracket` in Haskell/Scala Cats/etc.
  (declare with-resource ((MonadIoException :m) (RuntimeRepr :e)
                          => :m :r
                          -> (:r -> ExitCase :e -> :m :a)
                          -> (:r -> :m :b)
                          -> :m :b))
  (define (with-resource acquire-op release-op computation-op)
    "First, acquire a resource with ACQUIRE-OP. Then run COMPUTATION-OP with the
resource. Finally, run RELEASE-OP on the resource and ExitCase of the computation.
Guarantees that RELEASE-OP will be run regardless of if COMPUTATION-OP raises
an exception. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception,
then release is not guaranteed."
    (do
     (resource <- acquire-op)
     (result? <- (try (computation-op resource)))
     (do-match result?
       ((Ok result)
        (release-op resource Completed)
        (pure result))
       ((Err e)
        (release-op resource (Errored e))
        (raise e)))))

  (declare with-resource_ (MonadIoException :m
                          => :m :r
                          -> (:r -> :m :a)
                          -> (:r -> :m :b)
                          -> :m :b))
  (define (with-resource_ acquire-op release-op computation-op)
    "First, acquire a resource with ACQUIRE-OP. Then run COMPUTATION-OP with the
resource. Finally, run RELEASE-OP on the resource and ExitCase of the computation.
This version runs RELEASE-OP for any kind of error, and doesn't take an ExitCase."
    (do
     (resource <- acquire-op)
     (reraise (computation-op resource)
              (const (release-op resource)))))
  )

(cl:in-package :cl-user)
(defpackage :io/simple-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:local-nicknames
   (:r #:coalton-library/result)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:IO
   #:run-io!

   #:raise-io
   #:raise-io_
   #:raise-dynamic-io
   #:reraise-io
   #:handle-io
   #:handle-all-io
   #:try-dynamic-io

   #:uith-run-in-simple-io

   ;; Re-export the basic IO operations for usability, so that users
   ;; who want to use IO don't have to import two files.
   #:wrap-io
   #:wrap-io_
   #:map-into-io
   #:foreach-io
   #:do-map-into-io
   #:do-foreach-io
   ))
(in-package :io/simple-io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;
  ;; IO Monad
  ;;
  (repr :transparent)
  (define-type (IO :a)
    (IO% (Unit -> Result Dynamic :a)))

  (inline)
  (declare wrap-io%_ ((Unit -> :a) -> IO :a))
  (define (wrap-io%_ f)
    (IO%
     (fn ()
       (inline
        (r:map-err to-dynamic
                   (catch-thunk f))))))

  (inline)
  (declare run-io!% (IO :a -> Result Dynamic :a))
  (define (run-io!% (IO% f->a?))
    (f->a?))

  (inline)
  (declare run-io! (IO :a -> :a))
  (define (run-io! (IO% fa?))
    (match (fa?)
      ((Ok a)
       a)
      ((Err e)
       (error (force-string e)))))

  (define-instance (Functor IO)
    (define (map fb->c io-op)
      (match io-op
        ((IO% funit->b)
         (IO%
          (fn ()
            (map fb->c (funit->b))))))))

  (define-instance (Applicative IO)
    (inline)
    (define (pure x) (IO% (fn () (Ok x))))
    (inline)
    (define (liftA2 fa->b->c (IO% f->a?) (IO% f->b?))
      (IO%
       (fn ()
        (match (f->a?)
          ((Err e1)
           (Err e1))
          ((Ok a)
           (match (f->b?)
             ((Err e2)
              (Err e2))
             ((Ok b)
              (Ok (fa->b->c a b))))))))))

  (define-instance (Monad IO)
    (inline)
    (define (>>= (IO% f->a?) fa->io-b)
      (IO%
       (fn ()
        (match (f->a?)
          ((Err e)
           (Err e))
          ((Ok a)
           (run-io!% (fa->io-b a))))))))

  (inline)
  (declare raise-io ((RuntimeRepr :e) (Signalable :e) => :e -> IO :a))
  (define (raise-io e)
    (IO% (fn () (Err (to-dynamic e)))))

  (inline)
  (declare raise-dynamic-io (Dynamic -> IO :a))
  (define (raise-dynamic-io dyn)
    (IO% (fn () (Err dyn))))

  (inline)
  (declare raise-io_ ((RuntimeRepr :e) (Signalable :e) => :e -> IO Unit))
  (define raise-io_ raise-io)

  (inline)
  (declare reraise-io (IO :a -> (Unit -> IO :b) -> IO :a))
  (define (reraise-io op catch-op)
    (IO%
     (fn ()
       (let result = (run-io!% op))
       (do-match result
         ((Ok _)
          result)
         ((Err _)
          (let result2 = (run-io!% (catch-op)))
          (match result2
            ((Ok _)
             result)
            ((Err e)
             (Err e))))))))

  (inline)
  (declare handle-io (RuntimeRepr :e => IO :a -> (:e -> IO :a) -> IO :a))
  (define (handle-io io-op handle-op)
    (IO%
     (fn ()
      (let ((result (run-io!% io-op)))
        (match result
          ((Ok a)
           (Ok a))
          ((Err e?)
           (match (cast e?)
             ((Some e)
              (run-io!% (handle-op e)))
             ((None)
              result))))))))

  (inline)
  (declare handle-all-io (IO :a -> (Unit -> IO :a) -> IO :a))
  (define (handle-all-io io-op handle-op)
    "Run IO-OP, and run HANDLE-OP to handle exceptions of any type thrown by IO-OP."
    (IO%
     (fn ()
      (let ((result (run-io!% io-op)))
        (match result
          ((Ok a)
           (Ok a))
          ((Err _)
           (run-io!% (handle-op))))))))

  (inline)
  (declare try-dynamic-io (IO :a -> IO (Result Dynamic :a)))
  (define (try-dynamic-io io-op)
    (IO%
     (fn ()
       (Ok
        (run-io!% io-op)))))

  (define-instance (BaseIo IO)
    (define run! run-io!))

  (define-instance (UnliftIo IO IO)
    (inline)
    (define (with-run-in-io inner)
      (inner id)))

  (declare with-run-in-simple-io (UnliftIo :m IO => (((:m :a -> IO :a) -> IO :b) -> :m :b)))
  (define with-run-in-simple-io
    "`with-run-in-io`, but pegged to the simple-io implementation. Useful when you
need to unlift, run, then immediately re-run a function. See, e.g., io-file:with-open-file%."
    with-run-in-io)

  ;;
  ;; MonadIo Instances
  ;;

  (define-instance (MonadIo IO)
    (inline)
    (define wrap-io_ wrap-io%_)))

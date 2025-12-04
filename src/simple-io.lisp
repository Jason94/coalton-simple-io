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
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:IO
   #:run-io!

   #:raise-io
   #:raise-io_
   #:handle-io
   #:handle-all-io

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
    (define (pure x) (IO% (const (Ok x))))
    (inline)
    (define (liftA2 fa->b->c (IO% f->a?) (IO% f->b?))
      (IO%
       (const
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
       (const
        (match (f->a?)
          ((Err e)
           (Err e))
          ((Ok a)
           (run-io!% (fa->io-b a))))))))

  (inline)
  (declare raise-io (RuntimeRepr :e => :e -> IO :a))
  (define (raise-io e)
    (IO% (const (Err (to-dynamic e)))))

  (inline)
  (declare raise-io_ (RuntimeRepr :e => :e -> IO Unit))
  (define raise-io_ raise-io)

  (inline)
  (declare handle-io (RuntimeRepr :e => IO :a -> (:e -> IO :a) -> IO :a))
  (define (handle-io io-op handle-op)
    (IO%
     (const
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
     (const
      (let ((result (run-io!% io-op)))
        (match result
          ((Ok a)
           (Ok a))
          ((Err _)
           (run-io!% (handle-op))))))))

  (define-instance (BaseIo IO)
    (define run! run-io!))

  ;;
  ;; MonadIo Instances
  ;;

  (define-instance (MonadIo IO)
    (inline)
    (define (wrap-io_ f) (IO% (map Ok f)))))

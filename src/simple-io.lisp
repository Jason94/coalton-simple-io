(cl:in-package :cl-user)
(defpackage :io/simple-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
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
   #:try-io
   #:handle-io

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
  (define-type (IO :a)
    (IO% (Unit -> :a))
    (ErrorIO% String))

  (inline)
  (declare raise-io (String -> IO :a))
  (define (raise-io err)
    (ErrorIO% err))

  (inline)
  (declare raise-io_ (String -> IO Unit))
  (define raise-io_ raise-io)

  (inline)
  (declare try-io (IO :a -> IO (Result String :a)))
  (define (try-io io-op)
    (match io-op
      ((IO% f)
       (IO% (map Ok f)))
      ((ErrorIo% e)
       (IO% (const (Err e))))))

  (inline)
  (declare handle-io (IO :a -> (String -> IO :a) -> IO :a))
  (define (handle-io io-op handle-op)
    (match io-op
      ((IO% _)
       io-op)
      ((ErrorIo% e)
       (handle-op e))))

  (inline)
  (declare run-io! (IO :a -> :a))
  (define (run-io! io-op)
    (match io-op
      ((IO% funit->a)
       (funit->a))
      ((ErrorIO% e)
       (error e))))

  (define-instance (Functor IO)
    (define (map fb->c io-op)
      (match io-op
        ((IO% funit->b)
         (IO%
          (fn ()
            (fb->c (funit->b)))))
        ((ErrorIO% e)
         (ErrorIO% e)))))

  (define-instance (Applicative IO)
    (inline)
    (define pure (compose IO% const))
    (inline)
    (define (liftA2 fa->b->c io-a io-b)
      (match (Tuple io-a io-b)
        ((Tuple (IO% f->a) (IO% f->b))
         (IO%
          (fn ()
            (fa->b->c (f->a) (f->b)))))
        ((Tuple (ErrorIO% e) _)
         (ErrorIO% e))
        ((Tuple _ (ErrorIO% e))
         (ErrorIO% e)))))

  (define-instance (Monad IO)
    (inline)
    (define (>>= io-op fa->io-b)
      (match io-op
        ((IO% f->a)
         (IO%
          (fn ()
            (run-io! (fa->io-b (f->a))))))
        ((ErrorIO% e)
         (ErrorIO% e)))))

  (define-instance (BaseIo IO)
    (define run! run-io!))

  ;;
  ;; MonadIo Instances
  ;;

  (define-instance (MonadIo IO)
    (define wrap-io_ IO%)))

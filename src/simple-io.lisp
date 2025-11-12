(cl:in-package :cl-user)
(defpackage :io/simple-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
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
    (IO% (Unit -> :a)))

  (inline)
  (declare run-io! (IO :a -> :a))
  (define (run-io! (IO% funit->a))
    (funit->a))
  (define-instance (Functor IO)
    (inline)
    (define (map fb->c (IO% funit->b))
      (IO%
       (fn ()
         (fb->c (funit->b))))))

  (define-instance (Applicative IO)
    (inline)
    (define pure (compose IO% const))
    (inline)
    (define (liftA2 fa->b->c (IO% f->a) (IO% f->b))
      (IO%
       (fn ()
         (fa->b->c (f->a) (f->b))))))

  (define-instance (Monad IO)
    (inline)
    (define (>>= (IO% f->a) fa->io-b)
      (IO%
       (fn ()
         (run-io! (fa->io-b (f->a)))))))

  (define-instance (RunIo IO)
    (define run! run-io!))

  ;;
  ;; MonadIo Instances
  ;;

  (define-instance (MonadIo IO)
    (define wrap-io_ IO%)))

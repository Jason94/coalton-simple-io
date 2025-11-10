(cl:in-package :cl-user)
(defpackage :simple-io/io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils)
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
   ))
(in-package :simple-io/io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;
  ;; IO Monad
  ;;
  (repr :transparent)
  (define-type (IO :a)
    (IO% (Unit -> :a)))

  (inline)
  (declare run!% (IO :a -> :a))
  (define (run!% (IO% funit->a))
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
         (run!% (fa->io-b (f->a)))))))

  (define-instance (MonadIo IO)
    (define wrap-io_ IO%)
    (define map-into-io map-into-io%)
    (define foreach-io foreach-io%))

  (define-instance (RunIo IO)
    (define run! run!%)))

(cl:in-package :cl-user)
(defpackage :simple-io/io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions)
  (:export
   #:wrap-io
   #:IO
   #:run!))
(in-package :simple-io/io)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro wrap-io (cl:&body body)
  "Wrap the execution of BODY in the IO monad.

Example:
  (wrap-io
    (lisp :a (str)
      (cl:print str)))"
  `(IO% (fn () ,@body)))

(coalton-toplevel
  ;;
  ;; IO Monad
  ;;
  (repr :transparent)
  (define-type (IO :a)
    (IO% (Unit -> :a)))

  (declare run! (IO :a -> :a))
  (define (run! (IO% funit->a))
    (funit->a))

  (define-instance (Functor IO)
    (define (map fb->c (IO% funit->b))
      (IO%
        (fn ()
          (fb->c (funit->b))))))

  (define-instance (Applicative IO)
    (define pure (compose IO% const))
    (define (liftA2 fa->b->c (IO% f->a) (IO% f->b))
      (IO%
        (fn ()
          (fa->b->c (f->a) (f->b))))))

  (define-instance (Monad IO)
    (define (>>= (IO% f->a) fa->io-b)
      (IO%
        (fn ()
          (run! (fa->io-b (f->a))))))))

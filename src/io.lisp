(cl:in-package :cl-user)
(defpackage :simple-io/io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions)
  (:local-nicknames
   (:c #:coalton-library/cell))
  (:export
   #:wrap-io
   #:IO
   #:run!

   #:IORef
   #:new-io-ref
   #:read
   #:write
   #:modify
   ))
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

  (inline)
  (declare run! (IO :a -> :a))
  (define (run! (IO% funit->a))
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
         (run! (fa->io-b (f->a))))))))

(coalton-toplevel
  (repr :transparent)
  (define-type (IORef :a)
    (IORef% (Cell :a)))

  (declare new-io-ref (:a -> IO (IORef :a)))
  (define (new-io-ref val)
    (wrap-io (IORef% (c:new val))))

  (declare read (IORef :a -> IO :a))
  (define (read (IORef% cel))
    (wrap-io (c:read cel)))

  (declare write (IORef :a -> :a -> IO :a))
  (define (write (IORef% cel) val)
    "Set the value in an IORef and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (declare modify (IORef :a -> (:a -> :a) -> IO :a))
  (define (modify (IORef% cel) f)
    "Modify the value in an IORef and return the old value."
    (wrap-io (c:update-swap! f cel))))

(cl:in-package :cl-user)
(defpackage :simple-io/io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:local-nicknames
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:wrap-io
   #:IO
   #:run!
   #:map-into-io
   #:do-map-into-io
   #:foreach-io
   #:do-foreach-io

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

  (inline)
  (declare map-into-io (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO (List :b)))
  (define (map-into-io itr a->iob)
    "Efficiently perform an IO operation for each element of an iterator and
return the results."
    (IO%
     (fn ()
       (let results = (c:new (make-list)))
       (for a in (it:into-iter itr)
         (c:push! results (run! (a->iob a))))
       (reverse (c:read results)))))

  (inline)
  (declare foreach-io (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO Unit))
  (define (foreach-io itr a->iob)
    "Efficiently perform an IO operation for each element of an iterator."
    (IO%
     (fn ()
       (for a in (it:into-iter itr)
         (run! (a->iob a))))))

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

(cl:defmacro do-map-into-io ((var lst) cl:&body body)
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(cl:defmacro do-foreach-io ((var lst) cl:&body body)
  `(foreach-io ,lst
     (fn (,var)
       (do
        ,@body))))

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

(cl:in-package :cl-user)
(defpackage :simple-io/io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:wrap-io
   #:IO
   #:run!

   #:MonadIo
   #:derive-monad-io
   #:map-into-io
   #:do-map-into-io
   #:foreach-io
   #:do-foreach-io
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

  (define-class (Monad :m => MonadIo :m)
    (map-into-io (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> :m (List :b)))
    (foreach-io (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> :m Unit)))

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
  (declare map-into-io% (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO (List :b)))
  (define (map-into-io% itr a->iob)
    "Efficiently perform an IO operation for each element of an iterator and
return the results."
    (IO%
     (fn ()
       (let results = (c:new (make-list)))
       (for a in (it:into-iter itr)
         (c:push! results (run! (a->iob a))))
       (reverse (c:read results)))))

  (inline)
  (declare foreach-io% (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO Unit))
  (define (foreach-io% itr a->iob)
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
         (run! (fa->io-b (f->a)))))))

  (define-instance (MonadIo IO)
    (define map-into-io map-into-io%)
    (define foreach-io foreach-io%)))

(cl:defmacro derive-monad-io (monadT-form)
  "Automatically derive an instance of MonadIo for a monad transformer.

Example:
  (derive-monad-io (st:StateT :s :m))"
  `(define-instance (MonadIo :m => MonadIo ,monadT-form)
     (define map-into-io (compose2 lift map-into-io))
     (define foreach-io (compose2 lift foreach-io))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io (st:StateT :s :m))
  (derive-monad-io (env:EnvT :env :m))
  )

;;
;; Syntactic Sugar Macros
;;

(cl:defmacro do-map-into-io ((var lst) cl:&body body)
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(cl:defmacro do-foreach-io ((var into-itr) cl:&body body)
  `(foreach-io ,into-itr
     (fn (,var)
       (do
        ,@body))))


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
   #:wrap-io_

   #:MonadIo
   #:derive-monad-io
   #:wrap-io
   #:map-into-io
   #:foreach-io

   #:do-map-into-io
   #:do-foreach-io

   #:RunIo
   #:run!
   #:run-as!
   ))
(in-package :simple-io/io)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro wrap-io (cl:&body body)
  "Wrap the execution of BODY in the IO monad.
Supports any MonadIo instance.

Example:
  (wrap-io
    (lisp :a (str)
      (cl:print str)))"
  `(wrap-io_ (fn () ,@body)))

(coalton-toplevel

  (define-class (Monad :m => MonadIo :m)
    (wrap-io_
     "Wrap a (potentially) side-effectful function in the monad."
     ((Unit -> :a) -> :m :a))
    (map-into-io
     "Efficiently perform an IO operation for each element of an iterator and
return the results."
     (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> :m (List :b)))
    (foreach-io
     "Efficiently perform an IO operation for each element of an iterator."
     (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> :m Unit)))

  (define-class (MonadIo :m => RunIo :m)
    "A MonadIo operation that can be run to return its output."
    (run!
     "Run a (potentially) side-effectful operation."
     (:m :a -> :a)))

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

  (inline)
  (declare map-into-io% (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO (List :b)))
  (define (map-into-io% itr a->iob)
    "Efficiently perform an IO operation for each element of an iterator and
return the results."
    (IO%
     (fn ()
       (let results = (c:new (make-list)))
       (for a in (it:into-iter itr)
         (c:push! results (run!% (a->iob a))))
       (reverse (c:read results)))))

  (inline)
  (declare foreach-io% (it:IntoIterator :i :a => :i -> (:a -> IO :b) -> IO Unit))
  (define (foreach-io% itr a->iob)
    "Efficiently perform an IO operation for each element of an iterator."
    (IO%
     (fn ()
       (for a in (it:into-iter itr)
         (run!% (a->iob a))))))

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

(cl:defmacro derive-monad-io (monad-param monadT-form)
  "Automatically derive an instance of MonadIo for a monad transformer.

Example:
  (derive-monad-io :m (st:StateT :s :m))"
  `(define-instance (MonadIo ,monad-param => MonadIo ,monadT-form)
     (define wrap-io_ (compose lift wrap-io_))
     (define map-into-io (compose2 lift map-into-io))
     (define foreach-io (compose2 lift foreach-io))))

(cl:defmacro run-as! (m-type m-op)
  "Run M-OP using the concrete RunIo M-TYPE. Useful for situations where
you want to create a generic MonadIo operation and immediately run it,
so the compiler can't infer the type of the actual monad you want to use.

Example:
  (run-as! (IO Unit) (pure Unit))

NOTE: Unfortunately, there seems to be a type inference bug that requires
putting in the full type of M-OP, not just (IO :a).
"
  ;; NOTE: This should be fine, until Coalton gets scoped type variables.
  ;; Then we'll need to use a gensym to construct the keyword.
  ;; NOTE: This *should* work. See above.
  ;; `(run! (the (,m-type :a) ,m-op)))
  `(run! (the ,m-type ,m-op)))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io :m (st:StateT :s :m))
  (derive-monad-io :m (env:EnvT :env :m))
  (derive-monad-io :m (LoopT :m))
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


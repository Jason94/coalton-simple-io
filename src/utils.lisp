(cl:in-package :cl-user)
(defpackage :io/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   )
  (:export
   #:force-string
   #:compose2
   #:Dynamic
   #:to-dynamic
   #:cast
   ))
(in-package :io/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare force-string (:a -> String))
  (define (force-string x)
    (lisp String (x)
      (cl:format cl:nil "~a" x)))

  (declare compose2 ((:c -> :d) -> (:a -> :b -> :c) -> :a -> :b -> :d))
  (define (compose2 fcd fabc a b)
    (fcd (fabc a b)))

  (declare proxy-outer (Proxy :a -> Proxy (:m :a)))
  (define (proxy-outer _)
    Proxy)

  (repr :native cl:t)
  (define-type Anything)

  (define-type Dynamic
    (Dynamic% Anything LispType))

  (inline)
  (declare to-anything (:a -> Anything))
  (define (to-anything a)
    (lisp Anything (a)
      a))

  (inline)
  (declare to-dynamic (RuntimeRepr :a => :a -> Dynamic))
  (define (to-dynamic a)
    (Dynamic% (to-anything a) (runtime-repr-of a)))

  (declare cast (RuntimeRepr :b => Dynamic -> Optional :b))
  (define (cast (Dynamic% dyn-val dyn-repr))
    "Attempt to cast :a into a :b. WARNING: This will falsely cast :a into :b
if they are different Coalton types, but nonetheless have the same runtime
representation. To be safe, only use on types that have `(repr :lisp)`."
    (let prx-b = Proxy)
    (as-proxy-of
     (if (== dyn-repr
             (runtime-repr prx-b))
         (Some (lisp :b (dyn-val) dyn-val))
         None)
     (proxy-outer prx-b)))
  )

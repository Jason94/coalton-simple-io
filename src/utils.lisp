(cl:in-package :cl-user)
(defpackage :io/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/system
   #:coalton-library/types
   )
  (:export
   #:UnhandledError
   #:catch-thunk
   #:force-string
   #:compose2
   #:Dynamic
   #:to-dynamic
   #:cast
   #:throw-dynamic
   #:proxy-swap-inner
   ))
(in-package :io/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (repr :lisp)
  (define-type (UnhandledError :e)
    "An unhandled error that was thrown inside a wrap-io call."
    (UnhandledError :e))

  (define-instance (Signalable :e => Signalable (UnhandledError :e))
    (define (error (UnhandledError e))
      (error e)))

  (declare catch-thunk ((Unit -> :a) -> Result (UnhandledError :e) :a))
  (define (catch-thunk thunk)
    "Wraps `thunk` in a Lisp `handler-case`, and captures the output
as Err or Ok. Useful if you want to capture any thrown error, which is
currently not possible natively in Coalton. Works even with custom
Coalton exceptions via `define-exception`."
    ;; TODO: Test this in release mode...
    (lisp (Result (UnhandledError :e) :a) (thunk)
      (cl:handler-case (Ok (call-coalton-function thunk))
        (cl:error (e)
          (Err (UnhandledError e))))))

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

  ;;;
  ;;; Dynamic
  ;;;

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

  (declare throw-dynamic (Dynamic -> :a))
  (define (throw-dynamic (Dynamic% val _))
    "Throw the dynamic value. Will fail if it isn't a Signalable/LispCondition."
    (lisp :a (val)
      (cl:error val)))

  (declare proxy-swap-inner (Proxy (:m :a) -> Proxy (:m :b)))
  (define (proxy-swap-inner _)
    Proxy)
  )

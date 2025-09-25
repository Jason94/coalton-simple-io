(cl:in-package :cl-user)
(defpackage :simple-io/random
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io)
  (:export
   #:RandomLimit
   #:RandomState
   #:make-random-state
   #:copy-random-state
   #:get-current-random-state
   #:set-current-random-state
   #:random
   #:random_))
(in-package :simple-io/random)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Num :a => RandomLimit :a)
    "A number that can be used to bound a random number value.")

  (define-instance (RandomLimit UFix))
  (define-instance (RandomLimit F32))
  (define-instance (RandomLimit F64))

  (repr :native cl:random-state)
  (define-type RandomState)

  (declare make-random-state (IO RandomState))
  (define make-random-state
    (wrap-io (lisp :a ()
               (cl:make-random-state cl:t))))

  (declare copy-random-state (RandomState -> RandomState))
  (define (copy-random-state rs)
    (lisp :a (rs)
      (cl:make-random-state rs)))

  (declare get-current-random-state (IO RandomState))
  (define get-current-random-state
    (wrap-io (lisp :a ()
               cl:*random-state*)))

  (declare set-current-random-state (RandomState -> IO Unit))
  (define (set-current-random-state rs)
    (wrap-io
      (lisp :a (rs)
        (cl:setf cl:*random-state* rs))
      Unit))

  (declare random (RandomLimit :a => RandomState -> :a -> IO :a))
  (define (random rs limit)
    (wrap-io (lisp :a (rs limit)
               (cl:random limit rs))))

  (declare random_ (RandomLimit :a => :a -> IO :a))
  (define (random_ limit)
    (wrap-io (lisp :a (limit)
               (cl:random limit))))
  )

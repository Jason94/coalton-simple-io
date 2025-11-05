(cl:in-package :cl-user)
(defpackage :simple-io/random
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
  (:local-nicknames
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   #:MonadIoRandom
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

  ;;
  ;; IO Implementation
  ;;

  (declare make-random-state% (IO RandomState))
  (define make-random-state%
    (wrap-io (lisp :a ()
               (cl:make-random-state cl:t))))

  (declare copy-random-state (RandomState -> RandomState))
  (define (copy-random-state rs)
    (lisp :a (rs)
      (cl:make-random-state rs)))

  (declare get-current-random-state% (IO RandomState))
  (define get-current-random-state%
    (wrap-io (lisp :a ()
               cl:*random-state*)))

  (declare set-current-random-state% (RandomState -> IO Unit))
  (define (set-current-random-state% rs)
    (wrap-io
      (lisp :a (rs)
        (cl:setf cl:*random-state* rs))
      Unit))

  (declare random% (RandomLimit :a => RandomState -> :a -> IO :a))
  (define (random% rs limit)
    (wrap-io (lisp :a (rs limit)
               (cl:random limit rs))))

  (declare random_% (RandomLimit :a => :a -> IO :a))
  (define (random_% limit)
    (wrap-io (lisp :a (limit)
               (cl:random limit))))

  ;;
  ;; MonadIoRandom Interface
  ;;

  (define-class (Monad :m => MonadIoRandom :m)
    (make-random-state (:m RandomState))
    (get-current-random-state (:m RandomState))
    (set-current-random-state (RandomState -> :m Unit))
    (random (RandomLimit :a => RandomState -> :a -> :m :a))
    (random_ (RandomLimit :a => :a -> :m :a)))

  (define-instance (MonadIoRandom IO)
    (define make-random-state make-random-state%)
    (define get-current-random-state get-current-random-state%)
    (define set-current-random-state set-current-random-state%)
    (define random random%)
    (define random_ random_%))

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (define-instance ((MonadIoRandom :m) => MonadIoRandom (st:StateT :s :m))
    (define make-random-state (lift make-random-state))
    (define get-current-random-state (lift get-current-random-state))
    (define set-current-random-state (compose lift set-current-random-state))
    (define random (compose2 lift random))
    (define random_ (compose lift random_)))

  (define-instance ((MonadIoRandom :m) => MonadIoRandom (env:EnvT :e :m))
    (define make-random-state (lift make-random-state))
    (define get-current-random-state (lift get-current-random-state))
    (define set-current-random-state (compose lift set-current-random-state))
    (define random (compose2 lift random))
    (define random_ (compose lift random_))))

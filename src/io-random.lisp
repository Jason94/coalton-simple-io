(cl:in-package :cl-user)
(defpackage :simple-io/random
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:simple-io/io))
  (:export
   #:MonadIoRandom
   #:derive-monad-io-random
   #:RandomState
   #:make-random-state
   #:copy-random-state
   #:get-current-random-state
   #:set-current-random-state
   #:random
   #:random_
   #:implement-monad-io-random
   ))
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

  (declare make-random-state% (MonadIo :m => :m RandomState))
  (define make-random-state%
    (wrap-io (lisp :a ()
               (cl:make-random-state cl:t))))

  (declare copy-random-state (MonadIo :m => RandomState -> :m RandomState))
  (define (copy-random-state rs)
    (wrap-io (lisp :a (rs)
               (cl:make-random-state rs))))

  (declare get-current-random-state% (MonadIo :m => :m RandomState))
  (define get-current-random-state%
    (wrap-io (lisp :a ()
               cl:*random-state*)))

  (declare set-current-random-state% (MonadIo :m => RandomState -> :m Unit))
  (define (set-current-random-state% rs)
    (wrap-io
      (lisp :a (rs)
        (cl:setf cl:*random-state* rs))
      Unit))

  (declare random% ((RandomLimit :a) (MonadIo :m) => RandomState -> :a -> :m :a))
  (define (random% rs limit)
    (wrap-io (lisp :a (rs limit)
               (cl:random limit rs))))

  (declare random_% ((RandomLimit :a) (MonadIo :m) => :a -> :m :a))
  (define (random_% limit)
    (wrap-io (lisp :a (limit)
               (cl:random limit))))

  ;;
  ;; MonadIoRandom Interface
  ;;

  (define-class (Monad :m => MonadIoRandom :m)
    (make-random-state
     "Create a fresh random state."
     (:m RandomState))
    (get-current-random-state
     "Get the current thread's random state."
     (:m RandomState))
    (set-current-random-state
     "Set the current thread's random state."
     (RandomState -> :m Unit))
    (random
     "Generate a random value less than LIMIT using the given random state."
     (RandomLimit :a => RandomState -> :a -> :m :a))
    (random_
     "Generate a random value less than LIMIT using the current random state."
     (RandomLimit :a => :a -> :m :a))))

(cl:defmacro implement-monad-io-random (monad)
  `(define-instance (MonadIoRandom ,monad)
     (define make-random-state make-random-state%)
     (define get-current-random-state get-current-random-state%)
     (define set-current-random-state set-current-random-state%)
     (define random random%)
     (define random_ random_%)))

(cl:defmacro derive-monad-io-random (monad-param monadT-form)
  "Automatically derive an instance of MonadIoRandom for a monad transformer.

Example:
  (derive-monad-io-random :m (st:StateT :s :m))"
  `(define-instance (MonadIoRandom ,monad-param => MonadIoRandom ,monadT-form)
     (define make-random-state (lift make-random-state))
     (define get-current-random-state (lift get-current-random-state))
     (define set-current-random-state (compose lift set-current-random-state))
     (define random (compose2 lift random))
     (define random_ (compose lift random_))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-random :m (st:StateT :s :m))
  (derive-monad-io-random :m (env:EnvT :e :m))
  (derive-monad-io-random :m (LoopT :m)))

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-random io:IO))

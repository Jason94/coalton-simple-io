(cl:in-package :cl-user)
(defpackage :io/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:c   #:coalton-library/cell)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/io))
  (:export
   #:MVar

   #:MonadIoMVar
   #:derive-monad-io-mvar

   #:new-mvar
   #:new-empty-mvar

   #:take-mvar
   #:put-mvar
   #:try-take-mvar
   #:try-put-mvar
   #:read-mvar
   #:try-read-mvar
   #:swap-mvar
   #:is-empty-mvar

   #:with-mvar
   #:do-with-mvar

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   
   #:implement-monad-io-mvar
   ))
(in-package :io/mvar)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; MVar
;;;

(coalton-toplevel
  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold one :a.
Can put data into the container, blocking until it is empty.
Can take data out of the container, blocking until it is full."
    (lock lk:Lock)
    (cvar cv:ConditionVariable)
    (data (c:Cell (Optional :a))))

  (define-class (MonadIo :m => MonadIoMVar :m)
    (new-mvar
     "Create a new MVar containing an initial value."
     (:a -> :m (MVar :a)))
    (new-empty-mvar
     "Create a new empty MVar."
     (:m (MVar :a)))
    (take-mvar
     "Take a value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (put-mvar
     "Put a value into an MVar, blocking until it becomes empty."
     (MVar :a -> :a -> :m Unit))
    (try-take-mvar
     "Attempt to take a value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (try-put-mvar
     "Attempt to put a value into an MVar; returns False if full and the put fails,
True if the put succeeds."
     (MVar :a -> :a -> :m Boolean))
    (read-mvar
     "Read (without removing) the value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (try-read-mvar
     "Attempt to read (without removing) the value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (swap-mvar
     "Atomically replace the value in an MVar and return the old value."
     (MVar :a -> :a -> :m :a))
    (is-empty-mvar
     "Return True if the MVar is currently empty."
     (MVar :a -> :m Boolean))
    (with-mvar
     "Perform an IO action with the value from an MVar, without removing
the value. Blocks until the MVar is full."
     (RunIo :r => MVar :a -> (:a -> :r :b) -> :m :b)))

  (inline)
  (declare new-mvar% (MonadIo :m => :a -> :m (MVar :a)))
  (define (new-mvar% val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new (Some val)))))

  (inline)
  (declare new-empty-mvar% (MonadIo :m => :m (MVar :a)))
  (define new-empty-mvar%
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new None))))

  ;; TODO: Find a way to do error handling here.
  (declare take-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (take-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some x)
                     (c:write! (.data mvar) None)
                     (lk:release (.lock mvar))
                     (cv:notify (.cvar mvar))
                     x)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  ;; TODO: Find a way to do error handling here
  (declare put-mvar% (MonadIo :m => MVar :a -> :a -> :m Unit))
  (define (put-mvar% mvar val)
    (wrap-io
      (let lock = (.lock mvar))
      (let data = (.data mvar))
      (let cvar = (.cvar mvar))
      (lk:acquire lock)
      (let ((lp (fn ()
                  (match (c:read data)
                    ((None)
                     (c:write! data (Some val))
                     (lk:release lock)
                     (cv:notify cvar)
                     Unit)
                    ((Some _)
                     (cv:await cvar lock)
                     (lp))))))
        (lp))))

  ;; TODO: Find a way to do error handling here
  (declare try-take-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some x)
         (c:write! (.data mvar) None)
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         (Some x))
        ((None)
         (lk:release (.lock mvar))
         None))))

  ;; TODO: Find a way to do error handling here
  (declare try-put-mvar% (MonadIo :m => MVar :a -> :a -> :m Boolean))
  (define (try-put-mvar% mvar val)
    (wrap-io
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some _)
         (lk:release (.lock mvar))
         False)
        ((None)
         (c:write! (.data mvar) (Some val))
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         True))))

  ;; TODO: Find a way to do error handling here
  (declare read-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (read-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some x)
                     (lk:release (.lock mvar))
                     x)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  ;; TODO: Does this work without any locking at all?
  (declare try-read-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-read-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some x)
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         (Some x))
        ((None)
         (lk:release (.lock mvar))
         None))))

  ;; TODO: Find a way to do error handling here
  (declare swap-mvar% (MonadIo :m => MVar :a -> :a -> :m :a))
  (define (swap-mvar% mvar new-val)
    (wrap-io
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some old-val)
                     (c:write! (.data mvar) (Some new-val))
                     (lk:release (.lock mvar))
                     (cv:notify (.cvar mvar))
                     old-val)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  (declare is-empty-mvar% (MonadIo :m => MVar :a -> :m Boolean))
  (define (is-empty-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (let result =
        (match (c:read (.data mvar))
          ((Some _) False)
          ((None)   True)))
      (lk:release (.lock mvar))
      result))

  (declare with-mvar% ((RunIo :r) (MonadIo :m) => MVar :a -> (:a -> :r :b) -> :m :b))
  (define (with-mvar% mvar op)
    (do
      (x <- (read-mvar% mvar))
      (result <-
       (wrap-io (run! (op x))))
      (pure result))))

(cl:defmacro implement-monad-io-mvar (monad)
  `(define-instance (MonadIoMVar ,monad)
     (define new-mvar       new-mvar%)
     (define new-empty-mvar new-empty-mvar%)
     (define take-mvar      take-mvar%)
     (define put-mvar       put-mvar%)
     (define try-take-mvar  try-take-mvar%)
     (define try-put-mvar   try-put-mvar%)
     (define read-mvar      read-mvar%)
     (define try-read-mvar  try-read-mvar%)
     (define swap-mvar      swap-mvar%)
     (define is-empty-mvar  is-empty-mvar%)
     (define with-mvar      with-mvar%)))

(cl:defmacro derive-monad-io-mvar (monad-param monadT-form)
  "Automatically derive MonadIoMVar for a monad transformer.

Example:
  (derive-monad-io-mvar :m (st:StateT :s :m))"
  `(define-instance (MonadIoMVar ,monad-param => MonadIoMVar ,monadT-form)
     (define new-mvar       (compose lift new-mvar))
     (define new-empty-mvar (lift new-empty-mvar))
     (define take-mvar      (compose lift take-mvar))
     (define put-mvar       (compose2 lift put-mvar))
     (define try-take-mvar  (compose lift try-take-mvar))
     (define try-put-mvar   (compose2 lift try-put-mvar))
     (define read-mvar      (compose lift read-mvar))
     (define try-read-mvar  (compose lift try-read-mvar))
     (define swap-mvar      (compose2 lift swap-mvar))
     (define is-empty-mvar  (compose lift is-empty-mvar))
     (define with-mvar      (compose2 lift with-mvar))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;
  (derive-monad-io-mvar :m (st:StateT :s :m))
  (derive-monad-io-mvar :m (env:EnvT :e :m))
  (derive-monad-io-mvar :m (LoopT :m))
  )

(cl:defmacro do-with-mvar ((sym mvar) cl:&body body)
  `(with-mvar
     ,mvar
     (fn (,sym)
       (do
        ,@body))))

;;;
;;; MChan
;;;

(coalton-toplevel
  (define-type (ChanNode :a)
    (ChanNode% :a (MVar (ChanNode :a))))

  (define-struct (MChan :a)
    "A synchronized FIFO queue to pass data directionally between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  (declare new-empty-chan (MonadIoMVar :m => :m (MChan :a)))
  (define new-empty-chan
    "Create a new empty channel."
    (do
     (cell <- new-empty-mvar)
     (head-var <- (new-mvar cell))
     (tail-var <- (new-mvar cell))
     (pure (MChan head-var tail-var))))

  (declare push-chan (MonadIoMVar :m => MChan :a -> :a -> :m Unit))
  (define (push-chan chan val)
    "Push VAL onto CHAN."
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar (.tail-var chan)))
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var chan) new-tail-var)))

  (declare pop-chan (MonadIoMVar :m => MChan :a -> :m :a))
  (define (pop-chan chan)
    "Pop the front value in CHAN. Blocks while CHAN is empty."
    (do
     (old-tail-var <- (take-mvar (.head-var chan)))
     ((ChanNode% val new-head-var) <- (take-mvar old-tail-var))
     (put-mvar (.head-var chan) new-head-var)
     (pure val)))
  )

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-mvar io:IO))

(cl:in-package :cl-user)
(defpackage :simple-io/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
  (:local-nicknames
   (:c   #:coalton-library/cell)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
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
   #:swap-mvar
   #:is-empty-mvar

   #:with-mvar
   #:do-with-mvar

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   ))
(in-package :simple-io/mvar)

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

  (define-class (Monad :m => MonadIoMVar :m)
    (new-mvar        (:a -> :m (MVar :a)))
    (new-empty-mvar  (:m (MVar :a)))
    (take-mvar       (MVar :a -> :m :a))
    (put-mvar        (MVar :a -> :a -> :m Unit))
    (try-take-mvar   (MVar :a -> :m (Optional :a)))
    (try-put-mvar    (MVar :a -> :a -> :m Boolean))
    (read-mvar       (MVar :a -> :m :a))
    (swap-mvar       (MVar :a -> :a -> :m :a))
    (is-empty-mvar   (MVar :a -> :m Boolean))
    (with-mvar       (MVar :a -> (:a -> IO :b) -> :m :b)))

  (inline)
  (declare new-mvar% (:a -> IO (MVar :a)))
  (define (new-mvar% val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new (Some val)))))

  (inline)
  (declare new-empty-mvar% (IO (MVar :a)))
  (define new-empty-mvar%
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new None))))

  ;; TODO: Find a way to do error handling here.
  (declare take-mvar% (MVar :a -> IO :a))
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
  (declare put-mvar% (MVar :a -> :a -> IO Unit))
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
  (declare try-take-mvar% (MVar :a -> IO (Optional :a)))
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
  (declare try-put-mvar% (MVar :a -> :a -> IO Boolean))
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
  (declare read-mvar% (MVar :a -> IO :a))
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

  ;; TODO: Find a way to do error handling here
  (declare swap-mvar% (MVar :a -> :a -> IO :a))
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

  (declare is-empty-mvar% (MVar :a -> IO Boolean))
  (define (is-empty-mvar% mvar)
    (wrap-io
      (lk:acquire (.lock mvar))
      (let result =
        (match (c:read (.data mvar))
          ((Some _) False)
          ((None)   True)))
      (lk:release (.lock mvar))
      result))

  (declare with-mvar% (MVar :a -> (:a -> IO :b) -> IO :b))
  (define (with-mvar% mvar op)
    (do
      (x <- (read-mvar% mvar))
      (result <- (op x))
      (pure result)))

  (define-instance (MonadIoMVar IO)
    (define new-mvar       new-mvar%)
    (define new-empty-mvar new-empty-mvar%)
    (define take-mvar      take-mvar%)
    (define put-mvar       put-mvar%)
    (define try-take-mvar  try-take-mvar%)
    (define try-put-mvar   try-put-mvar%)
    (define read-mvar      read-mvar%)
    (define swap-mvar      swap-mvar%)
    (define is-empty-mvar  is-empty-mvar%)
    (define with-mvar      with-mvar%))
  )

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
     (define swap-mvar      (compose2 lift swap-mvar))
     (define is-empty-mvar  (compose lift is-empty-mvar))
     (define with-mvar      (compose2 lift with-mvar))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;
  (derive-monad-io-mvar :m (st:StateT :s :m))
  (derive-monad-io-mvar :m (env:EnvT :e :m))
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

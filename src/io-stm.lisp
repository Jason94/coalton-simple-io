(cl:in-package :cl-user)
(defpackage :io/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/monad-io
   #:io/exception
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:a #:coalton-threads/atomic))
  (:export)
  )
(in-package :io/stm)

(cl:defmacro mem-barrier ()
  `(lisp Void ()
     (sb-thread:barrier (:read))))

(named-readtables:in-readtable coalton:coalton)

;; This is an implementation of the NOrec STM algorithm, described:
;; https://pages.cs.wisc.edu/~markhill/restricted/757/ppopp10_norec.pdf

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type (TVar :a)
    (TVar% (c:Cell :a)))

  (inline)
  (declare unwrap-tvar% (TVar :a -> c:Cell :a))
  (define (unwrap-tvar% (TVar% a))
    a)

  (inline)
  (declare set-tvar% (TVar :a -> :a -> Unit))
  (define (set-tvar% tvar val)
    (c:write! (unwrap-tvar% tvar) val)
    Unit)

  (inline)
  (declare tvar-value% (TVar :a -> :a))
  (define (tvar-value% tvar)
    (c:read (unwrap-tvar% tvar)))

  ;; NOTE: For now, using the coalton-threads Atomic Integer instead of
  ;; our atomics, because it's probably faster, since our atomic type doesn't
  ;; have a way to use sb-ext:atomic-incf. BUT, the MOST important thing
  ;; is that whatever we do use HAS to eventually call something that is
  ;; a memory barrier in SBCL.
  ;; https://www.sbcl.org/manual/sbcl.pdf
  (declare global-lock a:AtomicInteger)
  (define global-lock (a:new 0))

  (inline)
  (declare get-global-time (Unit -> a::Word))
  (define (get-global-time)
    "Read the global lock time and establish a memory barrier."
    (mem-barrier)
    (a:read global-lock))

  (repr :native cl:cons)
  (define-type ReadEntry%)

  (inline)
  (declare read-entry-addr% (ReadEntry% -> Anything))
  (define (read-entry-addr% entr)
    (lisp Anything (entr)
      (cl:car entr)))

  (inline)
  (declare read-entry-current-val% (ReadEntry% -> Anything))
  (define (read-entry-current-val% entr)
    (tvar-value%
     (lisp (TVar Anything) (entr)
       (cl:car entr))))

  (inline)
  (declare read-entry-cached-val% (ReadEntry% -> Anything))
  (define (read-entry-cached-val% entr)
    (lisp Anything (entr)
      (cl:cdr entr)))

  (repr :native cl:hash-table)
  (define-type WriteHashTable%)

  (inline)
  (declare new-write-hash-table% (Unit -> WriteHashTable%))
  (define (new-write-hash-table%)
    (lisp WriteHashTable% ()
      (cl:make-hash-table :test 'cl:eq)))

  (inline)
  (declare logged-write-value% (WriteHashTable% -> :a -> Optional Anything))
  (define (logged-write-value% write-log key)
     (lisp (Optional Anything) (write-log key)
       (cl:multiple-value-bind (val found?) (cl:gethash key write-log)
         (cl:if found?
                (Some val)
                None))))

  (inline)
  (declare log-write-value% (WriteHashTable% -> TVar :a -> :a -> Unit))
  (define (log-write-value% write-log addr val)
    (lisp :a (write-log addr val)
      (cl:setf (cl:gethash write-log addr) val))
    Unit)

  (inline)
  (declare commit-logged-writes (WriteHashTable% -> Unit))
  (define (commit-logged-writes write-log)
    "Actually set the TVar's value to their corresponding logged write value."
    (lisp :a (write-log)
      (cl:loop :for addr :being :the :hash-keys :of write-log
         :using (hash-value value)
         :do (call-coalton-function set-tvar% addr value))))

  (define-struct TxData%
    (lock-snapshot (c:cell a::Word))
    (read-log (c:cell (List ReadEntry%)))
    (write-log WriteHashTable%))

  (inline)
  (declare new-tx-data% (a::Word -> TxData%))
  (define (new-tx-data% initial-snapshot)
    (TxData% (c:new initial-snapshot)
             (c:new Nil)
             (new-write-hash-table%)))

  (inline)
  (declare cached-snapshot (TxData% -> a::Word))
  (define (cached-snapshot tx-data)
    (c:read (.lock-snapshot tx-data)))

  (inline)
  (declare log-read-value (TVar :a -> :a -> TxData% -> Unit))
  (define (log-read-value addr val tx-data)
    (c:push! (.read-log tx-data) (lisp ReadEntry% (addr val)
                                   (cl:cons addr val)))
    Unit)

  (inline)
  (declare read-only? (TxData% -> Boolean))
  (define (read-only? tx-data)
    (let write-log = (.write-log tx-data))
    (lisp Boolean (write-log)
      (cl:zerop (cl:hash-table-count write-log))))

  (define-type (TxResult% :a)
    (TxSuccess :a)
    TxFailed)

  (repr :transparent)
  (define-type (STM :io :a)
    (STM% (TxData% -> :io (TxResult% :a))))

  (inline)
  (declare tx-begin-io% (MonadIo :m => Unit -> :m TxData%))
  (define (tx-begin-io%)
    (wrap-io
      (rec % ()
        (let snapshot = (a:read global-lock))
        (if (bit-odd? snapshot)
          (%)
          (new-tx-data% snapshot)))))

  (derive Eq)
  (define-type ValidateRes%
    TxAbort%
    (TxContinue% a::Word))

  (inline)
  (declare validate% (TxData% -> ValidateRes%))
  (define (validate% tx-data)
    (rec % ()
      (let start-time = (get-global-time))
      (if (bit-odd? start-time)
            (%)
        (progn
          (let check =
            (rec %% ((rem-reads (c:read (.read-log tx-data))))
              (match rem-reads
                ((Nil)
                 (if (== start-time (get-global-time))
                     (Some (TxContinue% start-time))
                     None))
                ((Cons read-entry next-reads)
                 (if (not (unsafe-pointer-eq?
                           (read-entry-current-val% read-entry)
                           (read-entry-cached-val% read-entry)))
                     (Some TxAbort%)
                     (%% next-reads))))))
          (match check
            ((None) (%))
            ((Some x) x))))))

  (inline)
  (declare tx-read% (MonadIo :m => TVar :a -> STM :m :a))
  (define (tx-read% tvar)
    (STM%
     (fn (tx-data)
       (wrap-io
         (match (logged-write-value% (.write-log tx-data) tvar)
           ((Some val)
            (TxSuccess (from-anything val)))
           ((None)
            (let validated-val =
              (rec % ((val (tvar-value% tvar)))
                ;; NOTE: We (probably?) don't need a memory barrier here
                ;; because Validate() uses one anyway.
                (if (== (cached-snapshot tx-data)
                        (a:read global-lock))
                    val
                    (% (tvar-value% tvar)))))
            (log-read-value tvar validated-val tx-data)
            (TxSuccess validated-val)))))))

  (inline)
  (declare tx-write% (MonadIo :m => TVar :a -> :a -> STM :m Unit))
  (define (tx-write% tvar val)
    (STM%
     (fn (tx-data)
       (wrap-io
         (TxSuccess
          (log-write-value% (.write-log tx-data) tvar val))))))

  (inline)
  (declare tx-commit% (MonadIo :m => STM :m Unit))
  (define tx-commit%
    (STM%
     (fn (tx-data)
       (wrap-io
         (if (read-only? tx-data)
             (TxSuccess Unit)
             (progn
               (while (not (a:cas! global-lock
                                   (c:read (.lock-snapshot tx-data))
                                   (1+ (c:read (.lock-snapshot tx-data)))))
                 (let validate-res = (validate% tx-data))
                 (match validate-res
                   ((TxContinue% time)
                    (c:write! (.lock-snapshot tx-data) time))
                   ((TxAbort%)
                    (error "Implement retries!!"))))
               (commit-logged-writes (.write-log tx-data))
               (a:incf! global-lock 1)
               (TxSuccess Unit)))))))

  )

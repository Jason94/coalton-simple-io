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
  (declare tvar-value% (TVar :a -> :a))
  (define (tvar-value% tvar)
    (c:read (unwrap-tvar% tvar)))

  ;; (inline)
  ;; (declare unwrap-stm% (STM :io :a -> :io :a))
  ;; (define (unwrap-stm% (STM% io-a))
  ;;   io-a)

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
  (declare )

  (define-struct TxData%
    (lock-snapshot (c:cell a::Word))
    (reads (List ReadEntry%))
    (writes WriteHashTable%))

  (inline)
  (declare new-tx-data% (a::Word -> TxData%))
  (define (new-tx-data% initial-snapshot)
    (TxData% (c:new initial-snapshot)
             Nil
             (new-write-hash-table%)))

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
            (rec %% ((rem-reads (.reads tx-data)))
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
  (declare tx-read% (MonadIo :m => TVar :a -> STM :a))
  (define (tx-read% ))

  )

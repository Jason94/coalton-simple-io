(defpackage :coalton-io/tests/stm
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/experimental/do-control-core
        #:io/utils
        #:io/simple-io
        #:io/exception
        #:io/mut
        #:io/mvar
        #:io/thread
        #:io/future
        #:io/stm)
  (:import-from #:io/stm/stm-impl
   #:tx-io!%)
  (:local-nicknames
   (:l #:coalton-library/list))
  )
(in-package :coalton-io/tests/stm)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/stm-fiasco)
(coalton-fiasco-init #:coalton-io/tests/stm-fiasco)

;;;
;;; Single threaded tests
;;;

(define-test test-read-tvar ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (run-tx (read-tvar a)))))
  (is (== 0 result)))

(define-test test-read-multiple-tvars ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar "b"))
      (do-run-tx
        (a-val <- (read-tvar a))
        (b-val <- (read-tvar b))
        (pure (Tuple a-val b-val))))))
  (is (== (Tuple 0 "b") result)))

(define-test test-write-read-tx ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (do-run-tx
        (write-tvar a 10)
        (read-tvar a)))))
  (is (== 10 result)))

(define-test test-write-read-separate-txs ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (run-tx (write-tvar a 10))
      (run-tx (read-tvar a)))))
  (is (== 10 result)))

(define-test test-retry ()
  (let result =
    (run-io!
     (do
      (retry-count <- (new-var 0))
      (x <- (new-tvar 0))
      (do-run-tx
        ;; Don't do this at home!
        (n-retries <- (tx-io!% (read retry-count)))
        (do-if (> n-retries 0)
            (write-tvar x n-retries)
          (tx-io!% (write retry-count (1+ n-retries)))
          retry))
      (run-tx (read-tvar x)))))
  (is (== 1 result)))

(define-test test-exception-aborts-transaction ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (result1 <-
       (try-all
        (do-run-tx
          (write-tvar a 100)
          (raise "Raising exception after write")
          (read-tvar a))))
      (result2 <- (run-tx (read-tvar a)))
      (pure (Tuple result1 result2)))))
  (is (== (Tuple None 0)
          result)))

(define-test test-tx-wrap-error ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (result1 <-
       (try-all
        (do-run-tx
          (write-tvar a 10)
          (next-val <- (wrap-error
                         (error "Coalton error!")
                         100))
          (write-tvar a next-val)
          (read-tvar a))))
      (result2 <- (run-tx (read-tvar a)))
      (pure (Tuple result1 result2)))))
  (is (== (Tuple None 0)
          result)))

;;;
;;; Multi-threaded tests
;;;

(define-test test-write-interrupts-read ()
  (let (Tuple3 observed-as observed-bs result) =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar 100))
      (observed-as <- (new-var Nil))
      (observed-bs <- (new-var Nil))
      (read-gate <- new-empty-mvar)
      (write-gate <- new-empty-mvar)
      (result-fut <-
        (fork-future
         (do-run-tx
           (a-val <- (read-tvar a))
           (tx-io!% (modify observed-as (Cons a-val)))
           ;; Let the write-tx know that we've read the first tvar
           (tx-io!% (put-mvar read-gate Unit))
           ;; Wait for the write-tx to write to both tvars
           (tx-io!% (take-mvar write-gate))
           (b-val <- (read-tvar b))
           (tx-io!% (modify observed-bs (Cons b-val)))
           (pure (Tuple a-val b-val)))))
      (do-fork
        (take-mvar read-gate)
        (do-run-tx
          (write-tvar a 1)
          (write-tvar b 101))
        (put-mvar write-gate Unit)
        ;; Let the read-tx get through its second try
        (take-mvar read-gate)
        (put-mvar write-gate Unit))
      (result <- (await result-fut))
      (observed-as <- (read observed-as))
      (observed-bs <- (read observed-bs))
      (pure (Tuple3 observed-as observed-bs result)))))
  (is (== (make-list 1 0) observed-as))
  (is (== (make-list 101) observed-bs))
  (is (== (Tuple 1 101) result)))

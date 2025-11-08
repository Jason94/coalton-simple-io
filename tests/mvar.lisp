(defpackage :coalton-simple-io/tests/mvar
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:simple-io/io
        #:simple-io/mvar
        #:simple-io/thread)
  (:local-nicknames
   (:l #:coalton-library/list))
  )
(in-package :coalton-simple-io/tests/mvar)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-simple-io/tests/mvar-fiasco)
(coalton-fiasco-init #:coalton-simple-io/tests/mvar-fiasco)

;;;
;;; Single threaded tests
;;;

(define-test test-mvar-read-initial-value ()
  (let result =
    (run!
      (do
        (mv <- (new-mvar 10))
        (read-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-subsequent-read ()
  (let result =
    (run!
      (do
        (mv <- (new-mvar 10))
        (a <- (read-mvar mv))
        (b <- (read-mvar mv))
        (pure (Tuple a b)))))
  (is (== (Tuple 10 10) result)))

(define-test test-mvar-take-initial-value ()
  (let result =
    (run!
      (do
        (mv <- (new-mvar 10))
        (take-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-try-take-initial-value ()
  (let result =
    (run!
     (do
      (mv <- (new-mvar 10))
      (try-take-mvar mv))))
  (is (== (Some 10) result)))

(define-test test-mvar-try-take-empty ()
  (let result =
    (the (Optional Integer)
         (run!
          (do
           (mv <- new-empty-mvar)
           (try-take-mvar mv)))))
  (is (== None result)))

(define-test test-mvar-try-put-empty ()
  (let result =
    (run!
     (do
      (mv <- new-empty-mvar)
      (put-result <- (try-put-mvar mv 10))
      (val <- (read-mvar mv))
      (pure (Tuple put-result val)))))
  (is (== (Tuple True 10)
          result)))

(define-test test-mvar-try-put-full ()
  (let result =
    (run!
     (do
      (mv <- (new-mvar 0))
      (put-result <- (try-put-mvar mv 10))
      (val <- (read-mvar mv))
      (pure (Tuple put-result val)))))
  (is (== (Tuple False 0)
          result)))

(define-test test-mvar-is-empty ()
  (let result =
    (run!
     (do
      (mv <- (the (:m (MVar Integer))
                  new-empty-mvar) )
      (is-empty-mvar mv))))
  (is (== True result)))

(define-test test-mvar-is-not-empty ()
  (let result =
    (run!
     (do
      (mv <- (new-mvar 10))
      (is-empty-mvar mv))))
  (is (== False result)))

(define-test test-mvar-put-empty ()
  (let result =
    (run!
     (do
      (mv <- new-empty-mvar)
      (put-mvar mv 10)
      (read-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-swap ()
  (let result =
    (run!
     (do
      (mv <- (new-mvar 10))
      (old <- (swap-mvar mv -10))
      (new <- (read-mvar mv))
      (pure (Tuple old new)))))
  (is (== (Tuple 10 -10) result)))

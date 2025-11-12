(defpackage :coalton-io/tests/random
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/random)
  )
(in-package :coalton-io/tests/random)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/random-fiasco)
(coalton-fiasco-init #:coalton-io/tests/random-fiasco)

(define-test test-random-integer-range ()
  (let result =
    (run-io!
     (do
      (let limit = (the UFix 10))
      (rs <- make-random-state)
      ;; check 200 draws are in [0, limit)
      (rec % ((n-to-check 200))
        (if (== n-to-check 0)
            (pure True)
            (do
             (x <- (random rs limit))
             (if (and (>= x 0) (< x limit))
                 (% (- n-to-check 1))
                 (pure False))))))))
  (is (== True result)))

(define-test test-random_-integer-range ()
  (let result =
    (run-io!
     (do
      (let limit = (the UFix 10))
      ;; check 200 draws are in [0, limit)
      (rec % ((n-to-check 200))
        (if (== n-to-check 0)
            (pure True)
            (do
             (x <- (random_ limit))
             (if (and (>= x 0) (< x limit))
                 (% (- n-to-check 1))
                 (pure False))))))))
  (is (== True result)))

(define-test test-random-float-range ()
  (let result =
    (run-io!
     (do
      (let limit = 1.5)
      (rs <- make-random-state)
      ;; check 200 draws are in [0, limit)
      (rec % ((n-to-check 200))
        (if (== n-to-check 0)
            (pure True)
            (do
             (x <- (random rs limit))
             (if (and (>= x 0) (< x limit))
                 (% (- n-to-check 1))
                 (pure False))))))))
  (is (== True result)))

(define-test test-copy-state-produces-identical-sequence ()
  (let limit = (the UFix 100))
  (let result =
    (run-io!
      (do
        (rs1 <- make-random-state)
        (rs2 <- (copy-random-state rs1))
        ;; Compare 100 draws pairwise without building lists
        (rec % ((n-to-check 200))
          (if (== n-to-check 0)
              (pure True)
              (do
               (x1 <- (random rs1 limit))
               (x2 <- (random rs2 limit))
               (if (/= x1 x2)
                   (pure False)
                   (% (- n-to-check 1)))))))))
  (is (== True result)))

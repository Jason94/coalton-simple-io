(defpackage coalton-simple-io/tests/main
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:simple-io/io))
(in-package :coalton-simple-io/tests/main)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: To run this test file, execute `(asdf:test-system :coalton-simple-io)' in your Lisp.

(fiasco:define-test-package #:coalton-simple-io/tests/main-fiasco)

(coalton-fiasco-init #:coalton-simple-io/tests/main-fiasco)

(coalton-toplevel
  (declare io-const (:a -> IO :a))
  (define (io-const a)
    (wrap-io a)))

(define-test test-run-wrapped-io ()
  (is (== 5 (run! (io-const 5)))))

(define-test test-do-notation ()
  (let result =
    (run!
     (do
      (x <- (io-const 5))
      (y <- (io-const 10))
      (pure (+ x y)))))
  (is (== 15 result)))

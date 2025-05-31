(defpackage coalton-simple-io/tests/main
  (:use :cl
        :coalton-simple-io
        :rove))
(in-package :coalton-simple-io/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :coalton-simple-io)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

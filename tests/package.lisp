(defpackage #:coalton-simple-io/tests
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-simple-io/tests/io)
  (:export #:run-tests))
(in-package #:coalton-simple-io/tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-simple-io/fiasco-test-package)

(coalton-fiasco-init #:coalton-simple-io/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:coalton-simple-io/tests/io-fiasco)
   :interactive cl:t))


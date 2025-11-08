
(defpackage #:coalton-simple-io/tests
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-simple-io/tests/io
        #:coalton-simple-io/tests/mut
        #:coalton-simple-io/tests/random
        #:coalton-simple-io/tests/thread
        #:coalton-simple-io/tests/mvar
        )
  (:export #:run-tests))
(in-package #:coalton-simple-io/tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-simple-io/fiasco-test-package)

(coalton-fiasco-init #:coalton-simple-io/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:coalton-simple-io/tests/io-fiasco
               #:coalton-simple-io/tests/mut-fiasco
               #:coalton-simple-io/tests/random-fiasco
               #:coalton-simple-io/tests/thread-fiasco
               #:coalton-simple-io/tests/mvar-fiasco
               )
   :interactive cl:t))

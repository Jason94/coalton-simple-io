(defsystem "coalton-simple-io"
  :long-name "coalton-simple-io"
  :version "0.1"
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license "MIT"
  :depends-on ("coalton" "named-readtables")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "io")
                 (:file "io-mut")
                 (:file "io-term")
                 (:file "io-random")
                 (:file "io-unique")
                 (:file "io-file")
                 (:file "stubs/term")
                 )))
  :description "Simple IO monad for Coalton."
  :long-description "Implements a *very* basic implementation of an IO monad for coalton and provides a few simple IO operations, such as console I/O."
  :in-order-to ((test-op (test-op "coalton-simple-io/tests"))))

(defsystem "coalton-simple-io/tests"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-simple-io"
               "coalton/testing"
               "fiasco")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "package"))))
  :description "Test system for coalton-simple-io"
  :perform (test-op (op c) (symbol-call '#:coalton-simple-io/tests '#:run-tests)))

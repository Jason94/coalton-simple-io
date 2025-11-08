(defsystem "coalton-simple-io"
  :long-name "coalton-simple-io"
  :version "0.1"
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license "MIT"
  :depends-on ("coalton" "named-readtables" "atomics" "coalton-threads")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "atomics")
                 (:file "io")
                 (:file "io-mut")
                 (:file "io-term")
                 (:file "io-random")
                 (:file "io-file")
                 (:file "io-thread")
                 (:file "io-atomic")
                 (:file "io-mvar")
                 (:file "io-unique")
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
                :serial t
                :components
                ((:file "io")
                 (:file "mut")
                 (:file "random")
                 (:file "thread")
                 (:file "mvar")
                 (:file "package"))))
  :description "Test system for coalton-simple-io"
  :perform (test-op (op c) (symbol-call '#:coalton-simple-io/tests '#:run-tests)))

(defsystem "coalton-simple-io/examples"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-simple-io")
  :components ((:module "examples"
                :components
                ((:file "channels-threading")
                 (:file "hangman"))))
  :description "Example programs for coalton-simple-io.")

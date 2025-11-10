(cl:in-package :cl-user)
(defpackage :io/io-all
  (:use #:coalton #:coalton-prelude)
  (:import-from #:io/thread
   #:implement-monad-io-thread)
  (:import-from #:io/atomic
   #:implement-monad-io-atomic)
  (:import-from #:io/mut
   #:implement-monad-io-var)
  (:import-from #:io/mvar
   #:implement-monad-io-mvar)
  (:import-from #:io/file
   #:implement-monad-io-file)
  (:import-from #:io/random
   #:implement-monad-io-random)
  (:import-from #:io/term
   #:implement-monad-io-term)
  (:import-from #:io/unique
   #:implement-monad-io-unique)
  (:export
   #:implement-monad-io-all))
(in-package :io/io-all)

(cl:defmacro implement-monad-io-all (monad)
  `(progn
     (implement-monad-io-thread ,monad)
     (implement-monad-io-atomic ,monad)
     (implement-monad-io-var ,monad)
     (implement-monad-io-mvar ,monad)
     (implement-monad-io-file ,monad)
     (implement-monad-io-random ,monad)
     (implement-monad-io-term ,monad)
     (implement-monad-io-unique ,monad)))

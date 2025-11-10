(cl:in-package :cl-user)
(defpackage :simple-io/io-all
  (:use
   #:cl)
  (:import-from #:simple-io/thread
   #:implement-monad-io-thread)
  (:import-from #:simple-io/atomic
   #:implement-monad-io-atomic)
  (:import-from #:simple-io/mut
   #:implement-monad-io-var)
  (:import-from #:simple-io/mvar
   #:implement-monad-io-mvar)
  (:import-from #:simple-io/file
   #:implement-monad-io-file)
  (:import-from #:simple-io/random
   #:implement-monad-io-random)
  (:import-from #:simple-io/term
   #:implement-monad-io-term)
  (:import-from #:simple-io/unique
   #:implement-monad-io-unique)
  (:export
   #:implement-monad-io-all))
(in-package :simple-io/io-all)

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

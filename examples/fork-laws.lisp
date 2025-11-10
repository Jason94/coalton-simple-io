(cl:in-package :cl-user)
(defpackage :io/examples/fork-laws
  (:use
   #:coalton
   #:coalton-prelude
   #:io/io
   #:io/thread
   #:io/term
   #:io/random
   #:io/mvar
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops)
  (:local-nicknames
   (:r #:coalton-library/result)
   ))
(in-package :io/examples/fork-laws)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; See:
;;; https://www.reddit.com/r/scala/comments/3zofjl/why_is_future_totally_unusable/
;;;

(coalton-toplevel

 (declare test-a (IO Unit))
 (define test-a
   (do
    (r <- make-random-state)
    (chan <- new-empty-chan)
     (let fork-op =
       (do-fork
         (x <- (random r (the UFix 1000)))
         (push-chan chan x)))
     fork-op
     fork-op
     (a <- (pop-chan chan))
     (b <- (pop-chan chan))
     (write-line a)
     (write-line b)))

 (declare test-b (IO Unit))
 (define test-b
   (do
    (r <- make-random-state)
    (chan <- new-empty-chan)
     (do-fork
       (x <- (random r (the UFix 1000)))
       (push-chan chan x))
     (do-fork
       (x <- (random r (the UFix 1000)))
       (push-chan chan x))
     (a <- (pop-chan chan))
     (b <- (pop-chan chan))
     (write-line a)
     (write-line b)))

  )

(cl:in-package :cl-user)
(defpackage :simple-io/examples/channels-threading
  (:use
   #:coalton
   #:coalton-prelude
   #:simple-io/io
   #:simple-io/thread
   #:simple-io/term
   #:simple-io/random
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops)
  (:local-nicknames
   (:s #:coalton-library/string)
   (:f_ #:coalton-library/file)
   (:f #:simple-io/file)
   (:r #:coalton-library/result)
   (:mv #:simple-io/mvar)
   (:m #:simple-io/mut)))
(in-package :simple-io/examples/channels-threading)

(named-readtables:in-readtable coalton:coalton)

;;; This example generates a file of random integers, then reads it
;;; back and sums them all.
;;;
;;; It creates one thread to read lines from the file, N-WORKERS
;;; number of threads to parse the lines into integers, and one
;;; thread to sum all of the integers togother. Two MChannels pass
;;; the data through the worker threads, and an MVar passes the total
;;; back to the main thread when the summing is finished.

(coalton-toplevel
  (define data-filename "rands.txt")
  (declare data-rows UFix)
  (define data-rows 10000000)
  (declare data-max UFix)
  (define data-max 500000000)
  (declare n-workers UFix)
  (define n-workers 4)

  (declare write-data-file (IO Unit))
  (define write-data-file
    (do
     (exists? <- (map (r:ok-or-def False)
                      (f:exists? data-filename)))
     (do-when (not exists?)
       (f:do-with-open-file (f_:Output (into data-filename) f_:Overwrite) (fs)
         (do-loop-times (_ data-rows)
           (x <- (random_ data-max))
           (f:write-line fs (into x)))
         (pure (Ok Unit))))))

  (declare reader-thread (mv:MChan (Optional String) -> IO Unit))
  (define (reader-thread mchan-input)
    (do
     (f:do-with-open-file (f_:Input (into data-filename)) (fs)
       (do-loop-while-val (line (f:read-line fs))
         (mv:push-chan mchan-input (Some line)))
       (pure (Ok Unit)))
     (do-loop-times (_ n-workers)
       (mv:push-chan mchan-input None))))

  (declare parser-thread (mv:MChan (Optional String) -> mv:MChan (Optional Integer) -> IO Unit))
  (define (parser-thread mchan-input mchan-int)
    (do-if-not-valM (str (mv:pop-chan mchan-input))
          (mv:push-chan mchan-int None)
      (do-when-val (x (s:parse-int str))
        (mv:push-chan mchan-int (Some x)))
      (parser-thread mchan-input mchan-int)))

  (declare summer-thread (mv:MChan (Optional Integer) -> mv:MVar Integer -> IO Unit))
  (define (summer-thread mchan-int mvar-sum)
    (do
     (sum <- (m:new-var 0))
     (closed-parsers <- (m:new-var 0))
     (do-loop-while
       (do-if-valM (x (mv:pop-chan mchan-int))
             (do
              (m:modify sum (+ x))
              (pure True))
         (m:modify closed-parsers (+ 1))
         (map (> (into n-workers)) (m:read closed-parsers))))
     (the-sum <- (m:read sum))
     (mv:put-mvar mvar-sum the-sum)))

  (declare sum-file (IO Integer))
  (define sum-file
    (do
     (write-line "Writing data file...")
     write-data-file
     (write-line "Done writing file...")
     (input-chan <- mv:new-empty-chan)
     (ints-chan <- mv:new-empty-chan)
     (sum-mvar <- mv:new-empty-mvar)
     (write-line "Forking threads...")
     (fork (reader-thread input-chan))
     (do-loop-times (_ n-workers)
       (fork (parser-thread input-chan ints-chan)))
     (fork (summer-thread ints-chan sum-mvar))
     (write-line "Waiting for sum...")
     (sum <- (mv:take-mvar sum-mvar))
     (write-line (<> "Calculated sum: " (into sum)))
     (pure sum)))
  )

(cl:defun run-example ()
  (coalton (run-io! sum-file)))

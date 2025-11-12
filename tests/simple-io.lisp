(defpackage coalton-io/tests/io
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/monad-io)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:c #:coalton-library/cell))
  )
(in-package :coalton-io/tests/io)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-fiasco)

(coalton-fiasco-init #:coalton-io/tests/io-fiasco)

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

(define-test test-map-into-io-empty ()
  (let result =
    (run-io!
     (do-map-into-io (x (make-list))
       (pure (+ x 10)))))
  (is (== Nil result)))

(define-test test-map-into-io ()
  (let result =
    (run-io!
     (do-map-into-io (x (make-list 0 10 20 30))
       (pure (+ x 10)))))
  (is (== (make-list 10 20 30 40) result)))

(define-test test-foreach-io-empty ()
  (let run-ints = (c:new Nil))
  (run-io!
   (do-foreach-io (x (the (List Integer) (make-list)))
     (wrap-io
       (c:push! run-ints x))))
  (is (== Nil (c:read run-ints))))

(define-test test-foreach-io ()
  (let run-ints = (c:new Nil))
  (run-io!
   (do-foreach-io (x (make-list 0 10 20 30))
     (wrap-io
       (c:push! run-ints x))))
  (is (== (make-list 0 10 20 30)
          (l:reverse (c:read run-ints)))))

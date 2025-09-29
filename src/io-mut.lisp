(cl:in-package :cl-user)
(defpackage :simple-io/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:local-nicknames
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:IORef
   #:new-io-ref
   #:read
   #:write
   #:modify
   ))
(in-package :simple-io/io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (IORef :a)
    (IORef% (Cell :a)))

  (declare new-io-ref (:a -> IO (IORef :a)))
  (define (new-io-ref val)
    (wrap-io (IORef% (c:new val))))

  (declare read (IORef :a -> IO :a))
  (define (read (IORef% cel))
    (wrap-io (c:read cel)))

  (declare write (IORef :a -> :a -> IO :a))
  (define (write (IORef% cel) val)
    "Set the value in an IORef and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (declare modify (IORef :a -> (:a -> :a) -> IO :a))
  (define (modify (IORef% cel) f)
    "Modify the value in an IORef and return the old value."
    (wrap-io (c:update-swap! f cel))))

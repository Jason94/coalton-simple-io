(cl:in-package :cl-user)
(defpackage :simple-io/term
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/io)
  (:export
   #:write-line
   #:read-line))
(in-package :simple-io/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write-line ((Into :a String) => :a -> IO Unit))
  (define (write-line obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp :a (str)
        (cl:format cl:t "~a~%" str))
      Unit))

  (declare read-line (IO String))
  (define read-line
    (wrap-io (lisp :a ()
               (cl:read-line))))

  ;; (declare sleep (Integer -> IO Unit))
  ;; (define (sleep s)
  ;;   (wrap-io
  ;;         (lisp :a (s)
  ;;           (cl:sleep s))
  ;;         Unit))
  )

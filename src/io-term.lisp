(cl:in-package :cl-user)
(defpackage :simple-io/term
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/io)
  (:import-from #:coalton-library/monad/statet
   #:StateT)
  (:import-from #:coalton-library/monad/environment
   #:EnvT)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:export
   #:MonadIoTerm
   #:derive-monad-io-term

   #:write
   #:write-line
   #:read-line))
(in-package :simple-io/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Monad :m => MonadIoTerm :m)
    (write
     "Write a string to standard output."
     (Into :a String => :a -> :m Unit))
    (write-line
     "Write a string to standard output followed by a newline."
     (Into :a String => :a -> :m Unit))
    (read-line
     "Read a line from standard input."
     (:m String)))

  (declare write% (Into :a String => :a -> IO Unit))
  (define (write% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp :a (str)
        (cl:format cl:t "~a" str))
      Unit))

  (declare write-line% ((Into :a String) => :a -> IO Unit))
  (define (write-line% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp :a (str)
        (cl:format cl:t "~a~%" str))
      Unit))

  (declare read-line% (IO String))
  (define read-line%
    (wrap-io (lisp :a ()
               (cl:read-line))))

  (define-instance (MonadIoTerm IO)
    (define write write%)
    (define write-line write-line%)
    (define read-line read-line%)))

(cl:defmacro derive-monad-io-term (monadT-form)
  "Automatically derive an instance of MonadIoTerm for a monad transformer.

Example:
  (derive-monad-io-term (st:StateT :s :m))"
  `(define-instance (MonadIoTerm :m => MonadIoTerm ,monadT-form)
     (define write (compose lift write))
     (define write-line (compose lift write-line))
     (define read-line (lift read-line))))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel
  (derive-monad-io-term (StateT :s :m))
  (derive-monad-io-term (EnvT :e :m))
  (derive-monad-io-term (LoopT :m)))

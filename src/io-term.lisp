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
  (:export
   #:MonadIoTerm
   #:write-line
   #:read-line))
(in-package :simple-io/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Monad :m => MonadIoTerm :m)
    (write-line (Into :a String => :a -> :m Unit))
    (read-line (:m String)))

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
    (define write-line write-line%)
    (define read-line read-line%))

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (define-instance ((MonadIoTerm :m) => MonadIoTerm (StateT :s :m))
    (define write-line (compose lift write-line))
    (define read-line (lift read-line)))

  (define-instance ((MonadIoTerm :m) => MonadIoTerm (EnvT :e :m))
    (define write-line (compose lift write-line))
    (define read-line (lift read-line))))

(cl:in-package :cl-user)
(defpackage :simple-io/file
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:simple-io/utils
   #:simple-io/io)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:file #:coalton-library/file)
   (:st   #:coalton-library/monad/statet)
   (:env  #:coalton-library/monad/environment))
  (:export
   #:MonadIoFile
   #:derive-monad-io-file

   #:exists?
   #:file-exists?
   #:directory-exists?

   #:open
   #:close
   #:abort
   #:with-open-file
   #:with-temp-file
   #:with-temp-directory

   #:copy
   #:create-directory
   #:delete-file
   #:remove-directory
   #:remove-directory-recursive
   #:system-relative-pathname

   #:read-file-to-string
   #:read-file-lines
   #:write-string
   #:write-line
   #:read-char
   #:read-line
   #:write-char

   #:read-file-to-vector
   #:read-vector
   #:write-vector
   #:write-to-file
   #:append-to-file
   #:set-file-position

   #:do-with-open-file
   #:do-with-temp-file
   #:do-with-temp-directory))
(in-package :simple-io/file)

(named-readtables:in-readtable coalton:coalton)

;;; ------------------------------------------------------------
;;; CL Macros
;;; ------------------------------------------------------------

(cl:defmacro derive-monad-io-file (monad-param monadT-form)
  "Derive a `MonadIoFile` instance for MONADT-FORM by lifting into the base instance.

Example:
  (derive-monad-io-file :m (st:StateT :s :m))"
  `(define-instance (MonadIoFile ,monad-param => MonadIoFile ,monadT-form)
     (define exists? (compose lift exists?))
     (define file-exists? (compose lift file-exists?))
     (define directory-exists? (compose lift directory-exists?))

     (define open (compose lift open))
     (define close (compose lift close))
     (define abort (compose lift abort))

     (define with-open-file (compose2 lift with-open-file))
     (define with-temp-file (compose2 lift with-temp-file))
     (define with-temp-directory (compose lift with-temp-directory))

     (define copy (compose2 lift copy))
     (define create-directory (compose lift create-directory))
     (define delete-file (compose lift delete-file))
     (define remove-directory (compose lift remove-directory))
     (define remove-directory-recursive (compose lift remove-directory-recursive))
     (define system-relative-pathname (compose2 lift system-relative-pathname))

     (define read-file-to-string (compose lift read-file-to-string))
     (define read-file-lines (compose lift read-file-lines))

     (define read-char (compose lift read-char))
     (define read-line (compose lift read-line))
     (define write-char (compose2 lift write-char))
     (define write-line (compose2 lift write-line))
     (define write-string (compose2 lift write-string))

     (define read-file-to-vector (compose lift read-file-to-vector))
     (define read-vector (compose2 lift read-vector))
     (define write-vector (compose2 lift write-vector))
     (define write-to-file (compose2 lift write-to-file))
     (define append-to-file (compose2 lift append-to-file))
     (define set-file-position (compose2 lift set-file-position))))

(cl:defmacro do-with-open-file (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file opts (fs)
    (line <- (read-char fs))
    ...)
"
  `(with-open-file ,opts (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-file (type (fs) cl:&body body)
  "`do` sugar for `with-temp-file` (TYPE is a string like \"txt\")."
  `(with-temp-file ,type (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-directory ((dir) cl:&body body)
  "`do` sugar for `with-temp-directory`."
  `(with-temp-directory (fn (,dir) (do ,@body))))

;;; ------------------------------------------------------------
;;; Coalton definitions
;;; ------------------------------------------------------------

(coalton-toplevel
  ;;
  ;; IO-backed primitives (internal, %-suffixed)
  ;;

  (declare exists?% (Into :a file:Pathname => :a -> IO (Result file:FileError Boolean)))
  (define (exists?% pth)
    (wrap-io (file:exists? pth)))

  (declare file-exists?% (Into :a file:Pathname => :a -> IO (Result file:FileError Boolean)))
  (define (file-exists?% pth)
    (wrap-io (file:file-exists? pth)))

  (declare directory-exists?% (Into :a file:Pathname => :a -> IO (Result file:FileError Boolean)))
  (define (directory-exists?% pth)
    (wrap-io (file:directory-exists? pth)))

  (declare open% (file:File :a => file:StreamOptions -> IO (Result file:FileError (file:FileStream :a))))
  (define (open% opts)
    (wrap-io (file:open opts)))

  (declare close% ((file:FileStream :a) -> IO (Result file:FileError :b)))
  (define (close% fs)
    (wrap-io (file:close fs)))

  (declare abort% ((file:FileStream :a) -> IO (Result file:FileError :b)))
  (define (abort% fs)
    (wrap-io (file:abort fs)))

  (declare with-open-file%
           (file:File :a => file:StreamOptions
            -> ((file:FileStream :a) -> IO (Result file:FileError :b))
            -> IO (Result file:FileError :b)))
  (define (with-open-file% opts k)
    "IO version of FILE:WITH-OPEN-FILE where the continuation returns IO."
    (wrap-io (file:with-open-file opts (fn (fs) (run! (k fs))))))

  (declare with-temp-file%
           (file:File :a => String
            -> ((file:FileStream :a) -> IO (Result file:FileError :b))
            -> IO (Result file:FileError :b)))
  (define (with-temp-file% file-type k)
    (wrap-io (file:with-temp-file file-type (fn (fs) (run! (k fs))))))

  (declare with-temp-directory%
           ((file:Pathname -> IO (Result file:FileError :a)) -> IO (Result file:FileError :a)))
  (define (with-temp-directory% k)
    (wrap-io (file:with-temp-directory (fn (dir) (run! (k dir))))))

  (declare copy% ((Into :a file:Pathname) (Into :b file:Pathname) => :a -> :b -> IO (Result file:FileError Unit)))
  (define (copy% a b)
    (wrap-io (file:copy! a b)))

  (declare create-directory% (Into :p file:Pathname => :p -> IO (Result file:FileError file:Pathname)))
  (define (create-directory% p)
    (wrap-io (file:create-directory! p)))

  (declare delete-file% (Into :p file:Pathname => :p -> IO (Result file:FileError Unit)))
  (define (delete-file% p)
    (wrap-io (file:delete-file! p)))

  (declare remove-directory% (Into :p file:Pathname => :p -> IO (Result file:FileError :p)))
  (define (remove-directory% p)
    (wrap-io (file:remove-directory! p)))

  (declare remove-directory-recursive% (Into :p file:Pathname => :p -> IO (Result file:FileError Unit)))
  (define (remove-directory-recursive% p)
    (wrap-io (file:remove-directory-recursive! p)))

  (declare system-relative-pathname% (Into :sys String => :sys -> String -> IO (Result file:FileError file:Pathname)))
  (define (system-relative-pathname% sys name)
    (wrap-io (file:system-relative-pathname sys name)))

  (declare read-file-to-string% (Into :p file:Pathname => :p -> IO (Result file:FileError String)))
  (define (read-file-to-string% p)
    (wrap-io (file:read-file-to-string p)))

  (declare read-file-lines% (Into :p file:Pathname => :p -> IO (Result file:FileError (List String))))
  (define (read-file-lines% p)
    (wrap-io (file:read-file-lines p)))

  (declare write-to-file% ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> IO (Result file:FileError Unit)))
  (define (write-to-file% p v)
    (wrap-io (file:write-to-file! p v)))

  (declare append-to-file% ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> IO (Result file:FileError Unit)))
  (define (append-to-file% p v)
    (wrap-io (file:append-to-file! p v)))

  (declare read-char% ((file:FileStream Char) -> IO (Result file:FileError Char)))
  (define (read-char% fs)
    (wrap-io (file:read-char fs)))

  (declare read-line% ((file:FileStream Char) -> IO (Result file:FileError String)))
  (define (read-line% fs)
    (wrap-io (file:read-line fs)))

  (declare write-char% ((file:FileStream Char) -> Char -> IO (Result file:FileError Unit)))
  (define (write-char% fs c)
    (wrap-io (file:write-char fs c)))

  (declare write-line% ((file:FileStream Char) -> String -> IO (Result file:FileError Unit)))
  (define (write-line% fs s)
    (wrap-io (file:write-line fs s)))

  (declare write-string% ((file:FileStream Char) -> String -> IO (Result file:FileError Unit)))
  (define (write-string% fs s)
    (wrap-io (file:write-string fs s)))

  (declare read-file-to-vector% (file:File :a => (file:FileStream :a) -> IO (Result file:FileError (Vector :a))))
  (define (read-file-to-vector% fs)
    (wrap-io (file:read-file-to-vector fs)))

  (declare read-vector% (file:File :a => (file:FileStream :a) -> UFix -> IO (Result file:FileError (Vector :a))))
  (define (read-vector% fs n)
    (wrap-io (file:read-vector fs n)))

  (declare write-vector% ((file:File :a) (RuntimeRepr :a) => (file:FileStream :a) -> (Vector :a) -> IO (Result file:FileError Unit)))
  (define (write-vector% fs v)
    (wrap-io (file:write-vector fs v)))

  (declare set-file-position% ((file:FileStream :a) -> UFix -> IO (Result file:FileError Unit)))
  (define (set-file-position% fs pos)
    (wrap-io (file:set-file-position fs pos)))

  ;;
  ;; Class + instances (generalized API)
  ;;

  (define-class (Monad :m => MonadIoFile :m)
    (exists? (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))
    (file-exists? (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))
    (directory-exists? (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))

    (open (file:File :a => file:StreamOptions -> :m (Result file:FileError (file:FileStream :a))))
    (close ((file:FileStream :a) -> :m (Result file:FileError :b)))
    (abort ((file:FileStream :a) -> :m (Result file:FileError :b)))

    (with-open-file (file:File :a => file:StreamOptions
                     -> ((file:FileStream :a) -> IO (Result file:FileError :b))
                     -> :m (Result file:FileError :b)))
    (with-temp-file (file:File :a => String
                     -> ((file:FileStream :a) -> IO (Result file:FileError :b))
                     -> :m (Result file:FileError :b)))
    (with-temp-directory ((file:Pathname -> IO (Result file:FileError :a))
                          -> :m (Result file:FileError :a)))

    (copy ((Into :a file:Pathname) (Into :b file:Pathname) => :a -> :b -> :m (Result file:FileError Unit)))
    (create-directory (Into :p file:Pathname => :p -> :m (Result file:FileError file:Pathname)))
    (delete-file (Into :p file:Pathname => :p -> :m (Result file:FileError Unit)))
    (remove-directory (Into :p file:Pathname => :p -> :m (Result file:FileError :p)))
    (remove-directory-recursive (Into :p file:Pathname => :p -> :m (Result file:FileError Unit)))
    (system-relative-pathname (Into :sys String => :sys -> String -> :m (Result file:FileError file:Pathname)))

    (read-file-to-string (Into :p file:Pathname => :p -> :m (Result file:FileError String)))
    (read-file-lines (Into :p file:Pathname => :p -> :m (Result file:FileError (List String))))
    (read-char ((file:FileStream Char) -> :m (Result file:FileError Char)))
    (read-line ((file:FileStream Char) -> :m (Result file:FileError String)))
    (write-char ((file:FileStream Char) -> Char -> :m (Result file:FileError Unit)))
    (write-line ((file:FileStream Char) -> String -> :m (Result file:FileError Unit)))
    (write-string ((file:FileStream Char) -> String -> :m (Result file:FileError Unit)))

    (read-file-to-vector (file:File :a => (file:FileStream :a) -> :m (Result file:FileError (Vector :a))))
    (read-vector (file:File :a => (file:FileStream :a) -> UFix -> :m (Result file:FileError (Vector :a))))
    (write-vector ((file:File :a) (RuntimeRepr :a) => (file:FileStream :a) -> (Vector :a) -> :m (Result file:FileError Unit)))
    (write-to-file ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
    (append-to-file ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
    (set-file-position ((file:FileStream :a) -> UFix -> :m (Result file:FileError Unit))))

  ;; IO instance
  (define-instance (MonadIoFile IO)
    (define exists? exists?%)
    (define file-exists? file-exists?%)
    (define directory-exists? directory-exists?%)

    (define open open%)
    (define close close%)
    (define abort abort%)

    (define with-open-file with-open-file%)
    (define with-temp-file with-temp-file%)
    (define with-temp-directory with-temp-directory%)

    (define copy copy%)
    (define create-directory create-directory%)
    (define delete-file delete-file%)
    (define remove-directory remove-directory%)
    (define remove-directory-recursive remove-directory-recursive%)
    (define system-relative-pathname system-relative-pathname%)

    (define read-file-to-string read-file-to-string%)
    (define read-file-lines read-file-lines%)

    (define read-char read-char%)
    (define read-line read-line%)
    (define write-char write-char%)
    (define write-line write-line%)
    (define write-string write-string%)

    (define read-file-to-vector read-file-to-vector%)
    (define read-vector read-vector%)
    (define write-vector write-vector%)
    (define write-to-file write-to-file%)
    (define append-to-file append-to-file%)
    (define set-file-position set-file-position%))

  (derive-monad-io-file :m (st:StateT :s :m))
  (derive-monad-io-file :m (env:EnvT :e :m))
  (derive-monad-io-file :m (LoopT :m)))

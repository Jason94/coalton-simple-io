(defpackage #:simple-io/atomics_
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:at #:atomics))
  (:export
   #:Atomic
   #:new
   #:read
   #:compare-and-swap
   #:atomic-pop
   #:atomic-push
   #:atomic-update
   #:atomic-write))

(in-package #:simple-io/atomics_)

(named-readtables:in-readtable coalton:coalton)

;;
;; This file wraps the Atomics library to Coalton.
;; https://github.com/Shinmera/atomics
;;

(cl:declaim (cl:inline make-atomic-internal))

(cl:defstruct atomic-internal
  (inner (cl:error "") :type cl:t))

(cl:defun nil-to-opt (x)
  (cl:if x
         (coalton (Some (lisp :a () x)))
         (coalton None)))

(cl:defmethod cl:print-object ((self atomic-internal) stream)
  (cl:format stream "#.(ATOMIC ~A)" (atomic-internal-inner self))
  self)

#+sbcl
(cl:declaim (sb-ext:freeze-type atomic-internal))

(coalton-toplevel

  (repr :native atomic-internal)
  (define-type (Atomic :a)
    "Thread-safe mutable cell")

  (inline)
  (declare new (:a -> Atomic :a))
  (define (new data)
    "Create a new atomic cell containing `data`."
    (lisp (Atomic :a) (data)
      (make-atomic-internal :inner data)))

  (inline)
  (declare read (Atomic :a -> :a))
  (define (read atm)
    "Read the value of an atomic cell `atm`."
    (lisp :a (atm)
      (atomic-internal-inner atm)))

  (declare compare-and-swap (Atomic :a -> :a -> :a -> Boolean))
  (define (compare-and-swap atm old new)
    "Attempt to swap the contents of `atm` from OLD to NEW. Returns
TRUE if the swap succeeded, FALSE otherwise. Does not repeat."
    (lisp Boolean (atm old new)
      (at:cas (atomic-internal-inner atm) old new)))

  (declare atomic-pop (Atomic (List :a) -> Optional :a))
  (define (atomic-pop atm)
    "Atomically pop from the list inside `atm` until it succeedes,
and return the popped value. Returns None if the list was empty."
    (lisp (Optional :a) (atm)
      (nil-to-opt
       (at:atomic-pop (atomic-internal-inner atm)))))

  (declare atomic-push (Atomic (List :a) -> :a -> List :a))
  (define (atomic-push atm elt)
    "Atomically push ELT onto the list inside `atm` until it succeedes.
Returns the new list, with the element included."
    (lisp (List :a) (atm elt)
      (at:atomic-push elt (atomic-internal-inner atm))))

  (declare atomic-update (Atomic :a -> (:a -> :a) -> :a))
  (define (atomic-update atm f)
    "Atomically update the value in `atm` by applying F until it succeedes.
Returns the new value stored in `atm` after applying F."
    (lisp :a (atm f)
      (cl:let ((update-fn (cl:lambda (x)
                            (call-coalton-function f x))))
        (at:atomic-update (atomic-internal-inner atm) update-fn))))

  (declare atomic-write (Atomic :a -> :a -> Unit))
  (define (atomic-write atm val)
    "Atomically set the value in `atm` to `val`."
    (atomic-update atm (const val))
    Unit)
  )

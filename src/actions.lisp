(in-package :cl-binary-store)

;; An action is a sub-dispatch mechanism for codes in the file.
;; It's currently used for magic-numbers (versioning) and for
;; set-reference-count which sets the size of the reference vector
;; during restore.  The hook is via the generic function ACTION
;; which you can specialize to your action-codes.  See for example
;; magic-numbers.lisp and set-references.lisp

;; An ACTION should return two values, the first value may be
;; an object to store into the stream, the second value may be
;; :ignore if the object is to be ignored and not presented to
;; the user or :end if this is the end of data, or nil if the
;; object is to be collected.

(declaim (inline make-action))
(defstruct action
  (code nil :type (unsigned-byte 8) :read-only t))

;; Allocated action numbers
(defconstant +magic-number-action-code+ 0)
(defconstant +set-reference-action-code+ 1)
(defconstant +end-action-code+ 2)

(defgeneric action (command storage references restore-object)
  (:documentation "If we hit an +action-code+ during restore,
 we will call this which should specialize on command (a ub8).
 You can read anything from the file here as arguments to
 the action.  Return two values, an object/nil and a second
 value which is :ignore, :end, or nil if the object is to be
 collected for the user.  The second value only works if the
 object is a top level object (that is one of the objects in
 the call to store (store nil obj-a obj-b (make-instance 'end-action))"))

(defgeneric store-action (action storage store-object)
  (:documentation "Call during the serialization phase.  You can
 write whatever you want to the file from here.  Specialize on
 the structure-object you made that inherits from `action'"))

(defun restore-action& (storage references restore-object)
  (let ((command (restore-ub8 storage)))
    (action command storage references restore-object)))

(defun store-action& (action storage store-object)
  (with-write-storage (storage)
    (store-ub8 +action-code+ storage nil)
    (store-ub8 (action-code action) storage nil))
  (store-action action storage store-object))

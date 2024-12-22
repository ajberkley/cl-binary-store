(in-package :cl-store-faster)

;; An action is a sub-dispatch mechanism for codes in the file.
;; It's currently used for magic-numbers (versioning) and for
;; set-reference-count which sets the size of the reference vector
;; during restore.  The hook is via the generic function ACTION
;; which you can specialize to your action-codes.  See for example
;; magic-numbers.lisp and set-references.lisp

(declaim (inline make-action))
(defstruct action
  (code nil :type (unsigned-byte 8) :read-only t))

;; Allocated action numbers
(defconstant +magic-number-action-code+ 0)
(defconstant +set-reference-action-code+ 1)

(defgeneric action (command storage references)
  (:documentation "If we hit an +action-code+ during restore,
 we will call this which should specialize on command (a ub8).
 You can read anything from the file here to as arguments to
 the action."))

(defgeneric store-action (action storage references)
  (:documentation "Call during the serialization phase.  You can
 write whatever you want to the file from here.  Specialize on
 the structure-object you made that inherits from `action'"))

(defun restore-action& (storage references)
  (let ((command (restore-ub8 storage)))
    (action command storage references)))

(defun store-action& (action storage references)
  (with-write-storage (storage)
    (store-ub8 +action-code+ storage nil)
    (store-ub8 (action-code action) storage nil)
    (store-action action storage references)))

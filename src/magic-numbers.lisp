;; Simple versioning mechanism.  All versioned output should start
;; with +magic-number-code+ and then a number (though it can occur
;; anywhere and even multiple times in the stream / file, not sure why
;; you'd want that).  When restored, the magic number is checked
;; against the numbers in *supported-version* and *version-being-read*
;; (which is locally bound in restore-objects) is bound to it.

(in-package :cl-store-faster)

(defvar *supported-versions* '(2718281828))

(defvar *write-version* 2718281828
  "Set this to the magic number you wish to write into the file.  It may
 be queried by serialization routines if desired.")

(defvar *version-being-read* nil
  "During restore this is bound to any magic number found previous to
 this in the file.")

(declaim (inline make-magic-number))

(defstruct magic-number
  (number 2718281828 :type integer))

(defun restore-magic-number (storage)
  (let ((magic-number (restore-object storage nil)))
    (unless (member magic-number *supported-versions*)
      (error "Unsupported version ~X, we support ~{~X~^ ~}"
	     magic-number *supported-versions*))
    (setf *version-being-read* magic-number)
    (make-magic-number :number magic-number)))

(defun store-magic-number (magic-number storage)
  (with-write-storage (storage)
    (store-ub8 +magic-number-code+ storage nil)
    (store-fixnum (magic-number-number magic-number) storage t)))
  

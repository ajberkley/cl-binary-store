;; This is our versioning mechanism.  All versioned output starts
;; with +magic-number-code+ and then a number.

(in-package :cl-store-faster)

(defvar *supported-versions* '(2718281828))

(defvar *write-version* 2718281828)

(declaim (inline make-magic-number))

(defstruct magic-number
  (number 2718281828 :type integer))

(defun restore-magic-number (storage)
  (let ((magic-number (restore-object storage nil)))
    (unless (member magic-number *supported-versions*)
      (error "Unsupported version ~X, we support ~{~X~^ ~}"
	     magic-number *supported-versions*))
    (make-magic-number :number magic-number)))

(defun store-magic-number (magic-number storage)
  (with-write-storage (storage)
    (store-ub8 +magic-number-code+ storage nil)
    (store-fixnum (magic-number-number magic-number) storage t)))
  

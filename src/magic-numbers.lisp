;; Simple versioning mechanism.  All versioned output should start
;; with +magic-number-code+ and then a number (though it can occur
;; anywhere and even multiple times in the stream / file, not sure why
;; you'd want that).  When restored, the magic number is checked
;; against the numbers in *supported-version* and *version-being-read*
;; (which is locally bound in restore-objects) is bound to it.

(in-package :cl-binary-store)

(defvar *supported-versions* '(#x0001))

(defvar *write-version* #x0001
  "Set this to the magic number you wish to write into the file.  It may
 be queried by serialization routines if desired.")

(defvar *version-being-read* nil
  "During restore this is bound to any magic number found previous to
 this in the file.")

(defstruct (magic-number (:include action (code +magic-number-action-code+)))
  (number #x0001 :type integer :read-only t))

(defmethod action ((code (eql +magic-number-action-code+)) storage references restore-object)
  (let ((magic-number (funcall restore-object)))
    (unless (member magic-number *supported-versions*)
      (error "Unsupported version #x~X, we support ~{#x~X~^ ~}"
	     magic-number *supported-versions*))
    (setf *version-being-read* magic-number)
    (let ((codespace (gethash magic-number *codespaces*)))
      (unless codespace
	(error "Unsupported codespace version #x~X, we have ~{~x~X~^ ~}~%"
	       magic-number (loop for key being the hash-keys of *codespaces*
				  collect key)))
      (cond
	((not (eq *current-codespace* codespace))
	 (format t "Switching codespace from ~A to #x~X (~A)~%"
		 (codespace-name *current-codespace*)
		 magic-number
		 (codespace-name codespace))
	 (setf *current-codespace* codespace)
	 (restore-objects storage))
	(t
	 (format t "Deserializing from version #x~X (~A)~%"
		 magic-number (codespace-name codespace))
	 (values "hi" :ignore))))))

(defmethod store-action ((action magic-number) storage store-object)
  (store-fixnum (magic-number-number action) storage))
  

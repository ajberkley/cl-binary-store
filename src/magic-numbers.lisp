;; Simple versioning mechanism.  All versioned output should start
;; with +magic-number-code+ and then a number (though it can occur
;; anywhere and even multiple times in the stream / file, not sure why
;; you'd want that).  When restored, the magic number is used to
;; pull up the correct `codespace' in *codespaces* and then restoration
;; continues.  If there is no codespace matching that magic number
;; an error will be signalled.

(in-package :cl-binary-store)

(defvar *write-version* #x0001
  "Set this to the magic number you wish to write into the file.  It may
 be queried by serialization routines if desired.")

(defvar *allow-codespace-switching* t
  "Set this to NIL if you want to specify the format of file you want to load and
 not allow it to be set automatically based on the data format of the file.")

(defstruct (magic-number (:include action (code +magic-number-action-code+)))
  (number #x0001 :type integer :read-only t))

(defmethod action ((code (eql +magic-number-action-code+)) storage references restore-object)
  (let ((magic-number (funcall restore-object)))
    (let ((codespace (gethash magic-number *codespaces*)))
      (unless codespace
	(error "Unsupported codespace version #x~X, we have ~{~x~X~^ ~}~%"
	       magic-number (loop for key being the hash-keys of *codespaces*
				  collect key)))
      (cond
	((not (eq *current-codespace* codespace))
	 (cond
	   (*allow-codespace-switching*
	    (format t "Switching codespace from ~A to #x~X (~A)~%"
		    (codespace-name *current-codespace*)
		    magic-number
		    (codespace-name codespace))
	    (setf *current-codespace* codespace)
	    (setf *version-being-read* magic-number)
	    (restore-objects storage))
	   (t
	    (error "Switching codespace away from #x~X (~A) is DISALLOWED"
		   (codespace-magic-number *current-codespace*)
		   (codespace-name *current-codespace*)))))
	(t
	 (setf *version-being-read* magic-number)
	 (format t "Deserializing from version #x~X (~A)~%"
		 magic-number (codespace-name codespace))
	 (values nil :ignore))))))

(defmethod store-action ((action magic-number) storage store-object)
  (when storage (store-fixnum (magic-number-number action) storage)))
  

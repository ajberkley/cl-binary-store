;; An end marker, useful for cases when sending data over the network
;; or reading from raw memory.

(in-package :cl-binary-store)

(defvar *write-end-marker* nil
  "If T, once all objects are stored an end marker will be written to the output.
 This will trigger the end of restore (for use in cases where there
 isn't an obvious end of file)")

(defstruct (end-marker (:include action (code +end-action-code+))))

(defmethod action ((code (eql +end-action-code+)) storage references restore-object)
  (values nil :end))

(defmethod store-action ((action end-marker) storage store-code))

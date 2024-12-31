;; An end marker, useful for cases when sending data over the network
;; or reading from raw memory.

(in-package :cl-binary-store)

(defstruct (end-marker (:include action (code +end-action-code+))))

(defmethod action ((code (eql +end-action-code+)) storage references restore-object)
  (values nil :end))

(defmethod store-action ((action end-marker) storage store-code))

(in-package :cl-binary-store)

;; During restore we use a vector of references.  It nominally auto-grows as
;; we see references during restore.  Since we know the number of references
;; after restore (since we have allocated them reference ids!) we can write
;; the number in advance to the file to avoid having to keep growing the vector.
;; This only matters when the number of references is huge and is just a small
;; performance tweak.

(defstruct (write-reference-count (:include action (code +set-reference-action-code+)))
  (reference-count nil :type fixnum :read-only t))

(defun write-reference-count (number-of-references store-object)
  (funcall store-object (make-write-reference-count :reference-count number-of-references)))

(defmethod store-action ((action write-reference-count) storage store-object)
  (store-tagged-unsigned-fixnum (write-reference-count-reference-count action) storage))

(defmethod action ((code (eql +set-reference-action-code+)) storage references restore-object)
  (let ((num-refs (restore-tagged-unsigned-fixnum storage)))
    #+info-cbs(format t "This file has ~A references~%" num-refs)
    (unless (<= 0 num-refs (ash most-positive-fixnum -3))
      (unexpected-data "num-refs stored in file invalid"))
    (check-if-too-much-data (read-storage-max-to-read storage) (* 8 num-refs))
    (values (setf (references-vector references) (make-array num-refs :initial-element nil))
	    :ignore)))


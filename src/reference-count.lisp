(in-package :cl-store-faster)

;; During restore we use a vector of references.  It nominally auto-grows as
;; we see references during restore.  Since we know the number of references
;; after restore (since we have allocated them reference ids!) we can write
;; the number in advance to the file to avoid having to keep growing the vector.
;; This only matters when the number of references is huge and is just a small
;; performance tweak.

(defstruct (write-reference-count (:include action (code +set-reference-action-code+)))
  (reference-count nil :type fixnum :read-only t))

(defun write-reference-count (number-of-references storage)
  (store-action +set-reference-action-code+ storage)
  (store-tagged-unsigned-fixnum number-of-references storage))

(defmethod action ((code (eql +set-reference-action-code+)) storage references)
  (let ((num-refs (restore-object storage nil)))
    #+info-csf(format t "This file has ~A references~%" num-refs)
    (setf (references-vector references) (make-array num-refs)))))


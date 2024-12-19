(in-package :cl-store-faster)

(defun store-simple-vector (sv storage)
  (declare (optimize speed safety) (type simple-vector sv))
  (maybe-store-reference-instead (sv storage)
    (store-ub8 +simple-vector+ storage nil)
    (store-tagged-unsigned-integer (length sv) storage)
    (map nil (lambda (elt) (store-object elt storage)) sv)))

(defun restore-simple-vector (storage)
  (declare (optimize speed safety))
  (let* ((num-elts (restore-object storage))
	 (sv (make-array num-elts)))
    ;; It's possible that we can refer to an
    ;; object that is not fully reified yet
    ;; (the only possibility is an array displaced
    ;; to us to which we hold a reference)
    (dotimes (idx num-elts)
      (restore-object-to (svref sv idx) storage))
    sv))

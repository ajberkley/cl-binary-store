(in-package :cl-store-faster)

(defun store-simple-vector (sv storage references)
  (declare (optimize speed safety) (type simple-vector sv))
  (maybe-store-reference-instead (sv storage references)
    (when storage
      (store-ub8 +simple-vector+ storage nil)
      (store-tagged-unsigned-fixnum (length sv) storage))
    (map nil (lambda (elt) (store-object elt storage references)) sv)))

(defun restore-simple-vector (storage references)
  (declare (optimize speed safety))
  (let* ((num-elts (restore-object storage references))
	 (sv (make-array num-elts)))
    ;; It's possible that we can refer to an
    ;; object that is not fully reified yet
    ;; (the only possibility is an array displaced
    ;; to us to which we hold a reference)
    (dotimes (idx num-elts)
      (restore-object-to (svref sv idx) storage references))
    sv))

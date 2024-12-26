(in-package :cl-binary-store)

(defun store-simple-vector (sv storage store-object)
  (declare (optimize speed safety) (type simple-vector sv) (type function store-object))
  (when storage
    (store-ub8 +simple-vector-code+ storage nil)
    (store-unsigned-fixnum (length sv) storage))
  (map nil store-object sv))

(defun restore-simple-vector (storage restore-object)
  (declare (optimize speed safety))
  (let* ((num-elts (restore-tagged-unsigned-fixnum storage))
	 (sv (make-array num-elts)))
    ;; It's possible that we can refer to an
    ;; object that is not fully reified yet
    ;; (the only possibility is an array displaced
    ;; to us to which we hold a reference)
    (dotimes (idx num-elts)
      (restore-object-to (svref sv idx) restore-object))
    sv))

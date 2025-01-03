(in-package :cl-binary-store)

(declaim (inline store-simple-vector))
(defun store-simple-vector (sv storage store-object)
  (declare (optimize speed safety) (type simple-vector sv) (type function store-object))
  (when storage
    (store-ub8/no-tag +simple-vector-code+ storage)
    (store-tagged-unsigned-fixnum/interior (length sv) storage))
  (map nil store-object sv))

(declaim (inline restore-simple-vector))
(defun restore-simple-vector (storage restore-object)
  (declare (optimize speed safety))
  (let* ((num-elts (restore-tagged-unsigned-fixnum/interior storage))
	 (sv (make-array num-elts)))
    ;; It's possible that we can refer to an
    ;; object that is not fully reified yet
    ;; (the only possibility is an array displaced
    ;; to us to which we hold a reference)
    (dotimes (idx num-elts)
      (restore-object-to (svref sv idx) restore-object))
    sv))

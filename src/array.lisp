(in-package :cl-store-faster)

(defun restore-array (storage)
  ;; This is somewhat complex because we cannot build the array
  ;; if it is displaced to another array until we restore what the array
  ;; is displaced to.  So we need to use a fix-up scenario.
  (let* ((has-fill-pointer (restore-object storage))
	 (fill-pointer (when has-fill-pointer (restore-object storage)))
	 (adjustable (restore-object storage))
	 (array-rank (the (unsigned-byte 8) (restore-ub8 storage)))
	 ;; restore tagged integers
	 (dimensions (loop repeat array-rank collect (restore-object storage)))
	 (displaced (restore-object storage)))
    (if displaced
	(let ((element-type (restore-object storage))
	      (offset (restore-object storage))
	      (displaced-to (restore-object storage)))
	  (make-array dimensions :element-type element-type :adjustable adjustable
				 :fill-pointer fill-pointer :displaced-to displaced-to
				 :displaced-index-offset offset))
	(let ((array
		(let* ((element-type (restore-object storage)))
		  (make-array dimensions :element-type element-type :adjustable adjustable
					 :fill-pointer fill-pointer))))
	  (loop for idx fixnum from 0 below (array-total-size array)
		do (restore-object-to (row-major-aref array idx) storage))
	  array))))

(defun store-array (array storage)
  (declare (optimize speed safety) (type array array))
  (maybe-store-reference-instead (array storage)
    ;; (format t "Storing array of type ~A~%" (type-of array))
    (when storage
      (store-ub8 +array+ storage nil)
      (cond
	((array-has-fill-pointer-p array)
	 (store-t storage)
	 (store-tagged-unsigned-integer (fill-pointer array) storage))
	(t
	 (store-nil storage)))
      (store-boolean (adjustable-array-p array) storage)
      (let ((array-dimensions (array-dimensions array)))
	(store-ub8 (length array-dimensions) storage nil) ;; sbcl limits to 128
	(dolist (a array-dimensions)
	  (store-tagged-unsigned-integer (the fixnum a) storage))))
    (multiple-value-bind (next-array offset)
	(array-displacement array)
      (when storage (store-boolean next-array storage))
      ;; element type may be a reference, so we store it after determining
      ;; the array displacement which allows the restore side to register
      ;; the reference for the array either as a delayed/fixup or by
      ;; creating the actual array.
      (let ((elt-type (array-element-type array)))
	(if (eq elt-type t)
	    (store-t storage)
	    (if (symbolp elt-type)
		(store-symbol elt-type storage)
		(store-cons elt-type storage t t))))
      (cond
	(next-array
	 (store-tagged-unsigned-integer offset storage)
	 (store-array next-array storage))
	(t
	 ;; We have to store the array elements even past the fill pointer
	 (dotimes (idx (array-total-size array))
	   (store-object (row-major-aref array idx) storage)))))))

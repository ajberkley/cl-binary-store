(in-package :cl-store-faster)

(defun restore-array (storage references)
  ;; This is somewhat complex because we cannot build the array
  ;; if it is displaced to another array until we restore what the array
  ;; is displaced to.  So we need to use a fix-up scenario.
  (let* ((has-fill-pointer (restore-object storage nil))
	 (fill-pointer (when has-fill-pointer (restore-object storage nil)))
	 (adjustable (restore-object storage nil))
	 (array-rank (the (unsigned-byte 8) (restore-ub8 storage)))
	 ;; restore tagged integers
	 (dimensions (loop repeat array-rank collect (restore-object storage nil)))
	 (displaced (restore-object storage nil)))
    (if displaced
	(let ((element-type (restore-object storage references))
	      (offset (restore-object storage nil))
	      (displaced-to (restore-object storage references)))
	  (make-array dimensions :element-type element-type :adjustable adjustable
				 :fill-pointer fill-pointer :displaced-to displaced-to
				 :displaced-index-offset offset))
	(let ((array
		(let* ((element-type (restore-object storage references)))
		  (make-array dimensions :element-type element-type :adjustable adjustable
					 :fill-pointer fill-pointer))))
	  (loop for idx fixnum from 0 below (array-total-size array)
		do (restore-object-to (row-major-aref array idx) storage references))
	  array))))

(defun store-array (array storage references)
  (declare (optimize speed safety) (type array array))
  (maybe-store-reference-instead (array storage references)
    #+debug-csf(format t "~A array of type ~A~%"
		       (if storage "Storing" "Analyzing")
		       (type-of array))
    (when storage
      (store-ub8 +array+ storage nil)
      (cond
	((array-has-fill-pointer-p array)
	 (store-t storage)
	 (store-tagged-unsigned-fixnum (fill-pointer array) storage))
	(t
	 (store-nil storage)))
      (store-boolean (adjustable-array-p array) storage)
      (let ((array-dimensions (array-dimensions array)))
	(store-ub8 (length array-dimensions) storage nil) ;; sbcl limits to 128
	(dolist (a array-dimensions)
	  (store-tagged-unsigned-fixnum (the fixnum a) storage))))
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
		(store-symbol elt-type storage references)
		(store-cons elt-type storage references))))
      (cond
	(next-array
	 (store-tagged-unsigned-fixnum offset storage)
	 (store-array next-array storage references))
	(t
	 ;; We have to store the array elements even past the fill pointer
	 (dotimes (idx (array-total-size array))
	   (store-object (row-major-aref array idx) storage references)))))))

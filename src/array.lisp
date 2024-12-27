(in-package :cl-binary-store)

(defun restore-array (storage restore-object)
  ;; This is somewhat complex because we cannot build the array
  ;; if it is displaced to another array until we restore what the array
  ;; is displaced to.  So we need to use a fix-up scenario.
  (declare (type function restore-object))
  (let* ((has-fill-pointer (funcall restore-object))
	 (fill-pointer (when has-fill-pointer (funcall restore-object)))
	 (adjustable (funcall restore-object))
	 (array-rank (the (unsigned-byte 8) (restore-ub8 storage)))
	 ;; restore tagged integers
	 (dimensions (loop repeat array-rank collect (funcall restore-object)))
	 (displaced (funcall restore-object)))
    (if displaced
	(let ((element-type (funcall restore-object))
	      (offset (funcall restore-object))
	      (displaced-to (funcall restore-object)))
	  (make-array dimensions :element-type element-type :adjustable adjustable
				 :fill-pointer fill-pointer :displaced-to displaced-to
				 :displaced-index-offset offset))
	(let ((array
		(let* ((element-type (funcall restore-object)))
		  (make-array dimensions :element-type element-type :adjustable adjustable
					 :fill-pointer fill-pointer))))
	  (loop for idx fixnum from 0 below (array-total-size array)
		do (restore-object-to (row-major-aref array idx) restore-object))
	  array))))

(defun store-array (array storage eq-refs store-object)
  (declare (optimize speed safety) (type array array) (type function store-object))
  (maybe-store-reference-instead (array storage eq-refs)
    #+debug-cbs(format t "~A array of type ~A~%"
		       (if storage "Storing" "Analyzing")
		       (type-of array))
    (when storage
      (store-ub8 +array-code+ storage nil)
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
		(store-symbol elt-type storage eq-refs store-object)
		(store-cons elt-type storage eq-refs store-object))))
      (cond
	(next-array
	 (when storage
	   (store-tagged-unsigned-fixnum offset storage))
	 (store-array next-array storage eq-refs store-object))
	(t
	 ;; We have to store the array elements even past the fill pointer
	 (dotimes (idx (array-total-size array))
	   (funcall store-object (row-major-aref array idx))))))))

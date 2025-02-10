(in-package :cl-binary-store)

(defun restore-array (storage restore-object)
  (declare (type function restore-object) (optimize (speed 3) (safety 1)))
  (let* ((has-fill-pointer (funcall restore-object))
	 (fill-pointer (when has-fill-pointer (restore-tagged-unsigned-fixnum storage)))
	 (adjustable (funcall restore-object))
	 (array-rank (the (unsigned-byte 8) (restore-ub8 storage)))
	 (dimensions (loop repeat array-rank
                           collect (restore-tagged-unsigned-fixnum storage)))
	 (displaced (funcall restore-object))
         (array-total-size (reduce #'* dimensions)))
    (unless (and (typep array-total-size 'fixnum) (>= array-total-size 0))
      (unexpected-data "Array total size is too large"))
    (check-if-too-much-data (read-storage-max-to-read storage) array-total-size)
    (labels ((check-fill-pointer (dimensions)
               (when has-fill-pointer
                 (unless (= array-rank 1)
                   (unexpected-data "found fill-pointer for a non-vector"))
                 (unless (<= fill-pointer (first dimensions))
                   (unexpected-data "fill-pointer > vector length")))
               (values)))
      (if displaced
	  (let ((element-type (funcall restore-object))
	        (offset (restore-tagged-unsigned-fixnum storage))
	        (displaced-to (funcall restore-object)))
            (unless (typep displaced-to 'array)
              (unexpected-data "displaced to a non array?!"))
            (unless (typep (array-element-type displaced-to) element-type)
              (unexpected-data "array displaced to array of different element-type"))
            (unless (< offset (array-total-size displaced-to))
              (unexpected-data "array displaced to too small array"))
            (when has-fill-pointer (check-fill-pointer dimensions))
	    (make-array dimensions :element-type element-type :adjustable adjustable
				   :fill-pointer fill-pointer :displaced-to displaced-to
				   :displaced-index-offset offset))
          (progn
            (when has-fill-pointer (check-fill-pointer dimensions))
	    (let ((array
		    (let* ((element-type (funcall restore-object)))
		      (make-array dimensions :element-type element-type :adjustable adjustable
					     :fill-pointer fill-pointer))))
              ;; We need to make our array first in case any of the array elements refer to it!
              ;; If we are ever referred to, then there will already be a fixup in place for
              ;; our array handled by `restore-new-reference-indicator'.
	      (loop for idx fixnum from 0 below array-total-size
		    do (restore-object-to (row-major-aref array idx) restore-object))
	      array))))))

(defun store-array (array storage eq-refs store-object assign-new-reference-id)
  (declare (optimize speed safety) (type array array) (type function store-object))
  (maybe-store-reference-instead (array storage eq-refs assign-new-reference-id)
    #+debug-cbs(format t "~A array of type ~A~%"
		       (if storage "Storing" "Analyzing")
		       (type-of array))
    (when storage
      (store-ub8/no-tag +array-code+ storage)
      (cond
	((array-has-fill-pointer-p array)
	 (store-t storage)
	 (store-tagged-unsigned-fixnum (fill-pointer array) storage))
	(t
	 (store-nil storage)))
      (store-boolean (adjustable-array-p array) storage)
      (let ((array-dimensions (array-dimensions array)))
	(store-ub8/no-tag (length array-dimensions) storage) ;; sbcl limits to 128
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
	    (when storage (store-t storage))
	    (if (symbolp elt-type)
		(store-symbol elt-type storage eq-refs store-object assign-new-reference-id)
		(funcall store-object elt-type))))
      (cond
	(next-array
	 (when storage
	   (store-tagged-unsigned-fixnum offset storage))
	 (store-array next-array storage eq-refs store-object assign-new-reference-id))
	(t
	 ;; We have to store the array elements even past the fill pointer
	 (dotimes (idx (array-total-size array))
	   (funcall store-object (row-major-aref array idx))))))))

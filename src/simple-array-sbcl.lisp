(in-package :cl-store-faster)

(declaim (ftype (function (t &optional (unsigned-byte 58))
			  (values (unsigned-byte 50) (unsigned-byte 8) &optional))
		sbcl-specialized-array-element-size/bits))

(declaim (inline sbcl-specialized-array-element-size/bits))
(defun sbcl-specialized-array-element-size/bits (array &optional (length (length array)))
  "Returns (values bits/elt ub8-encoded-array-type)"
  (declare (optimize speed safety) (type (unsigned-byte 58) length))
  (let ((upgraded (upgraded-array-element-type (array-element-type array))))
    ;; lowest three bits are the element type; upper bits are the bit size if needed
    (case upgraded
      (bit (values (ceiling length 8) 0))
      (base-char (values length 1))
      (character (values (* 4 length) 2))
      ;; TODO We could store the character arrays more compactly with UTF-8 encoding
      (fixnum (values (* 8 length) 3))
      (single-float (values (* 4 length) 4))
      (double-float (values (* 8 length) 5))
      (otherwise
       (ecase (first upgraded)
	 (signed-byte
	  (ecase (second upgraded)
	    (8 (values length 6))
	    (16 (values (* 2 length) 7))
	    (32 (values (* 4 length) 8))
	    (64 (values (* 8 length) 9))))
	 (unsigned-byte
	  (multiple-value-bind (encoded-value actual-bits)
	      (ecase (second upgraded)
		(2 10)
		(4 11)
		(7 (values 12 8))
		(8 13)
		(15 (values 14 16))
		(16 15)
		(31 (values 16 32))
		(32 17)
		(62 (values 18 64))
		(64 19))
	    (values (ceiling (the fixnum (* length
					    (the fixnum (or actual-bits (second upgraded)))))
			     8)
		  encoded-value))))))))

(declaim (inline sbcl-make-simple-array-from-encoded-element-type))
(defun sbcl-make-simple-array-from-encoded-element-type (encoded-element-type num-elts &optional array-dimensions)
  "Returns (values new-array array-bytes)"
  (declare (optimize speed safety) (type (unsigned-byte 8) encoded-element-type)
	   (type (unsigned-byte 59) num-elts))
  (if (< encoded-element-type 6)
      (multiple-value-bind (element-type array-bytes)
	  (case encoded-element-type
	    (0 (values 'bit (ceiling num-elts 8)))
	    (1 (values 'base-char num-elts))
	    (2 (values 'character (* 4 num-elts)))
	    (3 (values 'fixnum (* 8 num-elts)))
	    (4 (values 'single-float (* 4 num-elts)))
	    (5 (values 'double-float (* 8 num-elts))))
	(values (make-array (or array-dimensions num-elts) :element-type element-type)
		array-bytes))
      (multiple-value-bind (type actual-bits)
	  (case encoded-element-type
	    (6 '(signed-byte 8))
	    (7 '(signed-byte 16))
	    (8 '(signed-byte 32))
	    (9 '(signed-byte 64))
	    (10 '(unsigned-byte 2))
	    (11 '(unsigned-byte 4))
	    (12 (values '(unsigned-byte 7) 8))
	    (13 '(unsigned-byte 8))
	    (14 (values '(unsigned-byte 15) 16))
	    (15 '(unsigned-byte 16))
	    (16 (values '(unsigned-byte 31) 32))
	    (17 '(unsigned-byte 32))
	    (18 (values '(unsigned-byte 62) 64))
	    (19 '(unsigned-byte 64)))
	(values (make-array (or array-dimensions num-elts) :element-type type)
		(ceiling (the fixnum
			      (* num-elts
				 (the (integer 0 64) (or actual-bits (second type)))))
			 8)))))

(defun store-simple-specialized-vector (sv storage)
  (declare (optimize speed safety)
	   (type (simple-array * (*)) sv))
  (maybe-store-reference-instead (sv storage)
    (when storage
      (store-ub8 +simple-specialized-vector+ storage nil)
      (let ((sv-length (length sv)))
	(store-tagged-unsigned-integer sv-length storage)
	(multiple-value-bind (bytes-to-write encoded-element-type)
	    (sbcl-specialized-array-element-size/bits sv)
	  ;; (format t "~&SV: Writing a ~A (~A bytes encoded element-type ~A)~%"
	  ;; 	    (type-of sv) bytes-to-write encoded-element-type)
	  (store-ub8 encoded-element-type storage nil)
	  (ensure-enough-room storage bytes-to-write)
	  (let ((offset (storage-offset storage))
		(array (storage-store storage)))
	    (copy-n-bytes array offset sv 0 bytes-to-write)
	    (setf (storage-offset storage) (+ offset bytes-to-write))))))))

(defun restore-simple-specialized-vector (storage)
  (declare (optimize speed safety))
  (let ((num-elts (restore-object storage)))
    (let* ((encoded-element-info (restore-ub8 storage)))
      (multiple-value-bind (sv num-bytes)
	  (sbcl-make-simple-array-from-encoded-element-type encoded-element-info num-elts)
	;; (format t "~&SV: ~A (~A bytes from ~A elts ~A encoded element-type)~%"
	;; 	(type-of sv) num-bytes num-elts encoded-element-info)
	(ensure-enough-data storage num-bytes)
	(let ((offset (storage-offset storage))
	      (array (storage-store storage)))
	  (copy-n-bytes sv 0 array offset num-bytes)
	  (setf (storage-offset storage) (+ num-bytes offset))
	  sv)))))

(defun store-simple-specialized-array (sa storage)
  (declare (optimize speed safety)
	   (type (simple-array * *) sa))
  (maybe-store-reference-instead (sa storage)
    (when storage
      (store-ub8 +simple-specialized-array+ storage nil))
    (let ((array-dimensions (array-dimensions sa))
	  (num-elts (array-total-size sa)))
      (store-ub8 (length array-dimensions) storage nil)
      (dolist (a array-dimensions)
	(store-tagged-unsigned-integer (the fixnum a) storage))
      (multiple-value-bind (bytes-to-write encoded-element-type)
	  (sbcl-specialized-array-element-size/bits sa num-elts)
	(store-ub8 encoded-element-type storage nil)
	;; (format t "~&SA: Writing a ~A (~A bytes encoded element-type ~A)~%"
	;;  	    (type-of sa) bytes-to-write encoded-element-type)
	(ensure-enough-room storage bytes-to-write)
	(let ((offset (storage-offset storage))
	      (array (storage-store storage)))
	  (copy-n-bytes/array array offset sa 0 bytes-to-write)
	  (setf (storage-offset storage) (+ offset bytes-to-write))
	  (values))))))

(defun restore-simple-specialized-array (storage)
  (declare (optimize speed safety))
  ;; sbcl gets confused with restore-ub8 because of the error path
  (let* ((num-array-dimensions (the (unsigned-byte 8) (restore-ub8 storage)))
	 (array-dimensions (loop repeat num-array-dimensions
				 collect (restore-object storage)))
	 (encoded-element-info (restore-ub8 storage)))
    (multiple-value-bind (sa num-bytes)
	(sbcl-make-simple-array-from-encoded-element-type
	 encoded-element-info (reduce #'* array-dimensions) array-dimensions)
      ;; (format t "~&SA: ~A (~A bytes from ~A dims ~A encoded element-type)~%"
      ;; 	(type-of sa) num-bytes array-dimensions encoded-element-info)
      (ensure-enough-data storage num-bytes)
      (let ((offset (storage-offset storage))
	    (array (storage-store storage)))
	(copy-n-bytes/array sa 0 array offset num-bytes)
	(setf (storage-offset storage) (+ num-bytes offset))
	sa))))

(defun store-4-long-sv-sb8 (sv storage)
  (declare (optimize speed safety) (type (simple-array (signed-byte 8) (4)) sv))
  (when storage
    (ensure-enough-room storage 5)
    (let ((offset (storage-offset storage))
	  (array (storage-store storage)))
      (setf (aref array offset) +4-long-sb8-code+)
      (incf offset)
      (copy-n-bytes array offset sv 0 4)
      (setf (storage-offset storage) (+ offset 4)))))

(defun restore-4-long-sv-sb8 (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 4)
  (let ((sv (make-array 4 :element-type '(signed-byte 8))))
    (let ((offset (storage-offset storage))
	  (array (storage-store storage)))
      (copy-n-bytes sv 0 array offset 4)
      (setf (storage-offset storage) (+ offset 4))
      sv)))

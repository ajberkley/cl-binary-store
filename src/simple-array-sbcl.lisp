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

(defun write-sap-data-to-storage (sap num-bytes storage)
  (let ((storage-size (storage-max storage)))
    (declare (type (and (integer 1) fixnum) storage-size))
    (loop
      with num-bytes-remaining fixnum = num-bytes
      with sap-offset fixnum = 0
      for storage-offset = (storage-offset storage)
      while (> num-bytes-remaining 0)
      do
	 (let ((write-length (min storage-size num-bytes-remaining)))
	   (ensure-enough-room-to-write storage write-length)
	   (let ((offset (storage-offset storage)))
	     (copy-sap (storage-sap storage) offset sap sap-offset write-length)
	     (incf sap-offset write-length)
	     (decf num-bytes-remaining write-length)
	     (assert (>= num-bytes-remaining 0))
	     (setf (storage-offset storage) (+ offset write-length)))))))

(declaim (notinline store-simple-base-string))
(defun store-simple-base-string (string storage &optional references)
  (declare (optimize speed safety) (type simple-base-string string))
  (labels ((write-it ()
	     (with-write-storage (storage)
	       (storage-write-byte storage +simple-base-string-code+)
	       (let ((string-length (length string)))
		 (store-tagged-unsigned-fixnum string-length storage)
		 (with-pinned-objects (string)
		   (write-sap-data-to-storage (vector-sap string)
					      string-length storage))))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references)
	  (write-it))
	(write-it))))

(declaim (inline restore-simple-base-string))
(defun restore-simple-base-string (storage)
  (declare (optimize speed safety))
  (let* ((num-bytes (restore-object storage nil))
	 (string (make-string num-bytes :element-type 'base-char)))
    (ensure-enough-data storage num-bytes)
    (let ((offset (storage-offset storage))
	  (sap (storage-sap storage)))
      (with-pinned-objects (string)
        (copy-sap (vector-sap string) 0 sap offset num-bytes))
      (setf (storage-offset storage) (+ num-bytes offset))
      string)))

(declaim (notinline store-simple-string))
(defun store-simple-string (string storage &optional references)
  (declare (optimize speed safety) (type simple-string string))
  (labels ((write-it ()
	     (with-write-storage (storage)
	       #+sb-unicode
	       ;; Ideally we'd avoid this copy
	       (let* ((output (sb-ext:string-to-octets string :external-format :utf-8))
		      (num-bytes (length output)))
		 (storage-write-byte storage +simple-string-code+)
		 (store-tagged-unsigned-fixnum num-bytes storage)
		 (let ((offset (ensure-enough-room-to-write storage num-bytes)))
                   (with-pinned-objects (output)
                     (copy-sap (storage-sap storage) offset (vector-sap output) 0 num-bytes))
		   (setf (storage-offset storage) (+ offset num-bytes))))
	       #-sb-unicode (store-simple-base-string string storage nil))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references)
	  (write-it))
	(write-it))))

(declaim (notinline restore-simple-string))
(defun restore-simple-string (storage)
  (declare (optimize speed safety))
  (let* ((num-bytes (restore-object storage nil))) ;; restore-unsigned-fixnum?
    (ensure-enough-data storage num-bytes)
    (let ((offset (storage-offset storage))
          (store (storage-store storage))
	  (sap (storage-sap storage)))
      (setf (storage-offset storage) (+ num-bytes offset))
      (if (> (length store) 0)
          (sb-ext:octets-to-string store :external-format :utf-8 :start offset
				         :end (+ offset num-bytes))
          (let ((a (make-array num-bytes :element-type '(unsigned-byte 8))))
            (declare (dynamic-extent a))
            (with-pinned-objects (a)
              (copy-sap (vector-sap a) 0 sap offset num-bytes))
            (sb-ext:octets-to-string a :external-format :utf-8 :start 0
                                     :end num-bytes))))))

(declaim (notinline store-string))
(defun store-string (string storage references)
  (etypecase string
    (simple-base-string (store-simple-base-string string storage references))
    (simple-string (store-simple-string string storage references))))

(declaim (notinline store-string/no-refs))
(defun store-string/no-refs (string storage)
  (etypecase string
    (simple-base-string (store-simple-base-string string storage nil))
    (simple-string (store-simple-string string storage nil))))

(declaim (notinline restore-string))
(defun restore-string (storage)
  "Can only be used if you are sure that the strings are not references,
 so that means they must have been stored with store-string/no-refs"
  (declare (optimize speed safety))
  (let ((code (restore-ub8 storage)))
    (ecase code
      (#.+simple-base-string-code+ (restore-simple-base-string storage))
      (#.+simple-string-code+ (restore-simple-string storage)))))

(declaim (notinline restore-string/maybe-reference))
(defun restore-string/maybe-reference (storage references)
  "Can only be used if you are sure that the strings are not references,
 so that means they must have been stored with store-string/no-refs"
  (declare (optimize speed safety))
  (let ((code (restore-ub8 storage)))
    (case code
      (#.+simple-base-string-code+ (restore-simple-base-string storage))
      (#.+simple-string-code+ (restore-simple-string storage))
      (otherwise (read-dispatch code storage references)))))

(declaim (notinline store-simple-specialized-vector))
(defun store-simple-specialized-vector (sv storage &optional references)
  (declare (optimize speed safety) (type (simple-array * (*)) sv))
  (labels ((write-it ()
	     (with-write-storage (storage)
	       (storage-write-byte storage +simple-specialized-vector+)
	       (let ((sv-length (length sv)))
		 (store-tagged-unsigned-fixnum sv-length storage)
		 (multiple-value-bind (bytes-to-write encoded-element-type)
		     (sbcl-specialized-array-element-size/bits sv)
		   #+debug-csf (format t "~&SV: Writing a ~A (~A bytes encoded element-type ~A)~%"
				       (type-of sv) bytes-to-write encoded-element-type)
		   (storage-write-byte storage encoded-element-type)
		   (sb-sys:with-pinned-objects (sv)
		     (write-sap-data-to-storage
		      (sb-sys:vector-sap sv) bytes-to-write storage)))))))

    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (sv storage references)
	  (write-it))
	(write-it))))

(defun restore-simple-specialized-vector (storage)
  (declare (optimize speed safety))
  (let ((num-elts (restore-object storage nil)))
    #+debug-csf (format t "NUM ELETS ~A~%" num-elts)
    (let* ((encoded-element-info (restore-ub8 storage)))
      (multiple-value-bind (sv num-bytes)
	  (sbcl-make-simple-array-from-encoded-element-type encoded-element-info num-elts)
	#+debug-csf (format t "~&SV: ~A (~A bytes from ~A elts ~A encoded element-type)~%"
			    (type-of sv) num-bytes num-elts encoded-element-info)
	(ensure-enough-data storage num-bytes)
	(let ((offset (storage-offset storage))
	      (sap (storage-sap storage)))
          (with-pinned-objects (sv)
            (copy-sap (vector-sap sv) 0 sap offset num-bytes))
	  (setf (storage-offset storage) (+ num-bytes offset))
	  sv)))))

(defun store-simple-specialized-array (sa storage references)
  (declare (optimize speed safety)
	   (type (simple-array * *) sa))
  (maybe-store-reference-instead (sa storage references)
    (with-write-storage (storage)
      (storage-write-byte storage +simple-specialized-array+)
      (let ((array-dimensions (array-dimensions sa))
	    (num-elts (array-total-size sa)))
	(storage-write-byte storage (length array-dimensions))
	(dolist (a array-dimensions)
	  (store-tagged-unsigned-fixnum (the fixnum a) storage))
	(multiple-value-bind (bytes-to-write encoded-element-type)
	    (sbcl-specialized-array-element-size/bits sa num-elts)
	  (storage-write-byte storage encoded-element-type)
	  #+debug-csf (format t "~&SA: Writing a ~A (~A bytes encoded element-type ~A)~%"
	 		      (type-of sa) bytes-to-write encoded-element-type)
	  (sb-kernel:with-array-data ((backing-array sa) (start) (end))
	    (assert (zerop start))
	    (sb-sys:with-pinned-objects (backing-array)
	      (write-sap-data-to-storage (sb-sys:vector-sap backing-array) bytes-to-write
					 storage)))
	  (values))))))

(defun restore-simple-specialized-array (storage)
  (declare (optimize speed safety))
  ;; sbcl gets confused with restore-ub8 because of the error path
  (let* ((num-array-dimensions (the (unsigned-byte 8) (restore-ub8 storage)))
	 (array-dimensions (loop repeat num-array-dimensions
				 collect (restore-object storage nil)))
	 (encoded-element-info (restore-ub8 storage)))
    (multiple-value-bind (sa num-bytes)
	(sbcl-make-simple-array-from-encoded-element-type
	 encoded-element-info (reduce #'* array-dimensions) array-dimensions)
      #+debug-csf (format t "~&SA: ~A (~A bytes from ~A dims ~A encoded element-type)~%"
	(type-of sa) num-bytes array-dimensions encoded-element-info)
      (ensure-enough-data storage num-bytes)
      (let ((offset (storage-offset storage))
	    (sap (storage-sap storage)))
        (with-pinned-objects (sa)
	  (copy-sap (array-sap sa) 0 sap offset num-bytes))
	(setf (storage-offset storage) (+ num-bytes offset))
	sa))))

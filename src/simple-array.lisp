(in-package :cl-binary-store)

#+sbcl
(declaim (ftype (function (t &optional (unsigned-byte 58))
			  (values (unsigned-byte 50) (unsigned-byte 8) &optional))
		sbcl-specialized-array-element-size/bits))

#+sbcl
(declaim (inline sbcl-specialized-array-element-size/bits))
#+sbcl
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
#+sbcl
(declaim (inline sbcl-make-simple-array-from-encoded-element-type))
#+sbcl
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

#+sbcl
(defun write-sap-data-to-storage (sap num-bytes storage)
  (let ((storage-size (write-storage-max storage)))
    (declare (type (and (integer 1) fixnum) storage-size))
    (loop
      with num-bytes-remaining fixnum = num-bytes
      with sap-offset fixnum = 0
      for storage-offset = (write-storage-offset storage)
      while (> num-bytes-remaining 0)
      do
	 (let ((write-length (min storage-size num-bytes-remaining)))
	   (ensure-enough-room-to-write storage write-length)
	   (let ((offset (write-storage-offset storage)))
	     (copy-sap (write-storage-sap storage) offset sap sap-offset write-length)
	     (incf sap-offset write-length)
	     (decf num-bytes-remaining write-length)
	     (assert (>= num-bytes-remaining 0))
	     (setf (write-storage-offset storage) (truly-the fixnum (+ offset write-length))))))))

(declaim (notinline store-simple-base-string))
(defun store-simple-base-string (string storage &optional references assign-new-reference-id)
  (declare (optimize speed safety) (type simple-base-string string))
  (labels ((write-it ()
	     (with-write-storage (storage)
	       (storage-write-byte storage +simple-base-string-code+)
	       (let ((string-length (length string)))
		 (store-tagged-unsigned-fixnum string-length storage)
		 #+sbcl (with-pinned-objects (string)
			  (write-sap-data-to-storage (vector-sap string)
						     string-length storage))
		 #-sbcl (loop
			  with sap = (write-storage-sap storage)
			  for char of-type base-char across string
			  for string-offset from 0 below string-length
			  for sap-offset from (write-storage-offset storage)
			  do (setf (cffi:mem-ref sap :uint8 sap-offset) (char-code char))
			  finally (incf (write-storage-offset storage) string-length))))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references assign-new-reference-id)
	  (write-it))
	(write-it))))

(declaim (notinline restore-simple-base-string))
(defun restore-simple-base-string (storage)
  (declare (optimize speed safety))
  (let* ((num-bytes (restore-tagged-unsigned-fixnum storage))
	 (string (make-string num-bytes :element-type 'base-char)))
    (ensure-enough-data storage num-bytes)
    (let ((offset (read-storage-offset storage))
	  (sap (read-storage-sap storage)))
      #+sbcl (with-pinned-objects (string)
               (copy-sap (vector-sap string) 0 sap offset num-bytes))
      #-sbcl (loop for string-offset from 0 below num-bytes
		   for sap-offset from offset
		   do (setf (aref string string-offset)
			    (code-char (cffi:mem-ref sap :uint8 sap-offset))))
      (setf (read-storage-offset storage) (truly-the fixnum (+ num-bytes offset)))
      string)))

(declaim (notinline store-simple-string))
(defun store-simple-string (string storage &optional references assign-new-reference-id)
  (declare (optimize speed safety) (type simple-string string))
  (labels ((write-it ()
	     (with-write-storage (storage)
	       #+(or (not sbcl) (and sbcl sb-unicode))
	       ;; Ideally we'd avoid this copy
	       (let* ((output #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
			      #-sbcl (babel:string-to-octets string :encoding :utf-8))
		      (num-bytes (length output)))
		 (storage-write-byte storage +simple-string-code+)
		 (store-tagged-unsigned-fixnum num-bytes storage)
		 (let ((offset (ensure-enough-room-to-write storage num-bytes)))
                   #+sbcl
		   (with-pinned-objects (output)
                     (copy-sap (write-storage-sap storage) offset (vector-sap output) 0 num-bytes))
		   #-sbcl
		   (loop
		     with sap = (write-storage-sap storage)
		     for uint8 across output
		     for sap-offset from offset
		     do (setf (cffi:mem-ref sap :uint8 sap-offset) uint8))
		   (setf (write-storage-offset storage) (truly-the fixnum (+ offset num-bytes)))))
	       #+(and sbcl (not sb-unicode)) (store-simple-base-string string storage nil))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references assign-new-reference-id)
	  (write-it))
	(write-it))))

(declaim (notinline restore-simple-string))
(defun restore-simple-string (storage)
  (declare (optimize speed safety))
  (let* ((num-bytes (restore-tagged-unsigned-fixnum storage)))
    (ensure-enough-data storage num-bytes)
    (let ((offset (read-storage-offset storage))
          (store (read-storage-store storage))
	  (sap (read-storage-sap storage)))
      (setf (read-storage-offset storage) (truly-the fixnum (+ num-bytes offset)))
      (if (> (length store) 0)
          (babel:octets-to-string store :encoding :utf-8 :start offset
				         :end (the fixnum (+ offset num-bytes)))
          (let ((a (make-array num-bytes :element-type '(unsigned-byte 8))))
            (declare (dynamic-extent a))
            #+sbcl (with-pinned-objects (a)
		     (copy-sap (vector-sap a) 0 sap offset num-bytes))
	    #-sbcl (loop for sap-offset from offset
			 for string-offset from 0 below num-bytes
			 do (setf (aref a string-offset) (cffi:mem-ref sap :uint8 sap-offset)))
            (babel:octets-to-string a :encoding :utf-8 :start 0 :end num-bytes))))))

(declaim (notinline store-string))
(defun store-string (string storage references assign-new-reference-id)
  (etypecase string
    (simple-base-string (store-simple-base-string string storage references assign-new-reference-id))
    (simple-string (store-simple-string string storage references assign-new-reference-id))))

(declaim (notinline store-string/no-refs))
(defun store-string/no-refs (string storage)
  (etypecase string
    (simple-base-string (store-simple-base-string string storage))
    (simple-string (store-simple-string string storage))))

(declaim (notinline restore-string))
(defun restore-string (storage)
  "Can only be used if you are sure that the strings are not references,
 so that means they must have been stored with store-string/no-refs"
  (declare (optimize speed safety))
  (let ((code (restore-ub8 storage)))
    (ecase code
      (#.+simple-base-string-code+ (restore-simple-base-string storage))
      (#.+simple-string-code+ (restore-simple-string storage)))))

#-sbcl
(defun store-simple-specialized-vector (sv storage &optional (tag t))
  (error "implement me"))
#+sbcl
(defun store-simple-specialized-vector (sv storage &optional (tag t))
  (declare (optimize speed safety) (type (simple-array * (*)) sv))
  (with-write-storage (storage)
    (when tag (storage-write-byte storage +simple-specialized-vector-code+))
    (let ((sv-length (length sv)))
      (store-tagged-unsigned-fixnum sv-length storage)
      (multiple-value-bind (bytes-to-write encoded-element-type)
	  (sbcl-specialized-array-element-size/bits sv)
	#+debug-cbs (format t "~&SV: Writing a ~A (~A bytes encoded element-type ~A)~%"
			    (type-of sv) bytes-to-write encoded-element-type)
	(storage-write-byte storage encoded-element-type)
	(sb-sys:with-pinned-objects (sv)
	  (write-sap-data-to-storage
	   (sb-sys:vector-sap sv) bytes-to-write storage))))))

#+sbcl
(defun read-chunked (target-sap storage num-bytes-remaining)
  (declare (optimize speed safety) (type fixnum num-bytes-remaining))
  (loop
    with storage-size = (read-storage-size storage)
    with sap = (read-storage-sap storage)
    with num-bytes-read fixnum = 0
    while (> num-bytes-remaining 0)
    for bytes-to-read fixnum = (if (/= (read-storage-offset storage) (read-storage-max storage))
                                   (min (the fixnum (- (read-storage-max storage) (read-storage-offset storage)))
                                        num-bytes-remaining)
                                   (min storage-size num-bytes-remaining))
    do
       #+debug-cbs(format t "At storage offset ~A and storage-max ~A, asking for ~A bytes to read, ~A bytes remaining~%"
               (read-storage-offset storage) (read-storage-max storage) bytes-to-read num-bytes-remaining)
       (ensure-enough-data storage bytes-to-read)
       (let ((offset (read-storage-offset storage)))
         (copy-sap target-sap num-bytes-read sap offset bytes-to-read)
         (incf num-bytes-read bytes-to-read)
         (decf num-bytes-remaining bytes-to-read)
	 (setf (read-storage-offset storage) (truly-the fixnum (+ bytes-to-read offset))))))

#-sbcl
(defun restore-simple-specialized-vector (storage)
  (error "write me"))
#+sbcl
(defun restore-simple-specialized-vector (storage)
  (declare (optimize speed safety))
  (let ((num-elts (restore-tagged-unsigned-fixnum storage)))
    (let* ((encoded-element-info (restore-ub8 storage)))
      (multiple-value-bind (sv num-bytes)
	  (sbcl-make-simple-array-from-encoded-element-type encoded-element-info num-elts)
	#+debug-cbs (format t "~&SV: ~A (~A bytes from ~A elts ~A encoded element-type)~%"
			    (type-of sv) num-bytes num-elts encoded-element-info)
        (with-pinned-objects (sv)
          (read-chunked (vector-sap sv) storage num-bytes))
        sv))))

#-sbcl
(defun store-simple-specialized-array (sa storage)
  (error "write me"))
#+sbcl
(defun store-simple-specialized-array (sa storage)
  (declare (optimize speed safety)
	   (type (simple-array * *) sa))
  (with-write-storage (storage)
    (storage-write-byte storage +simple-specialized-array-code+)
    (let* ((array-dimensions (array-dimensions sa))
	   (num-elts (array-total-size sa)))
      (storage-write-byte storage (length array-dimensions))
      (dolist (a array-dimensions)
	(store-tagged-unsigned-fixnum (the fixnum a) storage))
      (multiple-value-bind (bytes-to-write encoded-element-type)
	  (sbcl-specialized-array-element-size/bits sa num-elts)
	(storage-write-byte storage encoded-element-type)
	#+debug-cbs (format t "~&SA: Writing a ~A (~A bytes encoded element-type ~A)~%"
	 		    (type-of sa) bytes-to-write encoded-element-type)
	(sb-kernel:with-array-data ((backing-array sa) (start) (end))
          (assert (= end num-elts))
	  (assert (zerop start))
	  (sb-sys:with-pinned-objects (backing-array)
	    (write-sap-data-to-storage (sb-sys:vector-sap backing-array) bytes-to-write
				       storage)))
	(values)))))

#+sbcl
(defun restore-simple-specialized-array (storage)
  (declare (optimize speed safety))
  ;; sbcl gets confused with restore-ub8 because of the error path
  (let* ((num-array-dimensions (the (unsigned-byte 8) (restore-ub8 storage)))
	 (array-dimensions (loop repeat num-array-dimensions
				 collect (restore-tagged-unsigned-fixnum storage)))
	 (encoded-element-info (restore-ub8 storage)))
    (multiple-value-bind (sa num-bytes)
	(sbcl-make-simple-array-from-encoded-element-type
	 encoded-element-info (reduce #'* array-dimensions) array-dimensions)
      #+debug-cbs (format t "~&SA: ~A (~A bytes from ~A dims ~A encoded element-type)~%"
	                  (type-of sa) num-bytes array-dimensions encoded-element-info)
      (with-pinned-objects (sa)
        (read-chunked (array-sap sa) storage num-bytes))
      sa)))

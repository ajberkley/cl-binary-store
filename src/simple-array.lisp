(in-package :cl-binary-store)

;; TODO CHECK INTEROP, DID I REVERSE BITS ON BIT VECTORS, ETC.

(declaim (ftype (function (t &optional (unsigned-byte 58))
			  (values (unsigned-byte 50) (unsigned-byte 8) &optional))
		specialized-array-element-size/bits))

(declaim (inline specialized-array-element-size/bits))
(defun specialized-array-element-size/bits (array &optional (length (length array)))
  "Returns (values bits/elt ub8-encoded-array-type).  64 bit specific.  This is used during
 serialization.  Inverse of encoded-element-type-to-type/packing"
  (declare (optimize (speed 3) (safety 1)) (type (unsigned-byte 58) length))
  (let ((upgraded (upgraded-array-element-type (array-element-type array))))
    #+ecl
    (setf upgraded
	  (case upgraded
	    (ext:integer64 '(signed-byte 64))
	    (ext:integer32 '(signed-byte 32))
	    (ext:integer16 '(signed-byte 16))
	    (ext:integer8 '(signed-byte 8))
	    (ext:byte8 '(unsigned-byte 8))
	    (ext:byte16 '(unsigned-byte 16))
	    (ext:byte32 '(unsigned-byte 32))
	    (ext:byte64 '(unsigned-byte 64))
	    (otherwise upgraded)))
    (case upgraded
      (bit (values (ceiling length 8) 0))
      (base-char (values length 1))
      (character (values (* 4 length) 2))
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
		(1 0)
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

(declaim (inline encoded-value-to-type/packing))
(defun encoded-element-type-to-type/packing (encoded-element-type)
  "How many bits per element when reading back in data.  We may be reading data that
 was stored from an array type that does not exist in the reader implementation, so
 consider this a definition of our serialization scheme."
  (case encoded-element-type
    (0 (values 'bit 1))
    (1 (values 'base-char 8))
    (2 (values 'character 32))
    (3 (values 'fixnum 64))
    (4 (values 'single-float 32))
    (5 (values 'double-float 64))
    (6 (values '(signed-byte 8) 8))
    (7 (values '(signed-byte 16) 16))
    (8 (values '(signed-byte 32) 32))
    (9 (values '(signed-byte 64) 64))
    (10 (values '(unsigned-byte 2) 2)) ;; sbcl actually stores things as 2 bits
    (11 (values '(unsigned-byte 4) 4)) ;; sbcl actually stores things as 4 bits
    (12 (values '(unsigned-byte 7) 8))
    (13 (values '(unsigned-byte 8) 8))
    (14 (values '(unsigned-byte 15) 16))
    (15 (values '(unsigned-byte 16) 16))
    (16 (values '(unsigned-byte 31) 32))
    (17 (values '(unsigned-byte 32) 32))
    (18 (values '(unsigned-byte 62) 64))
    (19 (values '(unsigned-byte 64) 64))
    (otherwise
     (unexpected-data "encoded element type" encoded-element-type))))

(defconstant +first-direct-unsigned-integer+ 20)
(defconstant +max-direct-encoded-unsigned-integer+ (- 255 +first-direct-unsigned-integer+))

(declaim (inline make-simple-array-from-encoded-element-type))
(defun make-simple-array-from-encoded-element-type (storage encoded-element-type num-elts &optional array-dimensions)
  "Returns (values new-array array-bytes)"
  (declare (optimize (speed 3) (safety 1)) (type (unsigned-byte 8) encoded-element-type)
           (type fixnum num-elts))
  (unless (<= num-elts (ash most-positive-fixnum -3))
    (unexpected-data (format nil "num-elts smaller than ~A" (ash most-positive-fixnum -3)) num-elts))
  (let ((num-elts num-elts))
    (declare (type (unsigned-byte 59) num-elts))
    (multiple-value-bind (type actual-bits)
        (encoded-element-type-to-type/packing encoded-element-type)
      (let ((total-bytes (ceiling (the fixnum
			               (* num-elts
			                  (the (integer 0 64) actual-bits)))
				  8)))
        (check-if-too-much-data (read-storage-max-to-read storage) total-bytes)
        (values (make-array (or array-dimensions num-elts) :element-type type)
	        total-bytes)))))

#+sbcl
(defun write-sap-data-to-storage (sap num-bytes storage)
  (let ((storage-size (write-storage-max storage)))
    (declare (type (and (integer 1) fixnum) storage-size))
    (loop
      with num-bytes-remaining fixnum = num-bytes
      with sap-offset fixnum = 0
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

(declaim (inline chunked/write))
(defun chunked/write (storage num-bytes function)
  (declare (optimize (speed 3) (safety 1)))
  "Used when we cannot do this directly from the sap"
  (let ((storage-size (write-storage-max storage)))
    (declare (type (and (integer 1) fixnum) storage-size))
    (loop
      with num-bytes-remaining fixnum = num-bytes
      with data-start-bytes fixnum = 0
      while (> num-bytes-remaining 0)
      do
	 (let ((write-length/bytes (min storage-size num-bytes-remaining)))
	   (ensure-enough-room-to-write storage write-length/bytes)
	   (let ((sap-offset (write-storage-offset storage))
		 (sap (write-storage-sap storage)))
	     (funcall function sap sap-offset data-start-bytes
		      (the fixnum (+ data-start-bytes write-length/bytes)))
	     (incf data-start-bytes write-length/bytes)
	     (setf (write-storage-offset storage) (truly-the fixnum
						    (+ sap-offset write-length/bytes)))
	     (decf num-bytes-remaining write-length/bytes))))))

(declaim (inline chunked/read))
(defun chunked/read (storage num-bytes function)
  (declare (optimize (speed 3) (safety 1)))
  (let ((storage-size (read-storage-max storage)))
    (declare (type (and (integer 1) fixnum) storage-size))
    (loop
      with num-bytes-remaining fixnum = num-bytes
      with data-start-bytes fixnum = 0
      while (> num-bytes-remaining 0)
      do
	 (let ((read-length/bytes (min storage-size num-bytes-remaining)))
	   (ensure-enough-data storage read-length/bytes)
	   (let ((sap-offset (read-storage-offset storage))
		 (sap (read-storage-sap storage)))
	     (funcall function sap sap-offset data-start-bytes
		      (the fixnum (+ data-start-bytes read-length/bytes)))
	     (incf data-start-bytes read-length/bytes)
	     (setf (read-storage-offset storage) (truly-the fixnum
						    (+ sap-offset read-length/bytes)))
	     (decf num-bytes-remaining read-length/bytes))))))

(declaim (notinline store-simple-base-string))
(defun store-simple-base-string (string storage &optional references assign-new-reference-id)
  (declare (optimize (speed 3) (safety 1)) (type simple-base-string string))
  (labels ((write-it ()
	     (when storage
	       (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
		 (set-sap-ref-8 sap offset +simple-base-string-code+))
	       (let ((string-length (length string)))
		 (store-tagged-unsigned-fixnum/interior string-length storage)
		 #+sbcl (with-pinned-objects (string)
			  (write-sap-data-to-storage (vector-sap string)
						     string-length storage))
		 #-sbcl
		 (chunked/write storage string-length
				(lambda (sap sap-offset-original string-start string-end)
				  (loop
				    for idx fixnum from string-start below string-end
				    for sap-offset fixnum from sap-offset-original
				    do (setf (sap-ref-8 sap sap-offset) (char-code (aref string idx))))))))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references assign-new-reference-id)
	  (write-it))
	(write-it))))

(declaim (notinline restore-simple-base-string))
(defun restore-simple-base-string (storage)
  (declare (optimize (speed 3) (safety 1)))
  (let ((num-bytes (restore-tagged-unsigned-fixnum/interior storage)))
    (unless (and (typep num-bytes 'fixnum) (>= num-bytes 0))
      (unexpected-data "unsigned fixnum" num-bytes))
    (check-if-too-much-data (read-storage-max-to-read storage) num-bytes)
    (let ((string (make-string num-bytes :element-type 'base-char)))
      (chunked/read storage num-bytes
		    (lambda (sap sap-offset-original string-start string-end)
		      #+sbcl
		      (with-pinned-objects (string)
		        (copy-sap (vector-sap string) string-start sap sap-offset-original
				  (- string-end string-start)))
		      #-sbcl
		      (loop
		        for idx fixnum from string-start below string-end
		        for sap-offset fixnum from sap-offset-original
		        do (setf (aref string idx) (code-char (sap-ref-8 sap sap-offset))))))
      string)))

(declaim (notinline store-simple-string))
(defun store-simple-string (string storage &optional references assign-new-reference-id)
  (declare (optimize (speed 3) (safety 1)) (type simple-string string))
  (labels ((write-it ()
	     #+(or (not sbcl) (and sbcl sb-unicode))
	     (when storage
	       (with-write-storage (storage :sap sap :offset offset :reserve-bytes 1)
	       	 (set-sap-ref-8 sap offset +simple-string-code+))
	       ;; Ideally we'd avoid this copy
	       (let* ((output #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
			      #-sbcl (babel:string-to-octets string :encoding :utf-8))
		      (num-bytes (length output))) 
		 (store-tagged-unsigned-fixnum/interior num-bytes storage)
		 (chunked/write storage num-bytes
				(lambda (sap offset string-start string-end)
				  #+sbcl
				  (with-pinned-objects (output)
				    (copy-sap sap offset (vector-sap output) string-start (- string-end string-start)))
				  #-sbcl
				  (loop
				    for idx fixnum from string-start below string-end
				    for sap-offset fixnum from offset
				    do (setf (sap-ref-8 sap sap-offset) (char-code (aref string idx))))
				  #+(and sbcl (not sb-unicode)) (store-simple-base-string string storage nil)))))))
    (declare (inline write-it))
    (if references
	(maybe-store-reference-instead (string storage references assign-new-reference-id)
	  (write-it))
	(write-it))))

(declaim (notinline restore-simple-string))
(defun restore-simple-string (storage)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((num-bytes (restore-tagged-unsigned-fixnum/interior storage)))
    (check-if-too-much-data (read-storage-max-to-read storage) num-bytes)
    (labels ((read/decode (a)
               (chunked/read storage num-bytes
		             (lambda (sap offset string-start string-end)
		               #+sbcl (with-pinned-objects (a)
			                (copy-sap (vector-sap a) string-start sap offset (- string-end string-start)))
		               #-sbcl (loop for sap-offset from offset
				            for string-offset from string-start below string-end
				            do (setf (aref a string-offset) (sap-ref-8 sap sap-offset)))))
               (babel:octets-to-string a :encoding :utf-8 :start 0 :end num-bytes)))
      (if (< num-bytes 65536)
          (let ((a (make-array num-bytes :element-type '(unsigned-byte 8))))
            (declare (dynamic-extent a))
            (read/decode a))
          (let ((a (make-array num-bytes :element-type '(unsigned-byte 8))))
            (read/decode a))))))

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
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (restore-ub8 storage)))
    (case code
      (#.+simple-base-string-code+ (restore-simple-base-string storage))
      (#.+simple-string-code+ (restore-simple-string storage))
      (otherwise
       (cerror "USE-EMPTY-STRING" 'invalid-input-data :format-control "While restoring a string expected one of ~A, found ~A" :format-arguments (list #.(format nil "(~A ~A)" +simple-string-code+ +simple-base-string-code+) code))
       ""))))

(defmacro make-writer/reader (size-bits signed &key name-override reader array-type)
  (let* ((writer (not reader))
	 (set/get (if (>= size-bits 8)
		      (or name-override (intern (format nil "~A~ASAP-REF-~A" (if writer "SET-" "") (if signed "SIGNED-" "") size-bits)))
		      (intern (format nil "~A~ASAP-REF-8" (if writer "SET-" "") (if signed "SIGNED-" ""))))))
    `(locally (declare (type ,(or array-type `(simple-array ,(list (if signed 'signed-byte 'unsigned-byte) size-bits) (*))) sv))
       (,(if reader 'chunked/read 'chunked/write)
	storage ,(if (>= size-bits 8)
		     `(* ,(ash size-bits -3) (length sv))
		     `(floor (* (length sv) ,size-bits) 8))
	(lambda (sap sap-offset data-start/bytes data-end/bytes)
	  (declare (type fixnum sap-offset data-start/bytes data-end/bytes) (optimize (speed 3) (safety 1)))
	  (loop for data-offset fixnum from ,(if (>= size-bits 8) `(ash data-start/bytes ,(* -1 (round (log (ash size-bits -3) 2)))) `data-start/bytes)
		  below ,(if (>= size-bits 8) `(ash data-end/bytes ,(* -1 (round (log (ash size-bits -3) 2)))) `data-end/bytes)
		do
		,(if reader
		     (if (>= size-bits 8)
			 `(setf (aref sv data-offset) (,set/get sap sap-offset))
			 `(let ((result (,set/get sap sap-offset)))
			    (dotimes (chunk ,(/ 8 size-bits))
			      (setf (aref sv (+ (* data-offset ,(/ 8 size-bits)) chunk)) (logand result ,(dpb 255 (byte size-bits 0) 0)))
			      (setf result (ash result ,(- size-bits))))))
		     `(,set/get
		       sap sap-offset
		       ,(if (>= size-bits 8)
			    `(aref sv data-offset)
			    `(let ((result 0))
			       (declare (type (unsigned-byte 8) result))
			       (dotimes (chunk ,(/ 8 size-bits))
				 (incf result (the fixnum (ash (aref sv (+ (truly-the fixnum (* data-offset ,(/ 8 size-bits))) chunk)) (* chunk ,size-bits)))))
			       result))))
		 (the fixnum (incf sap-offset ,(if (>= size-bits 8) (ash size-bits -3) 1))))))
       ;; Left over bits
       ,(when (< size-bits 8)
	  `(let ((elt-offset (* ,(/ 8 size-bits) (floor (length sv) ,(/ 8 size-bits)))))
	     (unless (= elt-offset (length sv))
	       ,(if reader
		    `(progn
		       (ensure-enough-data storage 1)
		       (let* ((offset (read-storage-offset storage))
			      (result (sap-ref-8 (read-storage-sap storage) offset)))
			 (setf (read-storage-offset storage) (+ 1 offset))
			 (loop for elt from elt-offset below (length sv)
			       for count fixnum from 0 by ,size-bits
			       do (setf (aref sv elt) (logand result ,(dpb 255 (byte size-bits 0) 0)))
				  (setf result (the fixnum (ash (the fixnum result) ,(- size-bits)))))))
		    `(with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
		       (,(intern (format nil "~A~ASAP-REF-8" (if writer "SET-" "") (if signed "SIGNED-" "")))
			sap offset (loop for elt from elt-offset below (length sv)
					 for count fixnum from 0 by ,size-bits
					 summing (the fixnum (ash (the fixnum (aref sv elt)) count)) fixnum))))))))))

(defmacro writer (bits signed &optional name-override array-type)
  `(make-writer/reader ,bits ,signed :name-override ,name-override :reader nil :array-type ,array-type))

(defmacro reader (bits signed &optional name-override array-type)
  `(make-writer/reader ,bits ,signed :name-override ,name-override :reader t :array-type ,array-type))

(defun store-elts (storage sv encoded-element-type)
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (type size/bits)
      (encoded-element-type-to-type/packing encoded-element-type)
    (cond
      ((consp type)
       (ecase (first type)
	 (unsigned-byte
	  (ecase size/bits
	    (1 (writer 1 nil))
	    (2 (writer 2 nil))
	    (4 (writer 4 nil))
	    (8 (writer 8 nil))
	    (16 (writer 16 nil))
	    (32 (writer 32 nil))
	    (64 (writer 64 nil))))
	 (signed-byte
	  (ecase size/bits
	    (1 (writer 1 nil))
	    (2 (writer 2 nil))
	    (4 (writer 4 nil))
	    (8 (writer 8 t))
	    (16 (writer 16 t))
	    (32 (writer 32 t))
	    (64 (writer 64 t))))))
      (t
       (ecase type
	 (bit (writer 1 nil))
	 (base-char (error "Should be handled by string store functions"))
	 (character (error "Should be handled by string store functions"))
	 (fixnum (writer 64 t set-signed-sap-ref-64 (simple-array fixnum (*))))
	 (single-float (writer 32 nil set-sap-ref-single (simple-array single-float (*))))
	 (double-float (writer 64 nil set-sap-ref-double (simple-array double-float (*)))))))))

(defun store-simple-specialized-vector (sv storage &optional (tag t))
  (declare (optimize (speed 3) (safety 1)) (type (simple-array * (*)) sv))
  (when storage
    (when tag
      (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
	(set-sap-ref-8 sap offset +simple-specialized-vector-code+)))
    (let ((sv-length (length sv)))
      (store-tagged-unsigned-fixnum/interior sv-length storage)
      (multiple-value-bind (bytes-to-write encoded-element-type)
	  (specialized-array-element-size/bits sv)
	#-sbcl(declare (ignorable bytes-to-write))
	#+debug-cbs
	(format t "~&SV: Writing a ~A (~A bytes encoded element-type ~A)~%"
			    (type-of sv) bytes-to-write encoded-element-type)
	(storage-write-byte storage encoded-element-type)
	#-sbcl
	(store-elts storage sv encoded-element-type)
	#+sbcl
	(sb-sys:with-pinned-objects (sv)
	  (write-sap-data-to-storage
	   (sb-sys:vector-sap sv) bytes-to-write storage))))))

(defmacro with-read-storage ((storage &key offset sap reserve-bytes) &body body)
  (let ((original-offset (gensym)))
    `(progn
       (ensure-enough-data ,storage ,reserve-bytes)
       (let* ((,offset (read-storage-offset ,storage))
	      (,original-offset ,offset)
	      (,sap (read-storage-sap ,storage)))
	 (multiple-value-prog1
	     ,@body
	   (setf (read-storage-offset ,storage) (+ ,original-offset ,reserve-bytes)))))))

(defun restore-simple-specialized-vector (storage)
  (declare (optimize (speed 3) (safety 1)))
  (let ((num-elts (restore-tagged-unsigned-fixnum/interior storage)))
    (let* ((encoded-element-info (restore-ub8 storage)))
      (multiple-value-bind (sv num-bytes)
	  (make-simple-array-from-encoded-element-type storage encoded-element-info num-elts)
	#-sbcl (declare (ignorable num-bytes))
	#+debug-cbs
	(format t "~&SV: ~A (~A bytes from ~A elts ~A encoded element-type)~%"
			    (type-of sv) num-bytes num-elts encoded-element-info)
	#-sbcl
	(multiple-value-bind (type bits-per-elt)
	    (encoded-element-type-to-type/packing encoded-element-info)
	  (cond
	    ((consp type)
	     (ecase (car type)
	       (unsigned-byte
		(ecase bits-per-elt
		  (1 (reader 1 nil))
		  (2 (reader 2 nil)) ;; supported on lispworks/sbcl
		  (4 (reader 4 nil)) ;; supported on lispworks/sbcl
		  (8 (reader 8 nil))
		  (16 (reader 16 nil))
		  (32 (reader 32 nil))
		  (64 (reader 64 nil))))
	       (signed-byte
		(ecase bits-per-elt
		  (8 (reader 8 t))
		  (16 (reader 16 t))
		  (32 (reader 32 t))
		  (64 (reader 64 t))))))
	    (t
	     (ecase type
	       (bit (reader 1 nil))
	       (single-float (reader 32 nil sap-ref-single (simple-array single-float (*))))
	       (double-float (reader 64 nil sap-ref-double (simple-array double-float (*))))
	       (fixnum (reader 64 t signed-sap-ref-64 (simple-array fixnum (*))))))))
        #+sbcl
	(with-pinned-objects (sv)
	  (let ((target-sap (vector-sap sv)))
	    (chunked/read
	     storage num-bytes
	     (lambda (source-sap source-sap-offset data-start/bytes data-end-bytes)
	       (copy-sap target-sap data-start/bytes source-sap source-sap-offset (the fixnum (- data-end-bytes data-start/bytes)))))))
        sv))))

;; for ccl ccl::array-data-and-offset would be fine... it's been a stable interface
;; forever.
#-sbcl
(defun store-simple-specialized-array (sa storage)
  (declare (ignorable sa storage))
  (error "write me"))
#+sbcl
(defun store-simple-specialized-array (sa storage)
  (declare (optimize speed safety)
	   (type (simple-array * *) sa))
  (when storage
    (storage-write-byte storage +simple-specialized-array-code+)
    (let* ((array-dimensions (array-dimensions sa))
	   (num-elts (array-total-size sa)))
      (storage-write-byte storage (length array-dimensions))
      (dolist (a array-dimensions)
	(store-tagged-unsigned-fixnum (the fixnum a) storage))
      (multiple-value-bind (bytes-to-write encoded-element-type)
	  (specialized-array-element-size/bits sa num-elts)
	(storage-write-byte storage encoded-element-type)
	#+debug-cbs (format t "~&SA: Writing a ~A (~A bytes encoded element-type ~A)~%"
	 		    (type-of sa) bytes-to-write encoded-element-type)
	(sb-kernel:with-array-data ((backing-array sa) (start) (end))
          (assert (= end num-elts))
	  (assert (zerop start))
	  (sb-sys:with-pinned-objects (backing-array)
	    (let ((source-sap (vector-sap backing-array)))
	    (chunked/write
	     storage bytes-to-write
	     (lambda (storage-sap storage-sap-offset data-start/bytes data-end-bytes)
	       (copy-sap storage-sap storage-sap-offset source-sap data-start/bytes (- data-end-bytes data-start/bytes)))))))
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
	(make-simple-array-from-encoded-element-type
	 storage encoded-element-info (reduce #'* array-dimensions) array-dimensions)
      #+debug-cbs (format t "~&SA: ~A (~A bytes from ~A dims ~A encoded element-type)~%"
	                  (type-of sa) num-bytes array-dimensions encoded-element-info)
      	(with-pinned-objects (sa)
	  (let ((target-sap (array-sap sa)))
	    (chunked/read
	     storage num-bytes
	     (lambda (source-sap source-sap-offset data-start/bytes data-end-bytes)
	       (copy-sap target-sap data-start/bytes source-sap source-sap-offset (- data-end-bytes data-start/bytes))))))
      sa)))

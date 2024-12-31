(in-package :cl-binary-store)

(declaim (inline make-read-storage read-storage-offset read-storage-max read-storage-sap
		 read-storage-flusher read-storage-store read-storage-size
		 read-storage-underlying-stream))

(defstruct read-storage
  "A static memory buffer (SAP) with an OFFSET, which is the offset in bytes of the
 first valid piece of unread data in SAP.  Then MAX which is the end of valid data
 within the buffer.  Then SIZE is the size in bytes of the SAP (used to inform chunked
 copying).  FLUSHER is a (lambda (storage)) which when called will try to fill the buffer
 with new data.  It will return the number of available bytes and may modify offset and
 max.

 We also have a STORE, which *may* if it exists be a
 (simple-array (unsigned-byte 8) (SIZE)) which the static memory
 buffer is based on (used only to speed up utf8 encoding).
 UNDERLYING-STREAM which will be a stream if we are just a buffer in
 front of a stream (unused)."
  (offset 0 :type fixnum)
  (max 0 :type fixnum)
  (sap nil #+sbcl :type #+sbcl sb-alien::system-area-pointer)
  (size 0 :type fixnum)
  (flusher nil :type function) ;; reads more data (lambda (storage))
  (store nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (underlying-stream nil :type (or null stream)))
  

(declaim (inline make-write-storage write-storage-offset write-storage-max write-storage-sap
		 write-storage-flusher write-storage-store write-storage-size
		 write-storage-underlying-stream))
(defstruct write-storage
  "A static memory buffer (SAP) with an OFFSET, which is the offset in bytes where you should
 write new data to.  MAX is the size of the SAP buffer.  STORE, if it exists, is an
 (simple-array (unsigned-byte 8) (MAX)) which is the vector on which the SAP is based.
 FLUSHER is a function (lambda (write-storage)) which flushes data out of the buffer and returns the
 new OFFSET (and updates OFFSET).  UNDERLYING-STREAM, if exists, is the stream which is is used by
 the flusher."
  (offset 0 :type fixnum) ;; index of the next valid location to write data to
  (max 0 :type fixnum) ;; size of SAP in bytes
  (sap nil #+sbcl :type #+sbcl sb-alien::system-area-pointer)
  (flusher nil :type function) ;; writes out data from sap/store and returns new storage-offset
  (store nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (underlying-stream nil :type (or null stream)))

(declaim (inline storage-write-byte storage-write-byte! storage-write-ub16! storage-write-ub32!
		 storage-write-ub64! storage-write-sb64! storage-read-sb64!))

(defun storage-write-byte! (storage ub8 &key (sap (write-storage-sap storage))
					   (offset nil offset-provided-p))
  "store will be a sap on sbcl or just a ub8 array otherwise.
 If you pass in offset, then you are responsible for incrementing it."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((offset (or offset (write-storage-offset storage))))
    (setf (sap-ref-8 sap offset) ub8)
    (unless offset-provided-p (setf (write-storage-offset storage) (truly-the fixnum (+ 1 offset))))))

(defun storage-write-ub16! (storage ub16 &key (sap (write-storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (write-storage-offset storage))))
    (setf (sap-ref-16 sap offset) ub16)
    (unless offset-provided-p (setf (write-storage-offset storage) (truly-the fixnum (+ 2 offset))))))

(defun storage-write-ub32! (storage ub32 &key (sap (write-storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (write-storage-offset storage))))
    (setf (sap-ref-32 sap offset) ub32)
    (unless offset-provided-p (setf (write-storage-offset storage) (truly-the fixnum (+ 4 offset))))))

(defun storage-write-ub64! (storage ub64 &key (sap (write-storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (write-storage-offset storage))))
    (setf (sap-ref-64 sap offset) ub64)
    (unless offset-provided-p (setf (write-storage-offset storage) (truly-the fixnum (+ 8 offset))))))

(defun storage-write-sb64! (storage sb64 &key (sap (write-storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (write-storage-offset storage))))
    (setf (signed-sap-ref-64 sap offset) sb64)
    (unless offset-provided-p (setf (write-storage-offset storage) (truly-the fixnum (+ 8 offset))))))

(declaim (inline storage-read-sb64))
(defun storage-read-sb64 (storage)
  (declare (optimize (speed 3) (safety 0)))
  (ensure-enough-data storage 8)
  (let ((offset (read-storage-offset storage)))
    (prog1
	(signed-sap-ref-64 (read-storage-sap storage) offset)
      (setf (read-storage-offset storage) (truly-the fixnum (+ offset 8))))))

(declaim (inline storage-write-byte))
(defun storage-write-byte (storage byte)
  (ensure-enough-room-to-write storage 1)
  (storage-write-byte! storage byte))

(defmacro with-write-storage ((storage &key offset reserve-bytes sap)
			      &body body)
  "Skips the body if storage does not exist, like during the reference scanning phase"
  (assert (atom storage))
  (let ((original-offset (gensym))
	(reserve-bytes-sym (when reserve-bytes (gensym "RESERVE"))))
    `(when storage
       (let* (,@(when reserve-bytes `((,reserve-bytes-sym ,reserve-bytes)))
	      (,original-offset ,(if reserve-bytes
				    `(ensure-enough-room-to-write ,storage ,reserve-bytes-sym)
				    `(write-storage-offset ,storage)))
	      ,@(when offset `((,offset ,original-offset)))
	      ,@(when sap
		  `((,sap (write-storage-sap ,storage)))))
	 (declare (ignorable ,original-offset))
	 (progn ,@body
		,(when reserve-bytes
		   `(setf (write-storage-offset ,storage) (truly-the fixnum (+ ,original-offset ,reserve-bytes-sym)))))))))

(defmethod print-object ((s write-storage) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "WRITE-STORAGE (FROM ~A), OFFSET: ~A MAX: ~A SAP: ~A"
	    (cond
	      ((write-storage-underlying-stream s) "STREAM")
	      ((write-storage-store s) "UB8 VECTOR")
	      (t "RAW SAP BUFFER"))
	    (write-storage-offset s) (write-storage-max s) (write-storage-sap s))))

(defmethod print-object ((s read-storage) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "READ-STORAGE (FROM ~A) OFFSET: ~A MAX: ~A SAP: ~A"
	    (cond
	      ((read-storage-underlying-stream s) "STREAM")
	      ((read-storage-store s) "UB8 VECTOR")
	      (t "RAW SAP BUFFER"))
	    (read-storage-offset s) (read-storage-max s)
            (read-storage-sap s))))

(defmacro with-tracking-rates ((&optional (leader-default "Read ")) &body body)
  "Provides a function (update bytes) which should be called when new
 bytes are read or written.  ONLY ACTIVE if #+info-cbs, so do not call unless
 #+info-cbs."
  (declare (ignorable leader-default))
  `(let* (#+info-cbs(total-bytes 0)
	  #+info-cbs(start-time (get-universal-time))
	  #+info-cbs(last-time start-time))
     (labels
	 (#+info-cbs(print-update (now &optional (leader ,leader-default leader-provided-p))
            (when (or leader-provided-p (> now (+ 10 last-time)))
              (setf last-time now)
	      (unless (= last-time start-time)
                (format t "~A~,2f MB in ~A seconds (~,2f MB/sec)~%"
                        leader
                        (/ total-bytes 1d6) (- last-time start-time)
			(/ total-bytes 1d6 (- last-time start-time))))))
	  #+info-cbs(update (bytes &optional eof)
	    (let ((now (get-universal-time)))
	      (incf total-bytes bytes)
	      (if eof (print-update now "Finished ") (print-update now)))))
       ,@body)))

(defun make-read-into-storage/stream (stream)
  (declare (optimize speed safety))
  (with-tracking-rates ("Read ")
    (lambda (storage)
      (let ((seq (read-storage-store storage)))
        #+dribble-cbs(format t "We currently have ~A..~A valid data (~A bytes)~%"
	                   (read-storage-offset storage) (read-storage-max storage)
	                   (- (read-storage-max storage) (read-storage-offset storage)))
        (let ((new-bytes-end-at (read-sequence seq stream :start (read-storage-max storage))))
	  #+info-cbs (update (- new-bytes-end-at (read-storage-max storage))
			     (= new-bytes-end-at (read-storage-offset storage)))
	  (setf (read-storage-max storage) new-bytes-end-at)
	  (- new-bytes-end-at (read-storage-offset storage)))))))

(defun make-write-into-storage/stream (stream)
  (declare (optimize speed safety))
  (with-tracking-rates ("Write ")
    (lambda (storage)
      (declare (optimize speed safety))
      (let ((seq (write-storage-store storage)))
	#+debug-cbs (format t "Writing bytes ~A..~A out to stream~%" 0 (storage-offset storage))
	(write-sequence seq stream :end (write-storage-offset storage))
	#+info-cbs (update (write-storage-offset storage))
	(setf (write-storage-offset storage) 0)))))

(defun make-write-into-adjustable-ub8-vector (vector)
  (assert (adjustable-array-p vector))
  (lambda (storage)
    (let* ((num-bytes (write-storage-offset storage))
	   (bytes-available (- (array-total-size vector) (fill-pointer vector))))
      (unless (>= bytes-available num-bytes)
	(setf vector
	      (adjust-array vector
			    (let ((current-size (array-total-size vector)))
			      (max (* 2 current-size)
				   (+ current-size (- num-bytes bytes-available)))))))
      (let ((start (fill-pointer vector)))
	(incf (fill-pointer vector) (write-storage-offset storage))
	(replace vector (write-storage-store storage)
		 :start1 start
		 :end1 (+ start num-bytes)
		 :start2 0
		 :end2 (write-storage-offset storage)))
      (setf (write-storage-offset storage) 0))))

(defmacro with-storage/read ((storage &key stream (buffer-size 8192)
					sap flusher store max size) &body body)
  "Used to create a read-storage from user provided stream or sap or stream"
  ;; If you pass in store, it must be a static-vector and pinned
  (cond
    (sap ;; no need to allocate a store
     `(let* ((,storage (make-read-storage
			:flusher ,flusher
			:store ,store
			:max ,max
			:sap ,sap
			:underlying-stream ,stream
			:size ,size)))
	(declare (dynamic-extent ,storage))
	,@body))
    (store ;; must be a static-vector and pinned!
     (let ((storesym (gensym))
	   (sapsym (gensym)))
       `(let* ((,storesym ,store))
	 (with-pinned-objects (,storesym)
	   (let* ((,sapsym (static-vectors:static-vector-pointer ,storesym))
		  (,storage (make-read-storage
			     :flusher ,flusher
			     :store ,storesym
			     :max ,(or max `(length ,storesym))
			     :sap ,sapsym
			     :underlying-stream ,stream
			     :size ,(or size `(length ,storesym)))))
	     (declare (dynamic-extent ,storage))
	     ,@body)))))
    (t ;; create a static vector buffer
      (let ((vector (gensym)))
	`(static-vectors:with-static-vector
	     (,vector ,buffer-size :element-type '(unsigned-byte 8))
	   (let ((,storage (make-read-storage
			    :flusher ,flusher
			    :store ,vector
			    :max ,(or max `(length ,vector))
			    :sap ,(or sap `(static-vectors:static-vector-pointer ,vector))
			    :underlying-stream ,stream
                            :size ,(or size `(length ,vector)))))
	     (declare (dynamic-extent ,storage))
	     ,@body))))))

(defmacro with-storage/write ((storage &key stream (buffer-size 8192)
					 sap flusher store max
					 (offset 0)) &body body)
  "Used to create a write-storage from user provided sap, store, or stream"
  ;; If you pass in store, it better be a static-vector (or otherwise on
  ;; sbcl where we can
  (cond
    (sap ;; no need to allocate a store
     `(let* ((,storage (make-write-storage
			:flusher ,flusher
			:store ,store
			:offset ,offset
			:max ,max
			:sap ,sap
			:underlying-stream ,stream)))
	(declare (dynamic-extent ,storage))
	,@body))
    (store
     (let ((storesym (gensym))
	   (sapsym (gensym)))
       `(let* ((,storesym ,store))
	  (with-pinned-objects (,store)
	    (let* ((,sapsym (static-vectors:static-vector-pointer ,storesym))
		   (,storage (make-write-storage
			      :flusher ,flusher
			      :store ,storesym
			      :offset ,offset
			      :max ,(or max `(length ,storesym))
			      :sap ,sapsym
			      :underlying-stream ,stream)))
	      (declare (dynamic-extent ,storage))
	      ,@body)))))
    (t	
      (let ((vector (gensym)))
	`(static-vectors:with-static-vector
	     (,vector ,buffer-size :element-type '(unsigned-byte 8))
	   (let ((,storage (make-write-storage
			    :flusher ,flusher
			    :store ,vector
			    :offset ,offset
			    :max ,(or max `(length ,vector))
			    :sap ,(or sap `(static-vectors:static-vector-pointer ,vector))
			    :underlying-stream ,stream)))
	     (declare (dynamic-extent ,storage))
	     ,@body))))))
		
(defun shift-data-to-beginning (read-storage)
  "Move the data in seq to the beginning and update storage-offset and storage-max.
 Returns the index where new data can be written (read-storage-max storage)"
  (let ((store (read-storage-store read-storage))
	(offset (read-storage-offset read-storage))
	(max (read-storage-max read-storage)))
    (replace store store :start1 0 :start2 offset :end2 max) ;; move data to beginning of array
    (setf (read-storage-offset read-storage) 0)
    (setf (read-storage-max read-storage) (- max offset))))

(defun maybe-shift-data-to-beginning-of-read-storage (read-storage bytes)
  "If all we have is a sap the store is a length 0 vector so this
 fails gracefully"
  (declare (optimize speed safety) (type fixnum bytes))
  (let ((vector-length (read-storage-size read-storage))
	(valid-data-ends-at (read-storage-max read-storage)))
    (when (and (> bytes (the fixnum (- vector-length valid-data-ends-at))) ;; we don't have room
               (<= bytes vector-length))
      #+debug-cbs(format t "Shifting data to beginning~%")
      (shift-data-to-beginning read-storage)
      #+debug-cbs(format t "Now storage offset is ~A and storage max is ~A~%"
			 (read-storage-offset read-storage)
			 (read-storage-max read-storage)))))

(define-condition out-of-data (simple-error)
  ()
  (:documentation "Ran out of data while expecting more while reading /deserializing"))

(defun refill-read-storage (storage bytes return-nil-on-eof)
  (declare #+debug-cbs (optimize (debug 3)) #-debug-cbs (optimize speed safety)
           (type fixnum bytes))
  #+dribble-cbs (format t "Asked to read ~A bytes from storage (return-nil-on-eof ~A)~%"
		      bytes return-nil-on-eof)
  (maybe-shift-data-to-beginning-of-read-storage storage bytes)
  (let ((num-bytes-available (the fixnum (funcall (read-storage-flusher storage) storage))))
    (if (< num-bytes-available bytes)
        (if return-nil-on-eof
	    nil
            (progn
	      #+dribble-cbs (format t "Valid data is from ~A to ~A (~A bytes, wanted ~A)~%"
				  (read-storage-offset storage) (read-storage-max storage)
				  (- (read-storage-max storage) (read-storage-offset storage))
				  bytes)
	      (error 'out-of-data :format-control "Out of data")))
        t)))

(declaim (#-debug-cbs inline #+debug-cbs notinline ensure-enough-data))
(defun ensure-enough-data (read-storage bytes &optional (return-nil-on-eof nil))
  "For RESTORE operation.
 Ensure that we have at least BYTES of data in STORAGE.  May signal `out-of-data'
 unless return-nil-on-eof is t.  Do not ask for more than (storage-size storage),
 which is guaranteed to be >8192 bytes."
  (declare #-debug-cbs (optimize (speed 3) (safety 0) (debug 0))
           (type (and fixnum (integer 0)) bytes))
  (or (<= #+sbcl (sb-ext:truly-the fixnum (+ (read-storage-offset read-storage) bytes))
	  #-sbcl (the fixnum (+ (read-storage-offset read-storage) bytes))
	  (read-storage-max read-storage))
      (refill-read-storage read-storage bytes return-nil-on-eof)))

(declaim (notinline flush-write-storage))
(declaim (ftype (function (write-storage &optional fixnum) (values fixnum &optional))
		flush-write-storage))

(define-condition out-of-space (error)
  ((current-offset :initarg :current-offset :reader out-of-space-current-offset)
   (wanted-bytes :initarg :wanted-bytes :reader out-of-space-wanted-bytes))
  (:documentation "Ran out of space while writing data"))

(defun flush-write-storage (storage &optional (bytes 0))
  "Make sure everything is written out of storage to whatever backing store
 there is, and assert there is room to write a further number of BYTES returns
 the current (write-storage-offset write-storage) after flushing.  Will signal an
 error if not enough room available"
  (let ((offset (funcall (write-storage-flusher storage) storage)))
    (when (> bytes (- (write-storage-max storage) offset))
      ;; In the case where we are writing to a raw sap, we can restart this
      ;; gracefully, not so for the case where we need to keep the object
      ;; pinned.  So we only throw this in the case of zero length storage
      ;; which is the signal we are storing to a sap.
      (if (= (length (write-storage-store storage)) 0)
          (restart-case
              (error 'out-of-space :current-offset (write-storage-offset storage))
            (replace-storage (sap sap-size sap-offset)
              :report "Replace storage (non interactive only!)"
              (setf (write-storage-sap storage) sap)
              (setf (write-storage-max storage) sap-size)
              (setf (write-storage-offset storage) sap-offset)))
          (error 'out-of-space :current-offset (write-storage-offset storage))))
    offset))

(declaim (ftype (function (write-storage fixnum) (values fixnum &optional))
                ensure-enough-room-to-write))
(declaim (inline ensure-enough-room-to-write))
(defun ensure-enough-room-to-write (write-storage bytes)
  "Ensure that we have room to write BYTES to STORAGE.  Returns storage offset."
  (declare (optimize (speed 3) (safety 0)) (type fixnum bytes))
  (let ((offset (write-storage-offset write-storage)))
    (if (< #+sbcl (sb-ext:truly-the fixnum (+ offset bytes))
	   #-sbcl (the fixnum (+ offset bytes))
	   (write-storage-max write-storage))
	offset
	(flush-write-storage write-storage bytes))))


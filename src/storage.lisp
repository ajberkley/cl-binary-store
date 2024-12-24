(in-package :cl-store-faster)

(declaim (inline make-storage storage-offset storage-max storage-store storage-sap
		 storage-flusher storage-underlying-stream))

(defstruct storage
  "For read or write"
  (offset 0 :type fixnum) ;; index of the next valid location to write data to or read data from
  (max 0 :type fixnum) ;; end of valid data if read, length of storage array if write
  (store nil :type (simple-array (unsigned-byte 8) (*)))
  (sap nil :type #+sbcl sb-alien::system-area-pointer #-sbcl (simple-array (unsigned-byte 8) (*)))
  (flusher nil :type function) ;; on read return number of available bytes, on write storage-offset
  (underlying-stream nil :type (or null stream))) ;; if exists... I have found no use for this yet

(declaim (inline sap-ref-8 (setf sap-ref-8) sap-ref-16 (setf sap-ref-16)
		 sap-ref-32 (setf sap-ref-32) sap-ref-64 (setf sap-ref-64)
		 signed-sap-ref-64 (setf signed-sap-ref-64)))

(defun (setf sap-ref-8) (ub8 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-8 sap offset) ub8)
  #-sbcl (setf (aref sap offset) ub8))

(defun (setf sap-ref-16) (ub16 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-16 sap offset) ub16)
  #-sbcl (progn (setf (aref store (+ 0 offset)) (logand ub16 #xFF))
		(setf (aref store (+ 1 offset)) (ash ub16 -8))))

(defun (setf sap-ref-32) (ub32 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-32 sap offset) ub32)
  #-sbcl (progn (setf (aref store (+ 0 offset)) (logand ub32 #xFF))
		(setf (aref store (+ 1 offset)) (logand (ash ub32 -8) #xFF))
		(setf (aref store (+ 2 offset)) (logand (ash ub32 -16) #xFF))
		(setf (aref store (+ 3 offset)) (ash ub32 -24))))

(defun (setf sap-ref-64) (ub64 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-64 sap offset) ub64)
  #-sbcl (progn (setf (aref store (+ 0 offset)) (logand ub32 #xFF))
		(setf (aref store (+ 1 offset)) (logand (ash ub32 -8) #xFF))
		(setf (aref store (+ 2 offset)) (logand (ash ub32 -16) #xFF))
		(setf (aref store (+ 3 offset)) (logand (ash ub32 -24) #xFF))
		(setf (aref store (+ 4 offset)) (logand (ash ub32 -32) #xFF))
		(setf (aref store (+ 5 offset)) (logand (ash ub32 -40) #xFF))
		(setf (aref store (+ 6 offset)) (logand (ash ub32 -48) #xFF))
		(setf (aref store (+ 7 offset)) (ash ub32 -56))))

(defun (setf signed-sap-ref-64) (sb64 sap offset)
  #+sbcl (setf (sb-sys:signed-sap-ref-64 sap offset) sb64)
  #-sbcl (error "impl me"))

(declaim (inline sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-64 signed-sap-ref-64))
(defun sap-ref-8 (sap offset)
  #+sbcl (sb-sys:sap-ref-8 sap offset)
  #-sbcl (aref sap offset))

(defun sap-ref-16 (sap offset)
  #+sbcl (sb-sys:sap-ref-16 sap offset)
  #-sbcl (+      (aref sap (+ 0 offset))
	    (ash (aref sap (+ 1 offset)) 8)))

(defun sap-ref-32 (sap offset)
  #+sbcl (sb-sys:sap-ref-32 sap offset)
  #-sbcl (+      (aref sap (+ 0 offset))
	    (ash (aref sap (+ 1 offset)) 8)
	    (ash (aref sap (+ 2 offset)) 16)
	    (ash (aref sap (+ 3 offset)) 24)))

(defun sap-ref-64 (sap offset)
  #+sbcl (sb-sys:sap-ref-64 sap offset)
  #-sbcl (+      (aref sap (+ 0 offset))
	    (ash (aref sap (+ 1 offset)) 8)
	    (ash (aref sap (+ 2 offset)) 16)
	    (ash (aref sap (+ 3 offset)) 24)
	    (ash (aref sap (+ 4 offset)) 32)
	    (ash (aref sap (+ 5 offset)) 40)
	    (ash (aref sap (+ 6 offset)) 48)
	    (ash (aref sap (+ 7 offset)) 56)))

(defun signed-sap-ref-64 (sap offset)
  #+sbcl (sb-sys:signed-sap-ref-64 sap offset)
  #-sbcl (error "imple me"))

(declaim (inline storage-write-byte storage-write-byte! storage-write-ub16! storage-write-ub32!
		 storage-write-ub64! storage-write-sb64! storage-read-sb64!))

(defun storage-write-byte! (storage ub8 &key (sap (storage-sap storage))
					   (offset nil offset-provided-p))
  "store will be a sap on sbcl or just a ub8 array otherwise.
 If you pass in offset, then you are responsible for incrementing it."
  (let ((offset (or offset (storage-offset storage))))
    (setf (sap-ref-8 sap offset) ub8)
    (unless offset-provided-p (setf (storage-offset storage) (+ 1 offset)))))

(defun storage-write-ub16! (storage ub16 &key (sap (storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (storage-offset storage))))
    (setf (sap-ref-16 sap offset) ub16)
    (unless offset-provided-p (setf (storage-offset storage) (+ 2 offset)))))

(defun storage-write-ub32! (storage ub32 &key (sap (storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (storage-offset storage))))
    (setf (sap-ref-32 sap offset) ub32)
    (unless offset-provided-p (setf (storage-offset storage) (+ 4 offset)))))

(defun storage-write-ub64! (storage ub64 &key (sap (storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (storage-offset storage))))
    (setf (sap-ref-64 sap offset) ub64)
    (unless offset-provided-p (setf (storage-offset storage) (+ 8 offset)))))

(defun storage-write-sb64! (storage sb64 &key (sap (storage-sap storage))
					   (offset nil offset-provided-p))
  (let ((offset (or offset (storage-offset storage))))
    (setf (signed-sap-ref-64 sap offset) sb64)
    (unless offset-provided-p (setf (storage-offset storage) (+ 8 offset)))))

(defun storage-read-sb64 (storage)
  (let ((offset (storage-offset storage)))
    (prog1
	(signed-sap-ref-64 (storage-sap storage) offset)
      (setf (storage-offset storage) (+ offset 8)))))

(declaim (inline storage-write-byte))
(defun storage-write-byte (storage byte)
  (ensure-enough-room-to-write storage 1)
  (storage-write-byte! storage byte))

(defmacro with-write-storage ((storage &key offset reserve-bytes over-reserve-bytes sap)
			      &body body)
  "Skips the body if storage does not exist"
  (assert (atom storage)) (assert (not (and reserve-bytes over-reserve-bytes)))
  (let ((original-offset (gensym))
	(reserve-bytes-sym (when reserve-bytes (gensym "RESERVE"))))
    `(when storage
       (let* (,@(when reserve-bytes `((,reserve-bytes-sym ,reserve-bytes)))
	      (,original-offset ,(cond
				   (reserve-bytes
				    `(ensure-enough-room-to-write ,storage ,reserve-bytes-sym))
				   (over-reserve-bytes
				    `(ensure-enough-room-to-write ,storage ,over-reserve-bytes))
				   (t
				    `(storage-offset ,storage))))
	      ,@(when offset `((,offset ,original-offset)))
	      ,@(when sap
		  `((,sap (storage-sap ,storage)))))
	 (declare (ignorable ,original-offset))
	 (progn ,@body
		,(when reserve-bytes
		   `(setf (storage-offset ,storage) (+ ,original-offset ,reserve-bytes-sym))))))))

(defmethod print-object ((s storage) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "OFFSET: ~A MAX: ~A STORE LEN: ~A SAP: ~A"
	    (storage-offset s) (storage-max s) (length (storage-store s))
            (storage-sap s))))

(defun make-read-into-storage/stream (stream)
  (let* (#+info-csf(total-read 0)
         #+info-csf(start-read-time (get-universal-time))
         #+info-csf(last-read-time start-read-time))
    (lambda (storage)
      (let ((seq (storage-store storage)))
        #+debug-csf(format t "We currently have ~A..~A valid data (~A bytes)~%"
	                   (storage-offset storage) (storage-max storage)
	                   (- (storage-max storage) (storage-offset storage)))
        (labels (#+info-csf
		 (print-update (now &optional (leader "Read " leader-provided-p))
                   (when (or leader-provided-p (> now (+ 10 last-read-time)))
                     (setf last-read-time now)
		     (unless (= last-read-time start-read-time)
                       (format t "~A~,2f MB in ~A seconds (~,2f MB/sec)~%"
                               leader
                               (/ total-read 1d6) (- last-read-time start-read-time)
			       (/ total-read 1d6 (- last-read-time start-read-time)))))))
          (let ((new-bytes-end-at (read-sequence seq stream :start (storage-max storage)))
                #+info-csf(now (get-universal-time)))
            #+info-csf(incf total-read (- new-bytes-end-at (storage-max storage)))
	    (setf (storage-max storage) new-bytes-end-at)
            #+info-csf
	    (if (= new-bytes-end-at (storage-offset storage))
                (print-update now "Finished reading ")
                (print-update now))
	    (- new-bytes-end-at (storage-offset storage))))))))

(defun make-write-into-storage/stream (stream)
  (declare (optimize speed safety))
  (lambda (storage)
    (declare (optimize speed safety))
    (let ((seq (storage-store storage)))
      #+debug-csf (format t "Writing bytes ~A..~A out to stream~%" 0 (storage-offset storage))
      (write-sequence seq stream :end (storage-offset storage))
      (setf (storage-offset storage) 0))))

(defun make-write-into-adjustable-ub8-vector (vector)
  (lambda (storage)
    (let* ((num-bytes (storage-offset storage))
	   (bytes-available (- (array-total-size vector) (fill-pointer vector))))
      (unless (>= bytes-available num-bytes)
	(setf vector
	      (adjust-array vector
			    (let ((current-size (array-total-size vector)))
			      (max (* 2 current-size)
				   (+ current-size (- num-bytes bytes-available)))))))
      (let ((start (fill-pointer vector)))
	(incf (fill-pointer vector) (storage-offset storage))
	(replace vector (storage-store storage)
		 :start1 start
		 :end1 (+ start num-bytes)
		 :start2 0
		 :end2 (storage-offset storage)))
      (setf (storage-offset storage) 0))))

(defmacro with-pinned-objects ((&rest objects) &body body)
  #+sbcl
  `(sb-sys:with-pinned-objects ,objects
     ,@body)
  #-sbcl `(progn ,@body))

(defmacro vector-sap (vector)
  "On sbcl, return a SAP referring to the backing store of vector, otherwise the
 vector itself"
  #+sbcl `(sb-sys:vector-sap ,vector)
  #-sbcl vector)

(defmacro array-sap (array)
  "Return a SAP referring to the backing store of array-sap (on sbcl) otherwise the
 1D vector backing-store of the vector."
  #+sbcl
  (let ((g (gensym)))
    `(sb-kernel:with-array-data ((,g ,array) (start) (end))
       (assert (zerop start))
       (with-pinned-objects (,g)
         (vector-sap ,g))))
  #-sbcl
  (error "unimplemented"))

(defmacro with-storage ((storage &key stream (buffer-size 8192)
                                 sap flusher store max) &body body)
  (assert (and (atom storage) (atom stream) (atom buffer-size))) ;; lazy, multiply evaluating
  (let ((vector (gensym)))
    `(let ((,vector
	     ,(if store store `(make-array ,buffer-size :element-type '(unsigned-byte 8)))))
       (with-pinned-objects (,vector)
	 (let ((,storage (make-storage
			  :flusher ,flusher
			  :store ,vector
			  :max ,(if max max `(length ,vector))
			  :sap ,(if sap sap `(vector-sap ,vector))
			  :underlying-stream ,stream)))
	   (declare (dynamic-extent ,storage))
	   ,@body)))))
		
(defun shift-data-to-beginning (read-storage)
  "Move the data in seq to the beginning and update storage-offset and storage-max.
 Returns the index where new data can be written (storage-max storage)"
  (let ((store (storage-store read-storage))
	(offset (storage-offset read-storage))
	(max (storage-max read-storage)))
    (replace store store :start1 0 :start2 offset :end2 max) ;; move data to beginning of array
    (setf (storage-offset read-storage) 0)
    (setf (storage-max read-storage) (- max offset))))

(defun maybe-shift-data-to-beginning-of-read-storage (read-storage bytes)
  "Returns nil on failure.  If all we have is a sap the store is a length 0
 vector so this fails gracefully"
  (let ((vector-length (length (storage-store read-storage)))
	(valid-data-ends-at (storage-max read-storage)))
    (when (and (> bytes (- vector-length valid-data-ends-at))
               (< (+ bytes (- valid-data-ends-at (storage-offset read-storage)))
                  vector-length))
      (shift-data-to-beginning read-storage))))

(define-condition end-of-data (simple-error)
  ())

(defun refill-read-storage (storage bytes return-nil-on-eof)
  (declare (optimize speed safety) (type fixnum bytes))
  #+debug-csf (format t "Asked to read ~A bytes from storage (return-nil-on-eof ~A~%"
		      bytes return-nil-on-eof)
  (maybe-shift-data-to-beginning-of-read-storage storage bytes)
  (let ((num-bytes-available (the fixnum (funcall (storage-flusher storage) storage))))
    (if (< num-bytes-available bytes)
        (if return-nil-on-eof
	    nil
            (progn
	      #+debug-csf (format t "Valid data is from ~A to ~A (~A bytes, wanted ~A)~%"
				  (storage-offset storage) (storage-max storage)
				  (- (storage-max storage) (storage-offset storage))
				  bytes)
	      (error 'end-of-data :format-control "Out of data")))
        t)))

(declaim (inline ensure-enough-data))
(defun ensure-enough-data (storage bytes &optional (return-nil-on-eof nil))
  "For RESTORE operation.
 Ensure that we have at least BYTES of data in STORAGE.  May signal `end-of-data'
 unless return-nil-on-eof is t."
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type fixnum bytes))
  (or (<= (sb-ext:truly-the fixnum (+ (storage-offset storage) bytes)) (storage-max storage))
      (refill-read-storage storage bytes return-nil-on-eof)))

(declaim (notinline flush-write-storage))
(declaim (ftype (function (storage &optional fixnum) (values fixnum &optional))
		flush-write-storage))

(define-condition out-of-space (error)
  ((current-offset :initarg :current-offset :reader out-of-space-current-offset)
   (wanted-bytes :initarg :wanted-bytes :reader out-of-space-wanted-bytes)))

(defun flush-write-storage (storage &optional (bytes 0))
  "Make sure everything is written out of storage to whatever backing store
 there is, and assert there is room to write a further number of BYTES returns
 the current (storage-offset write-storage) after flushing.  Will signal an
 error if not enough room available"
  (let ((offset (funcall (storage-flusher storage) storage)))
    (when (> bytes (- (storage-max storage) offset))
      ;; In the case where we are writing to a raw sap, we can restart this
      ;; gracefully, not so for the case where we need to keep the object
      ;; pinned.  So we only throw this in the case of zero length storage
      ;; which is the signal we are storing to a sap.
      (if (= (length (storage-store storage)) 0)
          (restart-case
              (error 'out-of-space :current-offset (storage-offset storage))
            (replace-storage (sap sap-size sap-offset)
              :report "Replace storage (non interactive only!)"
              (setf (storage-sap storage) sap)
              (setf (storage-max storage) sap-size)
              (setf (storage-offset storage) sap-offset)))
          (error 'out-of-space-to-write :current-offset (storage-offset storage))))
    offset))

(declaim (ftype (function (storage fixnum) (values fixnum &optional))
                ensure-enough-room-to-write))
(declaim (inline ensure-enough-room-to-write))
(defun ensure-enough-room-to-write (storage bytes)
  "Ensure that we have room to write BYTES to STORAGE.  Returns storage offset."
  (declare (optimize (speed 3) (safety 0)) (type fixnum bytes))
  (let ((offset (storage-offset storage)))
    (if (< (sb-ext:truly-the fixnum (+ offset bytes)) (storage-max storage))
	offset
	(flush-write-storage storage bytes))))


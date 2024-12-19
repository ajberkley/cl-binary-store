(in-package :cl-store-faster)

;; We serialize and deserialize from write-storage and read-storage.
;; Nominally we want more than just a stream interface to avoid excess
;; copying where we can.

;; Since FAST-GENERIC-FUNCTIONS doesn't work currently on SBCL, I manually
;; inlined all the access here all the way up to the writers and readers, so
;; we dispatch at the top.  Ugly, but works.

;; TODO: large object write/read without copying
;; TODO: Direct SAP buffer access (for mmap interfaces) (in progress)

(declaim (inline storage-store storage-offset storage-flusher storage-size storage-max
		 %make-buffering-write-storage))

(declaim (inline storage-offset))
(defstruct (storage-base (:constructor %make-storage-base) (:conc-name storage-))
  (offset 0 :type fixnum)) ;; Index of the next valid location to write data

(declaim (inline storage-store&))
(defstruct (buffering-write-storage (:constructor %make-buffering-write-storage)
				    (:conc-name storage-)
				    (:include storage-base))
  (store& (make-array 8192 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  ;; When called with storage, flusher writes-out or reads-in the data in seq and
  ;; updates offset and max and returns the number of valid bytes in storage if reading
  (flusher (constantly nil) :type function))

(declaim (inline storage-sap& storage-sap-size&))
(defstruct (sap-write-storage (:include storage-base) (:conc-name storage-))
  (sap& nil) ;;:type sb-alien::system-area-pointer)
  (sap-size& 0 :type fixnum))

;;(defstruct (raw-stream-write-storage (:include buffering-write-storage)))

(declaim (inline storage-sap))
(defmacro with-storage-sap ((sap storage) &body body)
  (assert (atom storage))
  `(typecase ,storage
     (sap-write-storage
      (let ((sap (storage-sap& ,storage)))
	,@body))
     (buffering-write-storage
      (let ((store (storage-store& ,storage)))
	(sb-sys:with-pinned-objects (store)
	  (let ((,sap (sb-sys:vector-sap store)))
	    ,@body))))
     ;; (raw-stream-write-storage
     ;;  ;; just treat as a buffering-write-storage
     ;;  ;; no reason to elide copying here
     ;;  (error "hm"))
     ))
     
(declaim (inline storage-write-byte storage-write-byte!
		 storage-write-ub16! storage-write-ub32!
		 storage-write-ub64!))

(defun storage-write-byte! (storage byte &optional offset)
  "If you pass in offset, then you are also responsible for incrementing it"
  (typecase storage
    (buffering-write-storage
     (let ((offset (or offset (storage-offset storage))))
       (setf (aref (storage-store& storage) offset) byte)
       (when offset (setf (storage-offset storage) (+ 1 offset)))))
    (sap-write-storage
      (let ((offset (storage-offset storage)))
	(setf (sb-sys:sap-ref-8 (storage-sap& storage) offset) byte)
	(when offset (setf (storage-offset storage) (+ 1 offset)))))))

(defun storage-write-ub16! (storage ub16 &optional offset)
  (typecase storage
    (buffering-write-storage
     (let ((offset (or offset (storage-offset storage)))
	   (array (storage-store& storage)))
       (setf (aref array offset) (logand ub16 255))
       (setf (aref array (incf offset)) (ash ub16 -8))
       (when offset (setf (storage-offset storage) (+ 2 offset)))))
    (sap-write-storage
      (let ((offset (or offset (storage-offset storage))))
	(setf (sb-sys:sap-ref-16 (storage-sap& storage) offset) ub16)
	(when offset (setf (storage-offset storage) (+ 2 offset)))))))

(defun storage-write-ub32! (storage ub32 &optional offset)
  (typecase storage
    (buffering-write-storage
     (let ((offset (or offset (storage-offset storage))))
       (with-storage-sap (sap storage)
	 (setf (sb-sys:sap-ref-32 sap offset) ub32))
       (when offset (setf (storage-offset storage) (+ 4 offset)))))
    (sap-write-storage
     (let ((offset (or offset (storage-offset storage))))
       (setf (sb-sys:sap-ref-32 (storage-sap& storage) offset) ub32)
       (when offset (setf (storage-offset storage) (+ 4 offset)))))))

(defun storage-write-ub64! (storage ub64 &optional offset)
  (typecase storage
    (buffering-write-storage
     (let ((offset (or offset (storage-offset storage))))
       (with-storage-sap (sap storage)
	 (setf (sb-sys:sap-ref-64 sap offset) ub64))
       (when offset (setf (storage-offset storage) (+ 8 offset)))))
    (sap-write-storage
     (let ((offset (or offset (storage-offset storage))))
       (setf (sb-sys:sap-ref-64 (storage-sap& storage) offset) ub64)
       (when offset (setf (storage-offset storage) (+ 8 offset)))))))

(defun storage-write-byte (storage byte)
  (ensure-enough-room-to-write storage 1)
  (storage-write-byte! storage byte))

(defmacro with-write-storage ((storage &optional offset number-of-bytes-to-reserve) &body body)
  "Skips the body if storage does not exist"
  (assert (atom storage))
  `(when storage
     (labels ((body (,storage)
		(let (,@(when offset
			  `((,offset ,(if number-of-bytes-to-reserve
					  `(ensure-enough-room-to-write ,storage ,number-of-bytes-to-reserve)
					  `(storage-offset ,storage))))))
		  ,@body)))
       (etypecase storage
	 (buffering-write-storage
	  (body storage))
	 (sap-write-storage
	  (body storage))))))

(declaim (inline %make-read-storage storage-read-more-func))
(defstruct (read-storage (:constructor %make-read-storage)
			 (:include storage-base)
			 (:conc-name storage-))
  (store (make-array 8192 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (max 0 :type fixnum)
  (read-more-func (lambda (read-storage) (declare (ignore read-storage)) (error "not implemented"))
   :type function))

(defmethod print-object ((rs read-storage) stream)
  (print-unreadable-object (rs stream :type t :identity t)
    (format stream "OFFSET: ~A MAX: ~A STORE: ~A"
	    (storage-offset rs) (storage-max rs) (storage-store rs))))

(declaim (inline storage-size))			       
(defun storage-size (storage)
  (typecase storage
    (buffering-write-storage
     (length (storage-store& storage)))
    (sap-write-storage
     (storage-sap-size& storage))
    (read-storage
     (length (storage-store storage)))))

(defun make-read-into-storage/stream (stream)
  (lambda (storage)
    (shift-data-to-beginning storage)
    (let ((offset (storage-offset storage))
	  (seq (storage-store storage)))
      (setf (storage-max storage) (read-sequence seq stream :start offset)))))

(defun make-write-into-storage/stream (stream)
  (lambda (storage)
    (let ((seq (storage-store& storage)))
      #+debug-csf
      (format t "Writing bytes ~A..~A out to stream~%" 0 (storage-offset storage))
      (write-sequence seq stream :end (storage-offset storage))
      (setf (storage-offset storage) 0))))

(declaim (inline make-input-storage/stream))
(defun make-input-storage/stream (stream)
  (%make-read-storage :read-more-func (make-read-into-storage/stream stream)))

(declaim (inline make-output-storage/stream))
(defun make-output-storage/stream (stream &optional (buffer-size 8192))
  (%make-buffering-write-storage :flusher (make-write-into-storage/stream stream)
		       :store& (make-array buffer-size :element-type '(unsigned-byte 8))))

(defun shift-data-to-beginning (storage)
  "FOR RESTORE
 Move the data in seq to the beginning and update storage-offset and storage-max.
 Returns the index where new data can be written (storage-max storage)"
  (let ((store (storage-store storage))
	(offset (storage-offset storage))
	(max (storage-max storage)))
    (replace store store :start1 0 :start2 offset :end2 max) ;; move data to beginning of array
    (setf (storage-offset storage) 0)
    (setf (storage-max storage) (- max offset))))

(defun maybe-increase-size-of-read-storage (storage bytes)
  (let ((vector-length (storage-size storage))
	(valid-data-ends-at (storage-max storage)))
    (when (> bytes (- vector-length valid-data-ends-at))
      (let* ((valid-data-starts-at (storage-offset storage))
	     (valid-data-bytes (- valid-data-ends-at valid-data-starts-at)))
	(cond
	  ((> bytes (- vector-length valid-data-bytes))
	   (shift-data-to-beginning storage))
	  (t
	   (let ((new (make-array (+ bytes valid-data-bytes) :element-type '(unsigned-byte 8))))
	     (replace new (storage-store storage) :start2 valid-data-starts-at
						  :end2 valid-data-ends-at)
	     (setf (storage-offset storage) 0)
	     (setf (storage-max storage) valid-data-bytes))))))))

(declaim (ftype (function (buffering-write-storage fixnum) (values fixnum &optional))
		flush-then-increase-size-of-storage))

(defun flush-then-increase-size-of-storage (storage bytes)
  "Returns storage offset after action"
  (funcall (storage-flusher storage) storage)
  (if (> bytes (storage-size storage))
      (progn
	(setf (storage-store& storage) (make-array bytes :element-type '(unsigned-byte 8)))
	(setf (storage-offset storage) 0))
      (storage-offset storage)))

(define-condition end-of-data (simple-error)
  ())

(defun refill-read-storage (storage bytes return-nil-on-eof)
  (declare (optimize speed safety) (type fixnum bytes))
  (maybe-increase-size-of-read-storage storage bytes)
  (let ((storage-end (the fixnum (funcall (storage-read-more-func storage) storage))))
    (if (< storage-end bytes)
        (if return-nil-on-eof
	    nil
            (error 'end-of-data :format-control "Out of data"))
        t)))

(declaim (inline ensure-enough-data))
(defun ensure-enough-data (storage bytes &optional (return-nil-on-eof nil))
  "For RESTORE operation.
 Ensure that we have at least BYTES of data in STORAGE.  May signal `end-of-data'
 unless return-nil-on-eof is t."
  (declare (optimize speed safety) (type fixnum bytes))
  (or (<= (the fixnum (+ (storage-offset storage) bytes)) (storage-max storage))
      (refill-read-storage storage bytes return-nil-on-eof)))

(declaim (inline flush-write-storage))
(defun flush-write-storage (storage)
  (etypecase storage
    (buffering-write-storage (funcall (storage-flusher storage) storage))
    (sap-write-storage nil)))

(declaim (inline ensure-enough-room-to-write))
(defun ensure-enough-room-to-write (storage bytes)
  "Ensure that we have room to write BYTES to STORAGE.  Returns storage offset."
  (declare (optimize speed safety) (type fixnum bytes))
  (etypecase storage
    (buffering-write-storage
     (locally (declare (type fixnum bytes))
       (let ((offset (storage-offset storage)))
	 (if (< (the fixnum (+ offset bytes)) (storage-size storage))
	     offset
	     (flush-then-increase-size-of-storage storage bytes)))))
    (sap-write-storage
     (let ((offset (storage-offset storage)))
       (assert (<= (the fixnum (+ offset bytes)) (storage-sap-size& storage)))
       offset))))


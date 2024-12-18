(in-package :cl-store-faster)

;; The interface for serialization is through a STORAGE structure.
;; This is an abstraction for a buffered reader / writer.  We have a
;; buffer STORAGE-STORE which is a (simple-array (unsigned-byte 8)
;; (*)) which we are reading or writing to.  When we want more data or
;; run out of space we call the storage-flusher.

;; We provide a stream interface and a vector interface for users to use.

(declaim (inline storage-store storage-offset storage-flusher storage-size storage-max))

(defstruct (write-storage (:constructor %make-write-storage) (:conc-name storage-))
  (store (make-array 8192 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (offset 0 :type fixnum) ;; index of the next valid input or location for output
  ;; When called with storage, flusher writes-out or reads-in the data in seq and
  ;; updates offset and max and returns the number of valid bytes in storage if reading
  (flusher))

(defstruct (read-storage (:constructor %make-read-storage)
			 (:include write-storage)
			 (:conc-name storage-))
  (max 0 :type fixnum))

(defun storage-size (storage)
  (length (storage-store storage)))

(defun make-read-into-storage/stream (stream)
  (lambda (storage)
    (shift-data-to-beginning storage)
    (let ((offset (storage-offset storage))
	  (seq (storage-store storage)))
      (setf (storage-max storage) (read-sequence seq stream :start offset)))))

(defun make-write-into-storage/stream (stream)
  (lambda (storage)
    (let ((seq (storage-store storage)))
      (write-sequence seq stream :end (storage-offset storage))
      (setf (storage-offset storage) 0))))

(defun make-input-storage/stream (stream)
  (%make-read-storage :flusher (make-read-into-storage/stream stream)))

(defun make-output-storage/stream (stream)
  (%make-write-storage :flusher (make-write-into-storage/stream stream)))

(defun shift-data-to-beginning (storage)
  "Move the data in seq to the beginning and update storage-offset and storage-max.
 Returns the index where new data can be written (storage-max storage)"
  (let ((store (storage-store storage))
	(offset (storage-offset storage))
	(max (storage-max storage)))
    (replace store store :start1 0 :start2 offset :end2 max) ;; move data to beginning of array
    (setf (storage-offset storage) 0)
    (setf (storage-max storage) (- max offset))))

(defun maybe-increase-size-of-storage (storage bytes)
  (when (> bytes (storage-size storage))
    (setf (storage-store storage) (make-array bytes :element-type '(unsigned-byte 8)))))

(define-condition end-of-data (simple-error)
  ())

(defun refill-storage (storage bytes return-nil-on-eof)
  (declare (optimize speed safety) (type fixnum bytes))
  (maybe-increase-size-of-storage storage bytes)
  (let ((storage-end (the fixnum (funcall (storage-flusher storage) storage))))
    (if (< storage-end bytes)
        (if return-nil-on-eof
	    nil
            (error 'end-of-data))
        t)))

(declaim (inline ensure-enough-data))
(defun ensure-enough-data (storage bytes &optional (return-nil-on-eof nil))
  "Ensure that we have at least BYTES of data in STORAGE.  May signal `end-of-data'
 unless return-nil-on-eof is t."
  (declare (optimize speed safety) (type fixnum bytes))
  (or (<= (the fixnum (+ (storage-offset storage) bytes)) (storage-max storage))
      (refill-storage storage bytes return-nil-on-eof)))

(declaim (inline flush-storage))
(defun flush-storage (storage)
  (funcall (storage-flusher storage) storage))

(defun flush-writer (storage bytes)
  (maybe-increase-size-of-storage storage bytes)
  (flush-storage storage))

(declaim (inline ensure-enough-room))
(defun ensure-enough-room (storage bytes)
  "Ensure that we have room to write BYTES to STORAGE"
  (declare (optimize speed safety))
  (or (not storage)
      (locally (declare (type fixnum bytes))
	(or (< (the fixnum (+ (storage-offset storage) bytes)) (storage-size storage))
	    (flush-writer storage bytes)))))

;; User facing

;; STREAMS
(defun store-to-stream (stream &rest elements)
  (declare (dynamic-extent elements))
  (let ((storage (make-output-storage/stream stream)))
    (apply #'store-objects storage elements)
    (flush-storage storage)))

(defun restore-from-stream (stream)
  (restore-objects (make-input-storage/stream stream)))

;; UB8 VECTORS
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

(defun make-output-storage/ub8 ()
  (let ((output-vector (make-array 8 :element-type '(unsigned-byte 8)
				   :adjustable t :fill-pointer 0)))
    (values (%make-write-storage :flusher (make-write-into-adjustable-ub8-vector output-vector))
	    output-vector)))

(defun store-to-vector (&rest elements)
  (declare (dynamic-extent elements))
  "Returns an (array (unsigned-byte 8) (*)) with the data"
  (multiple-value-bind (storage vector)
      (make-output-storage/ub8)
    (apply #'store-objects storage elements)
    (flush-storage storage)
    vector))
    
(defun restore-from-vector (vector)
  (if (typep vector '(simple-array (unsigned-byte 8) (*)))
      (let* ((storage
	       (%make-read-storage :flusher (lambda (storage)
					      (- (storage-max storage) (storage-offset storage)))
				   :store vector
				   :max (length vector))))
	(restore-objects storage))
      (flexi-streams:with-input-from-sequence (str vector)
	(let ((storage (make-input-storage/stream str)))
	  (restore-objects storage)))))

(defun store-to-file (filename &rest elements)
  (with-open-file (str filename :direction :output :if-exists :supersede :if-does-not-exist :create
		       :element-type '(unsigned-byte 8))
    (apply #'store-objects (make-output-storage/stream str) elements)))

(defun restore-from-file (filename)
  (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
    (restore-objects (make-input-storage/stream str))))

;; TODO: SAP VECTORS / MMAPPED MEMORY

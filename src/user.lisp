(in-package :cl-store-faster)

;; User facing

;; STREAMS
(defun store-to-stream (stream &rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  (let ((storage (make-output-storage/stream stream)))
    (declare (dynamic-extent storage))
    (apply #'store-objects/buffering-write-storage storage elements)
    (flush-write-storage storage)
    (values)))

(defun restore-from-stream (stream)
  (declare (optimize speed safety))
  (let ((input-storage (make-input-storage/stream stream)))
    (declare (dynamic-extent input-storage))
    (restore-objects input-storage)))

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
	(replace vector (storage-store& storage)
		 :start1 start
		 :end1 (+ start num-bytes)
		 :start2 0
		 :end2 (storage-offset storage)))
      (setf (storage-offset storage) 0))))

(defun store-to-vector (&rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  "Returns an (array (unsigned-byte 8) (*)) with the data"
    (let* ((output-vector (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0))
	   (write-storage
	     (%make-buffering-write-storage
	      :flusher (make-write-into-adjustable-ub8-vector output-vector))))
      (declare (dynamic-extent write-storage))
      (apply #'store-objects/buffering-write-storage write-storage elements)
      (flush-write-storage write-storage)
      output-vector))

(defun store-to-extant-vector (vector &rest data)
  (let* ((offset 0)
	 (is-simple-octet-array (typep vector '(simple-array (unsigned-byte 8) (*))))
	 (temp-vector
	   (if is-simple-octet-array
	       vector
	       (make-array (min 8192 (length vector)) :element-type '(unsigned-byte 8))))
	 (flusher
	   (if is-simple-octet-array
	       (lambda (storage)
		 (declare (type buffering-write-storage storage))
		 (storage-offset storage))
	       (lambda (storage)
		 (declare (type buffering-write-storage storage))
		 (assert (> (- (length vector) offset)
			    (storage-offset storage))
			 nil
			 'out-of-space-in-fixed-vector
			 :format-control "Out of space in provided output vector")
		 (replace vector (storage-store& storage)
			  :start1 offset :start2 0
			  :end2 (storage-offset storage))
		 (incf offset (storage-offset storage))
		 (setf (storage-offset storage) 0))))
	 (storage (%make-buffering-write-storage :store& temp-vector :flusher flusher)))
    (declare (dynamic-extent temp-vector storage flusher) (type fixnum offset))
    (store-objects/buffering-write-storage storage data)
    offset))

(defun restore-from-vector (vector)
  (declare (optimize speed safety))
  #+debug-csf(format t "Restoring from a vector with ~A bytes in it~%" (length vector))
  (if (typep vector '(simple-array (unsigned-byte 8) (*)))
      (let* ((storage
	       (%make-read-storage
		:read-more-func (lambda (storage)
				  (the fixnum (- (storage-max storage) (storage-offset storage))))
		:store vector
		:max (length vector))))
	(declare (dynamic-extent storage))
	(restore-objects storage))
      (flexi-streams:with-input-from-sequence (str vector)
	(let ((storage (make-input-storage/stream str)))
	  (declare (dynamic-extent storage))
	  (restore-objects storage)))))

(defun store-to-file (filename &rest elements)
  (declare (optimize speed safety))
  (with-open-file (str filename :direction :output
				:if-exists :supersede
				:element-type '(unsigned-byte 8))
    (let ((output-storage (make-output-storage/stream str)))
      (declare (dynamic-extent output-storage))
      (apply #'store-objects/buffering-write-storage output-storage elements))))

(defun restore-from-file (filename)
  (declare (optimize speed safety))
  (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
    (let ((input-storage (make-input-storage/stream str)))
      (declare (dynamic-extent input-storage))
      (restore-objects input-storage))))

;; TODO: SAP VECTORS / MMAPPED MEMORY

(defvar *write-magic-number* nil)

(defun restore (place)
  "(restore #(14 39 37 0 2 72 73 15)) -> (values :hi))
 (store filename (list :hi :bye) :something)
 (restore filename/stream/place) -> (values (list :hi :bye) :something)
 (cl-store-faster:restore-from-vector (cl-store-faster:store nil :hi :bye)) -> (values :hi :bye)

 Note that if you specified *write-magic-number* then a `magic-number' will be the first value
 returned.  It will be asserted against *supported-versions*"
  (etypecase place
    ((or string pathname)
     (restore-from-file place))
    (stream
     (restore-from-stream place))
    (vector
     (restore-from-vector place))))

(define-condition out-of-space-in-fixed-vector (simple-error) ())

(defun store (place &rest data)
  "When place is NIL, returns a (vector (unsigned-byte 8) (*)) with fill-pointer
 ex: (cl-store-faster:store nil :hi) -> #(39 37 0 2 72 73)

 When place is a filename/string/pathname writes data to the respective file.

 When place is a vector writes data to it and returns num used bytes... if vector is
 adjustable, it may be adjusted.  Otherwise we error if we run out of space.

 When place is a system-area-pointer writes into it and returns num used bytes (NOT IMPLEMENTED)

 Note that if you provide more than one data object, they will come back from RESTORE as
 multiple values.

 ex: (restore (store filename :hi :bye)) -> (values :hi :bye).

 If *write-magic-number* we write out *write-version* at the beginning of the stream and
 it will then be validated on restore."
  (declare (dynamic-extent data) (optimize speed safety))
  (let* ((magic-number (make-magic-number :number *write-version*))
	 (data* (if *write-magic-number* (cons magic-number data) data)))
    (declare (dynamic-extent magic-number data*))
    (etypecase place
      ((or string pathname)
       (apply #'store-to-file place data*)
       place)
      (stream
       (apply #'store-to-stream place data*)
       place)
      (vector
       (cond
	 ((adjustable-array-p place)
	  (let ((storage (%make-buffering-write-storage
			  :flusher (make-write-into-adjustable-ub8-vector place))))
	    (declare (dynamic-extent storage))
	    (apply #'store-objects/buffering-write-storage storage data*)))
	 (t
	  (apply #'store-to-extant-vector place data*))))
      (null
       (apply #'store-to-vector data*)))))

    

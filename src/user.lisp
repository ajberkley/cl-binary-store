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
    (let* ((output-vector (make-array 8 :element-type '(unsigned-byte 8)
					:adjustable t :fill-pointer 0))
	   (write-storage
	     (%make-buffering-write-storage
	      :flusher (make-write-into-adjustable-ub8-vector output-vector))))
      (declare (dynamic-extent write-storage))
      (apply #'store-objects/buffering-write-storage write-storage elements)
      (flush-write-storage write-storage)
      output-vector))
    
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

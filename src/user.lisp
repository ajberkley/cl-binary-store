(in-package :cl-store-faster)

;; User facing interface is generally just `restore' and `store' except
;; for SAPs which would be `restore-from-sap' `store-to-sap' and `replace-store-sap-buffer'

;;; STREAMS

(defun store-to-stream (stream &rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  (with-storage (storage :flusher (make-write-into-storage/stream stream))
    (apply #'store-objects storage elements)
    (flush-write-storage storage)
    (values)))

(defun restore-from-stream (stream)
  (declare (optimize speed safety))
  (with-storage (storage :flusher (make-read-into-storage/stream stream))
    (restore-objects storage)))

;;; UB8 VECTORS

(defun store-to-vector (&rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  "Returns an (array (unsigned-byte 8) (*)) with the data"
  (let* ((output-vector (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (with-storage (storage :flusher (make-write-into-adjustable-ub8-vector output-vector))
      (apply #'store-objects storage elements)
      (flush-write-storage storage)
      output-vector)))

(define-condition out-of-space-in-fixed-vector (simple-error) ())

(defun store-to-extant-vector (vector &rest data)
  (declare (optimize speed safety))
  (let* ((offset 0)
	 (is-simple-octet-array (typep vector '(simple-array (unsigned-byte 8) (*))))
	 (is-adjustable (adjustable-array-p vector))
	 (temp-vector
	   (if is-simple-octet-array
	       vector
	       (make-array (min 8192 (length vector)) :element-type '(unsigned-byte 8))))
	 (vector-len (length temp-vector))
	 (flusher
	   (cond
	     (is-simple-octet-array
	      (lambda (storage) (storage-offset storage)))
	     (is-adjustable
	      (make-write-into-adjustable-ub8-vector vector))
	     (t
	       (lambda (storage)
		 (assert (> (- vector-len offset)
			    (storage-offset storage))
			 nil
			 'out-of-space-in-fixed-vector
			 :format-control "Out of space in provided output vector")
		 (replace vector (storage-store storage)
			  :start1 offset :start2 0
			  :end2 (storage-offset storage))
		 (incf offset (storage-offset storage))
		 (setf (storage-offset storage) 0))))))
    (declare (dynamic-extent temp-vector flusher) (type fixnum vector-len offset))
    (with-pinned-objects (temp-vector)
      (with-storage (storage :flusher flusher :store temp-vector)
	(declare (type fixnum offset))
	(apply #'store-objects storage data)
	(if is-simple-octet-array
	    (storage-offset storage)
	    offset)))))

(defun restore-from-vector (vector)
  (declare (optimize speed safety))
  #+debug-csf(format t "Restoring from a vector with ~A bytes in it~%" (length vector))
  (if (typep vector '(simple-array (unsigned-byte 8) (*)))
      (with-storage (storage
		     :flusher
		     (lambda (storage)
		       (the fixnum (- (storage-max storage)
				      (storage-offset storage))))
		     :store vector :max (length vector))
	(restore-objects storage))
      (flexi-streams:with-input-from-sequence (str vector)
	(with-storage (storage :flusher (make-read-into-storage/stream str) :max 0)
	  (restore-objects storage)))))

;;; SAP vectors

(defun replace-store-sap-buffer (sap &key (sap-size 0) (sap-offset 0))
  (invoke-restart 'replace-storage sap sap-size sap-offset))

(defun store-to-sap (sap size &rest data)
  "This may error with 'out-of-space (which contains
 out-of-space-current-offset and out-of-space-wanted-bytes).  The
 out-of-space-current-offset is the amount of data that has been
 written so far.  out-of-space-wanted-bytes is not useful unless you
 happen to be writing very large stuff as it will likely be a small
 number representing the immediate need.  Best to allocate a big chunk
 and when this finally returns we return the amount of data we wrote
 to the chunk.  Call (replace-store-sap-buffer sap sap-offset) in a
 handler-bind to do this updating.  See test test-sap-write/read for
 an example."
  (let ((store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage (storage :flusher (lambda (storage) (storage-offset storage))
                           :sap sap :store store :max size :buffer-size nil)
      (apply #'store-objects storage data)
      (storage-offset storage))))

(defun restore-from-sap (sap size)
  (declare (optimize speed safety))
  (let ((store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage (storage :flusher
		   (lambda (storage)
                     (the fixnum (- (storage-max storage)
				    (storage-offset storage))))
	                   :sap sap :max size :store store :buffer-size nil)
      (values (restore-objects storage)))))

;;; FILES

(defun store-to-file (filename &rest elements)
  (declare (optimize speed safety))
  (with-open-file (str filename :direction :output
				:if-exists :supersede
				:element-type '(unsigned-byte 8))
    (with-storage (storage :flusher (make-write-into-storage/stream str))
      (apply #'store-objects storage elements))))

(defun restore-from-file (filename)
  (declare (optimize speed safety))
  (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
    (with-storage (storage :flusher (make-read-into-storage/stream str))
      (restore-objects storage))))

;;; General interface

(defvar *write-magic-number* nil
  "If T we will write out a magic number and *write-version* to the stream, which will be
 validated against *supported-versions* when we read it back")

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
      (vector ;; returns length
       (apply #'store-to-extant-vector place data*))
      (null
       (apply #'store-to-vector data*)))))

    

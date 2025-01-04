(in-package :cl-binary-store)

;; User facing interface is generally just `restore' and `store' except
;; for SAPs which would be `restore-from-sap' `store-to-sap' and `replace-store-sap-buffer'

(defvar *write-magic-number* nil
  "If T we will write out a magic number and *write-version* to the stream, which will be
 validated against our existing *codespaces* when we read it back")

(defvar *read-version* #x0001
  "The default codespace version to use if no versioning information is in the stream")

;;; STREAMS

(defun store-to-stream (stream &rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*))))
    (with-storage/write (storage :flusher (make-write-into-storage/stream stream))
      (apply #'store-objects storage elements)
      (flush-write-storage storage)
      (values))))

(defun restore-from-stream (stream)
  (declare (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*))))
    (with-storage/read (storage :flusher (make-read-into-storage/stream stream) :max 0 :buffer-size 32768)
      (restore-objects storage))))

;;; UB8 VECTORS

(defun store-to-vector (&rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  "Returns an (array (unsigned-byte 8) (*)) with the data"
  (let* ((output-vector (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0
				       :adjustable t))
	 (*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*))))
    (with-storage/write (storage :flusher (make-write-into-adjustable-ub8-vector output-vector))
      (apply #'store-objects storage elements)
      (flush-write-storage storage)
      output-vector)))

(defun store-to-extant-vector (vector &rest data)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*)))
	 (offset 0)
	 ;; We cannot pin objects on other lisps, so copy in those cases
	 (is-simple-octet-array #+sbcl (typep vector '(simple-array (unsigned-byte 8) (*))) #-sbcl nil)
	 (is-adjustable (adjustable-array-p vector))
	 (vector-len (length vector))
	 (flusher
	   (cond
	     (is-simple-octet-array
	      (lambda (storage) (write-storage-offset storage)))
	     (is-adjustable
	      (make-write-into-adjustable-ub8-vector vector))
	     (t
	      (lambda (storage)
		#+dribble-cbs(format t "Flusher called with vector-len ~A, offset ~A and write-storage-offset ~A~%" vector-len offset (write-storage-offset storage))
		 (assert (>= (- vector-len offset)
			    (write-storage-offset storage))
			 nil
			 'out-of-space
			 :current-offset offset
			 :wanted-bytes (write-storage-offset storage))
		 #+dribble-cbs(format t "Copying into storage vector from temp array~%")
		 #+dribble-cbs(format t "Storage-vector being written from ~A to ~A from write-storage-store of ~A to ~A~%"
			 offset (+ offset (write-storage-offset storage)) 0 (write-storage-offset storage))
		 (replace vector (write-storage-store storage)
			  :start1 offset :start2 0
			  :end2 (write-storage-offset storage))
		 (incf offset (write-storage-offset storage))
		 (setf (write-storage-offset storage) 0))))))
    (declare (dynamic-extent flusher) (type fixnum vector-len offset))
    (labels ((store (storage)
	       (apply #'store-objects storage data)
	       (if is-simple-octet-array
		   (write-storage-offset storage)
		   offset)))
      (cond
	(is-simple-octet-array
	 (with-pinned-objects (vector) ;; does nothing on #-sbcl
	   (with-storage/write (storage :flusher flusher :store vector)
	     (store storage))))
	(t
	 (with-storage/write (storage :flusher flusher)
	   (store storage)))))))

(defun restore-from-vector (vector)
  (declare (optimize speed safety))
  #+debug-cbs(format t "Restoring from a vector with ~A bytes in it~%" (length vector))
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*))))
    ;; Cannot read/write
    (if #+sbcl (typep vector '(simple-array (unsigned-byte 8) (*))) #-sbcl nil
	(with-storage/read (storage
			    :flusher
			    (lambda (storage)
			      (the fixnum (- (read-storage-max storage)
					     (read-storage-offset storage))))
			    :store vector :max (length vector))
	  (restore-objects storage))
	(flexi-streams:with-input-from-sequence (str vector)
	  (with-storage/read (storage :flusher (make-read-into-storage/stream str) :max 0)
	    (restore-objects storage))))))

;;; SAP vectors

(defun replace-store-sap-buffer (sap &key (sap-size 0) (sap-offset 0))
  (invoke-restart 'replace-storage sap sap-size sap-offset))

(defun store-to-sap (sap size &rest data)
  "This may error with `out-of-space' (which contains
 out-of-space-current-offset and out-of-space-wanted-bytes).  The
 out-of-space-current-offset is the amount of data that has been
 written so far.  out-of-space-wanted-bytes is not useful unless you
 happen to be writing very large stuff as it will likely be a small
 number representing the immediate need.  Best to allocate a big chunk
 and when this finally returns we return the amount of data we wrote
 to the chunk.  Call (replace-store-sap-buffer sap sap-offset) in a
 handler-bind to do this updating.  See test test-sap-write/read for
 an example."
  (let ((*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*)))
	(store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage/write (storage :flusher (lambda (storage) (write-storage-offset storage))
                           :sap sap :store store :max size :buffer-size nil)
      (apply #'store-objects storage data)
      (write-storage-offset storage))))

(defun restore-from-sap (sap size)
  (declare (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*)))
	(store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage/read (storage :flusher
			(lambda (storage)
			  (the fixnum (- (read-storage-max storage)
					 (read-storage-offset storage))))
				:sap sap :max size :store store :buffer-size nil :size 0)
      (values (restore-objects storage)))))

;;; FILES

(defun store-to-file (filename &rest elements)
  (declare (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*))))
    (with-open-file (str filename :direction :output
				  :if-exists :supersede
				  :element-type '(unsigned-byte 8))
      (with-storage/write (storage :flusher (make-write-into-storage/stream str))
	(apply #'store-objects storage elements)))
    filename))

(defun restore-from-file (filename)
  (declare (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*))))
    (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
      (with-storage/read (storage :flusher (make-read-into-storage/stream str) :max 0
			     :stream str)
	(restore-objects storage)))))

;;; General interface

(defun restore (place)
  "(restore #(14 39 37 0 2 72 73 15)) -> (values :hi))
 (store filename (list :hi :bye) :something)
 (restore filename/stream/place) -> (values (list :hi :bye) :something)
 (restore-from-vector (store nil :hi :bye)) -> (values :hi :bye)

 Note that if you specified *write-magic-number* then a `magic-number' will be the first value
 returned.  It will be asserted against available decoders in *codespaces*"
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*))))
    (etypecase place
      ((or string pathname)
       (restore-from-file place))
      (stream
       (restore-from-stream place))
      (vector
       (restore-from-vector place)))))

(defun store (place &rest data)
  "When place is NIL, returns a (vector (unsigned-byte 8) (*)) with fill-pointer
 ex: (store nil :hi) -> #(39 37 0 2 72 73)

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
	 (*current-codespace* (gethash *write-version* *codespaces*))
	 (data* (if *write-magic-number* (cons magic-number data) data)))
    (declare (dynamic-extent magic-number data*))
    (assert *current-codespace* nil
	    (format nil "Write-version ~A does not have an existing codespace, we have ~A"
		    *write-version* (loop for key being the hash-keys of *codespaces*
					  collect (list key (codespace-name
							      (gethash key *codespaces* ))))))
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

    

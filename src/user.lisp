(in-package :cl-binary-store)

;; User facing interface is generally just `restore' and `store' except
;; for SAPs which would be `restore-from-sap' `store-to-sap' and `replace-store-sap-buffer'

(defvar *output-magic-number* nil
  "If T we will write out a magic number and *write-version* to the output, which will be
 validated against our existing *codespaces* when we read it back.")

(defvar *read-version* #x0001
  "The default codespace version to use if no versioning information is in the stream")

;;; STREAMS

(defun store-to-stream (stream &rest elements)
  (declare (dynamic-extent elements) (optimize speed safety))
  (let ((*current-codespace* (or *current-codespace* (gethash *write-version* *codespaces*))))
    (with-storage/write (storage :flusher (make-write-into-storage/stream stream))
      (apply #'store-objects storage elements)
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

(defun restore (place &key 
			(load/save-progress-indicator *load/save-progress-indicator*)
			(allow-codespace-switching *allow-codespace-switching*)
			(max-to-read *max-to-read*)
			(read-version *read-version*))
  "PLACE may be a string or pathname designating a file to read from, or a stream to
 read from (must have element type (unsigned-byte 8)), or a vector.

 (restore #(14 39 37 0 2 72 73 15)) -> (values :hi))
 (store filename (list (list :hi :bye) :something) :as-separate-objects t)
 (restore filename/stream/place) -> (values (list :hi :bye) :something)
 (restore-from-vector (store nil :hi :bye)) -> (values :hi :bye)

 If ALLOW-CODESPACE-SWITCHING then the file can specify a version different from
 READ-VERSION and we will switch to it if it is available.

 MAX-TO-READ specifies the maximum amount of data in bytes we should load.

 LOAD/SAVE-PROGRESS-INDICATOR, if T, shows you some indications while loading"
  (let ((*current-codespace* (or *current-codespace* (gethash *read-version* *codespaces*)))
	(*load/save-progress-indicator* load/save-progress-indicator)
	(*allow-codespace-switching* allow-codespace-switching)
	(*max-to-read* max-to-read)
	(*read-version* read-version))
    (handler-case
        (etypecase place
          ((or string pathname)
           (restore-from-file place))
          (stream
           (restore-from-stream place))
          (vector
           (restore-from-vector place)))
      (babel:character-decoding-error (e)
        (unexpected-data "Expected UTF-8 data" e)))))

(defun store (place data &key (track-references *track-references*)
			   (support-shared-list-structures *support-shared-list-structures*)
			   (max-to-write *max-to-write*)
			   (as-separate-objects nil)
			   (output-end-marker *output-end-marker*)
			   (output-magic-number *output-magic-number*)
			   (write-version *write-version*)
			   (load/save-progress-indicator *load/save-progress-indicator*))
  "When PLACE is NIL, returns a (vector (unsigned-byte 8) (*)) with fill-pointer
 ex: (store nil (list 1.234d56)) #(32 5 11 90 215 48 169 108 33 148 75 5)

 When PLACE is a filename/string/pathname writes data to the respective file.

 When PLACE is a vector writes data to it and returns num used bytes... if vector is
 adjustable, it may be adjusted.  Otherwise we error if we run out of space.

 If you provide a list of objects in DATA and you specify AS-SEPARATE-OBJECTS
 they will come back from RESTORE as multiple values.  Otherwise we just store the list.

 ex: (restore (store filename (list :hi :bye) :data-is-list-of-separate-objects t)) -> (values :hi :bye).

 If OUTPUT-MAGIC-NUMBER we write out WRITE-VERSION at the beginning of the stream and
 it will then be validated on restore.

 MAX-TO-WRITE is the maximum number of bytes you want to write out before erroring.

 SUPPORT-SHARED-LIST-STRUCTURES should be T if you have circular lists or share tails of lists and
 want them to come back properly EQL.

 TRACK-REFERENCES should be T if you have references between the elements in data.

 LOAD/SAVE-PROGRESS-INDICATOR, if T, shows you some indications while loading"
  (declare (optimize speed safety))
  (let* ((magic-number (make-magic-number :number *write-version*))
	 (*current-codespace* (gethash *write-version* *codespaces*))
	 (*write-version* write-version)
	 (*max-to-write* max-to-write)
	 (*support-shared-list-structures* support-shared-list-structures)
	 (*track-references* track-references)
	 (*load/save-progress-indicator* load/save-progress-indicator)
	 (*output-end-marker* output-end-marker)
	 (data* (if output-magic-number
		    (if as-separate-objects
			(cons magic-number data)
			(list magic-number data))
		    (if as-separate-objects
			data
			(list data)))))
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

    

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
  (let ((*current-codespace* (gethash *write-version* *codespaces*)))
    (with-storage/write (storage :flusher (make-write-into-storage/stream stream))
      (apply #'store-objects storage elements)
      (values))))

(defun restore-from-stream (stream &optional (codespace (gethash *read-version* *codespaces*)))
  (declare (optimize speed safety))
  (let ((*current-codespace* codespace))
    (with-storage/read (storage :flusher (make-read-into-storage/stream stream) :buffer-size 32768)
      (restore-objects storage codespace))))

(defun restore-from-stream/no-buffering (stream &optional (codespace (gethash *read-version* *codespaces*)))
  "Restore from a stream but do not do any buffering (so we will not read any excess data from the
 stream beyond what is needed.   Couple this with data that was created with
 (store ... :output-end-marker t)."
  (let ((*current-codespace* codespace))
    (with-storage/read
        (storage :buffer-size 32768
                 :flusher (make-read-into-storage/stream stream :unbuffered t :wait-until-data t))
      (restore-objects storage codespace))))

(defun restore-from-read-storage/single-object
    (read-storage &optional (codespace (gethash *read-version* *codespaces*)))
  "Restore a single object from read-storage (a wrapper around a stream, for example).
 You may not use the stream the read-storage is based on when you are
 using this interface, as we will over-read data from the stream when
 decoding objects.  If you want to use the stream for other users,
 then you probably want to use restore-from-stream/no-buffering or
 restore-from-stream/single-bounded-object (and its pair
 store-to-stream/single-bounded-object),
 (or implement a stream wrapper which uses the read-storage object)."
  (declare (optimize (speed 3) (safety 1)))
  ;; Should I persist the global state to avoid creating hashtables and the
  ;; like for each call here??
  (restore-single-object read-storage codespace))

(defun store-to-stream/single-bounded-object
    (stream object &optional (codespace (gethash *write-version* *codespaces*)))
    (declare (optimize (speed 3) (safety 1)))
  (let ((encoded-object (store nil object :output-end-marker nil :codespace codespace)))
    (declare (type (vector (unsigned-byte 8) *) encoded-object))
    ;; Need output-end-marker because the interface is RESTORE-OBJECTS
    #+debug-cbs (format t "Length of object is ~A~%" (length encoded-object))
    (store stream (length encoded-object) :output-end-marker t :codespace codespace)
    (write-sequence encoded-object stream)
    encoded-object))

(defun restore-from-stream/single-bounded-object
    (stream &optional (codespace (gethash *read-version* *codespaces*)))
  "Restore a bounded-object from a stream.  The size of the object is
 known in advance so no end-marker is needed and we can properly used
 buffered reads.  A bounded object is one where the stream contains
 first a length in bytes which is read without buffering, then we read
 the object itself using buffering which is congnizant of the total
 length limitation.  Returns the object as the first value.  Returns
 (values nil t) if end of file."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((*current-codespace* codespace)
         (size (restore-from-stream/no-buffering stream)))
    (when (null size) ;; check this behavior, not well defined yet
      (return-from restore-from-stream/single-bounded-object
        (values nil t)))
    (assert (typep size '(and (integer 0) fixnum)) nil "corrupted data stream")
    #+debug-cbs(format t "Going to receive an object of size ~A~%" size)
    (assert (and (integerp size) (>= size 0)))
    (with-storage/read (storage :flusher (make-read-into-storage/stream/known-size
                                          stream size :wait-until-data t)
                                :buffer-size (max size 4096))
      (multiple-value-prog1
          (restore-objects storage)
        (assert (= (read-storage-max storage) (read-storage-offset storage)))))))

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

(defun restore-from-vector (vector &optional (codespace (gethash *read-version* *codespaces*)))
  (declare (optimize speed safety))
  #+debug-cbs(format t "Restoring from a vector with ~A bytes in it~%" (length vector))
  (let ((*current-codespace* codespace))
    ;; Cannot read/write
    (if #+sbcl (typep vector '(simple-array (unsigned-byte 8) (*))) #-sbcl nil
	(with-storage/read (storage
			    :flusher
			    (lambda (storage bytes)
                              (declare (ignore bytes))
			      (the fixnum (- (read-storage-max storage)
					     (read-storage-offset storage))))
			    :store vector :max (length vector))
	  (restore-objects storage codespace))
	(flexi-streams:with-input-from-sequence (str vector)
	  (with-storage/read (storage :flusher (make-read-into-storage/stream str) :max 0)
	    (restore-objects storage codespace))))))

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
  (let ((*current-codespace* (gethash *write-version* *codespaces*))
	(store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage/write (storage :flusher (lambda (storage) (write-storage-offset storage))
                           :sap sap :store store :max size :buffer-size nil)
      (apply #'store-objects storage data)
      (write-storage-offset storage))))

(defun restore-from-sap (sap size &optional (codespace (gethash *read-version* *codespaces*)))
  (declare (optimize speed safety))
  (let ((*current-codespace* codespace)
	(store (make-array 0 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent store))
    (with-storage/read (storage :flusher
			(lambda (storage bytes)
                          (declare (ignore bytes))
			  (the fixnum (- (read-storage-max storage)
					 (read-storage-offset storage))))
				:sap sap :max size :store store :buffer-size nil :size 0)
      (values (restore-objects storage codespace)))))

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

(defun restore-from-file (filename &optional (codespace (gethash *read-version* *codespaces*)))
  (declare (optimize speed safety))
  (let ((*current-codespace* codespace))
    (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
      (with-storage/read (storage :flusher (make-read-into-storage/stream str) :max 0
				  :stream str)
	(restore-objects storage codespace)))))

;;; General interface

(defun restore (place &key 
			(load/save-progress-indicator *load/save-progress-indicator*)
			(allow-codespace-switching *allow-codespace-switching*)
			(max-to-read *max-to-read*)
			(read-version *read-version*)
                        (codespace (gethash read-version *codespaces*)))
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
  (let* ((*read-version* read-version)
         (*current-codespace* codespace)
         (*load/save-progress-indicator* load/save-progress-indicator)
         (*allow-codespace-switching* allow-codespace-switching)
         (*max-to-read* max-to-read))
    (handler-case
        (etypecase place
          ((or string pathname)
           (restore-from-file place codespace))
          (stream
           (restore-from-stream place codespace))
          (vector
           (restore-from-vector place codespace)))
      (babel:character-decoding-error (e)
        (unexpected-data "Expected UTF-8 data" e)))))

(defun store (place data &key (track-references *track-references*)
			   (support-shared-list-structures *support-shared-list-structures*)
			   (max-to-write *max-to-write*)
			   (as-separate-objects nil)
			   (output-end-marker *output-end-marker*)
			   (output-magic-number *output-magic-number*)
			   (write-version *write-version*)
                           (codespace (gethash write-version *codespaces*))
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
  (let* ((*write-version* write-version)
         (magic-number (make-magic-number :number *write-version*))
         (*current-codespace* codespace)
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
		    write-version (loop for key being the hash-keys of *codespaces*
					collect (list key (codespace-name
							   (gethash key *codespaces*))))))
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

    

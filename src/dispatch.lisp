(in-package :cl-binary-store)

(defun build-read-dispatch ()
  `(defun read-dispatch (code storage references restore-object)
     #-debug-cbs(declare (optimize (speed 3) safety))
     ,(make-read-dispatch-table 'code)))
  
(defun build-store-objects ()
  `(defun store-objects (storage &rest stuff)
     (declare (optimize speed safety) (type storage storage))
     (let* ((track-references *track-references*)
            (slot-info (make-hash-table :test 'eql))
	    (*slot-info* slot-info))
       (declare (dynamic-extent slot-info))
       (with-reference-tables (track-references)
	 #+debug-cbs (format t "Starting reference counting pass on ~A objects~%" (length stuff))
         (labels ((store-object2 (obj)
                    (let ((store-object #'store-object)
                          (storage nil))
		      (store-object/reference-phase obj)))
                  (store-object (obj)
                    (let ((store-object #'store-object2)
                          (storage nil))
		      (store-object/reference-phase obj))))
           (declare (inline store-object2)) ;; inline one level deep
           (when track-references
	     (dolist (elt stuff)
	       (store-object elt))))
         #+debug-cbs (format t "Finished reference counting pass~%")
         (let ((ref-id 0))
           (declare (type fixnum ref-id))
           (when track-references
             (map-reference-tables #'analyze-references-hash-table) ;; debugging only
             ;; Now clean up the references table: delete anyone who has no references
             #+debug-cbs (format t "Generating real reference hash-table~%")
             (map-reference-tables
	      (lambda (table-name table)
		(declare (ignore table-name))
		(maphash (lambda (k v)
        	           (if (> (the fixnum v) 1)
        		       (setf (gethash k table) (- (incf ref-id))) ; signal it needs writing
			       (remhash k table)))
                         table)))
             #+debug-cbs
             (map-reference-tables (lambda (table-name table)
                                     (format t "~A: there are ~A actual references~%"
                                             table-name
                                             (hash-table-count table)))))
           #+debug-cbs (format t "Starting actual storage phase~%")
           (labels ((store-object2 (obj) ;; inline one deep
		      (let ((store-object #'store-object))
			(store-object/storage-phase obj)))
                    (store-object (obj)
		      (let ((store-object #'store-object2))
			(store-object/storage-phase obj))))
             (declare (inline store-object2))
             (when (>= ref-id 2048) ;; if we would have to expand the references vector
	       #+debug-cbs (format t "Writing total reference count ~A to file~%" (1+ ref-id))
	       (write-reference-count (1+ ref-id) #'store-object))
             (dolist (elt stuff)
	       (store-object elt))))
         (flush-write-storage storage)))))

(defun build-restore-objects ()
  `(defun restore-objects (storage)
     "Returns all the elements in storage.  If a single element just
     returns it, otherwise returns a list of all elements restored."
     (let* ((references-vector (make-array 2048))
	    (references (make-references :vector references-vector))
	    (*version-being-read* nil))
       (declare (dynamic-extent references references-vector))
       (labels ((restore-object (&optional (tag (restore-ub8 storage)))
                  (read-dispatch tag storage references #'restore-object)))
         (let ((objects
                 (loop for code = (maybe-restore-ub8 storage)
		       while code
		       with object and ignore
		       do #+debug-cbs (format t "Read code ~A (offset ~A max ~A)~%" code
					      (storage-offset storage) (storage-max storage))
                          (setf (values object ignore) (restore-object code))
		       unless ignore
                         collect object)))
	   (apply #'values objects))))))

(defun analyze-references-hash-table (table-name references)
  (declare (ignorable table-name references))
  #+debug-clb(let ((types (make-hash-table :test 'equal))
                   (individual-reference-counts (make-hash-table :test 'equal))
                   (max-refed (make-hash-table :test 'equal))
                   (total-references-used 0)
                   (total-unique-multiply-referenced-objects 0))
	       (declare (type fixnum total-references-used total-unique-multiply-referenced-objects))
	       (maphash (lambda (k v)
                          (let ((type (type-of k)))
                            (incf (gethash type types 0) v)
                            (when (> v 1)
			      (incf total-references-used v)
			      (incf total-unique-multiply-referenced-objects))
                            (push v (gethash type individual-reference-counts))
                            (when (< (car (gethash type max-refed (cons 0 nil))) v)
			      (setf (gethash type max-refed) (cons v k)))))
	                references)
	       (format t "~A: Total ~A references (out of ~A objects) emitted to file with ~A ~
                            reference ids allocated~%" table-name total-references-used
                            (hash-table-count references)  total-unique-multiply-referenced-objects)
               (when (not (zerop total-references-used))
		 (let (data)
                   (format t "Reference types are:~%")
                   (maphash (lambda (type total-count)
                              (push (cons type total-count) data))
                            types)
                   (setf data (sort data #'> :key #'cdr))
                   (map nil (lambda (d)
                              (let ((individual-counts (gethash (car d) individual-reference-counts)))
				(format t "~A of type ~A (~A unique)~%     ~
                                    avg ref count ~,3f / min ~,3f / max ~,3f / frac>1 ~,3f~
                                    ~A"
					(cdr d) (car d) (length individual-counts)
					(/ (cdr d) (length individual-counts) 1d0)
					(loop for i fixnum in individual-counts minimizing i)
					(loop for i fixnum in individual-counts maximizing i)
					(/ (count-if (lambda (x) (> x 1)) individual-counts)
                                           (length individual-counts) 1d0)
					(let ((obj (gethash (car d) max-refed)))
                                          (let ((*print-circle* t))
                                            (if (> (car obj) 1)
                                                (format nil "~%     most-refed ~A times: ~S~%" (car obj) (cdr obj))
                                                (format nil "~%")))))))
			data)))))

  (defmacro rebuild-dispatch ()
    `(progn
       (format t "~&CL-BINARY-STORE: recompiling dispatch mechanisms~%")
       ,(build-read-dispatch)
       ,(build-store-objects)
       ,(build-restore-objects)
       (format t "~&CL-BINARY-STORE: done~%")
       (values)))

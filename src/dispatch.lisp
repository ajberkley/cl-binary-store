(in-package :cl-store-faster)

;; Since reading is very fast now, let's work on writing.
;; We can parallelize if storing multiple objects --- we could
;; start after the reference counting phase, but it looks like that's
;; maybe 1/3rd of the total time, so we'd get only a 2-3x speed-up
;; at best.  That's nice, and forces me to work out the parallel
;; disk writing, but...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-read-dispatch-table (code)
    `(case ,code
       ,@(sort
	  (let ((restore-codes nil))
	    (maphash (lambda (k code-info)
		       (when (numberp k)
			 (let ((code (code-info-code code-info))
			       (func-name (code-info-restore-func-name code-info))
			       (needs-references (code-info-restore-references code-info)))
			   (push (list
				  code
				  #+debug-csf
				  `(format
				    t
				    ,(format nil "Restoring from code ~A: ~A~~%" code func-name))
				  `(,func-name storage
					       ,@(when needs-references '(references))))
				 restore-codes))))
		     *code-info*)
	    restore-codes)
	  #'< :key #'first)))
  
  (defmacro make-read-dispatch ()
    `(progn
       (declaim (sb-ext:maybe-inline read-dispatch))
       (defun read-dispatch (code storage references)
	 (declare (optimize speed safety))
	 ,(make-read-dispatch-table 'code))))
  
  (make-read-dispatch)

  (defun strict-subtype-ordering (type-specs &key (key #'identity))
    ;; This sort of works, but some weird issues I haven't debugged yet
    ;; with the type hierarchy.
    ;; We need to order these by subtypep, a simple sort won't work
    ;; because we have disjoint sets.  So first, we have to sort into
    ;; disjoint sets, then sort, then recombine.
    (let* ((type-groups '(cons symbol integer ;; fixnum bignum
			  vector array number structure-object standard-object t))
	   (groups (make-array (length type-groups) :initial-element nil)))
      (loop for item in type-specs
	    do (loop for count below (length type-groups)
		     for type-group in type-groups
		     until (subtypep (funcall key item) type-group)
		     finally
			(push item (svref groups count))))
      (loop for g across groups
	    appending (stable-sort (reverse g) #'subtypep :key key))))

  (defmacro generate-store-object (value storage references)
    "Here we specialize the calls to have storage nil or not nil so the inlined
 storage code can delete the actual storage code when not used"
    `(etypecase value
       ,@(strict-subtype-ordering
	  (let ((type-dispatch-table nil))
	    (maphash (lambda (k code-info)
		       (when (not (numberp k))
			 (let ((type-spec (code-info-type code-info))
			       #+debug-csf (code (code-info-code code-info))
			       (func-name (code-info-store-func-name code-info))
			       (takes-references (code-info-store-references code-info)))
			   (push
			    (list
			     type-spec
			     #+debug-csf
			     `(format t ,(format nil
						 "Dispatching to code ~A: ~A~~%"
						 code func-name))
			     ;; symbol call when debugging for no inline
			     #+debug-csf
			     `(locally (declare (notinline ,func-name))
			       (funcall ',func-name ,value ,storage
					,@(when takes-references `(,references))))
			     #-debug-csf
			     `(locally (declare (inline ,func-name))
				(,func-name ,value ,storage
					    ,@(when takes-references `(,references)))))
			    type-dispatch-table))))
		     *code-info*)
	    type-dispatch-table)
	  :key #'first)))

  (declaim (inline store-object/storage)) ;; to propagate storage / references
  (defun store-object/storage (value storage references)
    (generate-store-object value storage references))
  
  (declaim (inline store-object/no-storage))
  (defun store-object/no-storage (value references)
    (generate-store-object value nil references))

  ;; Just synchronizing references slows us to 1.2 seconds and BIG slow down
  ;; while actually parallelizing the reference counting pass.
  ;; Normal: 483 ms, just synchronized hash-table single threaded 1.2 seconds.
  ;; brutal.  I suppose we could go lock-less eql hashing instead of eq hashing
  ;; for our objects
  (declaim (inline store-objects/generic)) ;; so storage can be specialized down the chain
  (defun store-objects/generic (storage &rest stuff)
    (declare (optimize speed safety))
    (let* ((references (make-hash-table :test 'eql :size 256 :synchronized nil))
	   (struct-info (make-hash-table :test 'eql :synchronized nil))
	   (*struct-info* struct-info))
      (declare (dynamic-extent struct-info references))
      #+debug-csf (format t "Starting reference counting pass on ~A objects~%" (length stuff))
      (dolist (elt stuff)
       	(store-object/no-storage elt references))
      #+debug-csf (format t "Finished reference counting pass~%")
      (let ((new-ht (make-hash-table :test 'eql))
	    (ref-id 0))
	(declare (type fixnum ref-id))
	;; Now clean up the references table: delete anyone who has no references
	#+debug-csf (format t "Generating real reference hash-table~%")
	(maphash (lambda (k v)
		   (when (> (the fixnum v) 1)
		     (setf (gethash k new-ht) (- (incf ref-id))))) ; signal it needs writing
		 references)
	(clrhash references)		; help the gc?
	(setf references new-ht)
	#+debug-csf (format t "There are ~A actual references~%" (hash-table-count new-ht)))
      (dolist (elt stuff)
	(store-object/storage elt storage references))
      (flush-write-storage storage)))

  (defun store-objects/buffering-write-storage (storage &rest stuff)
    (declare (type buffering-write-storage storage) (inline store-objects/generic))
    (apply #'store-objects/generic storage stuff))

  (defun store-objects/sap-write-storage (storage &rest stuff)
    (declare (type sap-write-storage storage))
    (apply #'store-objects/generic storage stuff))

  (declaim (inline restore-object))
  (defun restore-object (storage references &optional (tag (restore-ub8 storage)))
    (declare (notinline read-dispatch))
    (read-dispatch tag storage references))

  (declaim (inline store-object))
  (defun store-object (value storage references)
    (declare (notinline store-object/storage) (notinline store-object/no-storage))
    (if storage
	(store-object/storage value storage references)
	(store-object/no-storage value references)))

  (defun restore-objects (storage)
    "Returns all the elements in storage.  If a single element just
 returns it, otherwise returns a list of all elements restored."
    (let* ((references-vector (make-array 1024 :initial-element nil)) ;; not needed but helps with debuggingfe
	   (references (make-references :vector references-vector))
	   (first-code (maybe-restore-ub8 storage))
	   (first-result (when first-code (read-dispatch first-code storage references))))
      (declare (dynamic-extent references references-vector))
      (when first-code
	(let ((rest (loop for code = (maybe-restore-ub8 storage)
			  while code
			  collect (read-dispatch code storage references))))
	  (if rest
	      (cons first-result rest)
	      first-result))))))

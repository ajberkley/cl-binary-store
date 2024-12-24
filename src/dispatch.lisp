(in-package :cl-store-faster)

;; Since reading is very fast now, let's work on writing.
;; We can parallelize if storing multiple objects --- we could
;; start after the reference counting phase, but it looks like that's
;; maybe 1/3rd of the total time, so we'd get only a 2-3x speed-up
;; at best.  That's nice, and forces me to work out the parallel
;; disk writing, but...

;; TODO CHANGE FROM STRAIGHT ECASE TO NESTED BY THE TOP LEVEL TYPE

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
    ;; TODO CHANGE TO DUAL LAYER DISPATCH TO HELP WITH CODE GEN
    ;; FIXNUM -> , structure-object ->, standard-object ->, vector ->, array ->, symbol ->
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
			     `(,func-name ,value ,storage
					 ,@(when takes-references `(,references))))
			    type-dispatch-table))))
		     *code-info*)
	    type-dispatch-table)
	  :key #'first)))

  ;; NO, so what we are going to do here is define the lambda-lists in register-code
  ;; so users can provide optimized reference phase stuff

  (declaim (notinline store-object))
  (defun store-object (value storage references)
    (declare (type (or null storage) storage))
    (if storage ;; can remove by passing in a store-object func to serializers
	(generate-store-object value storage references)
	(generate-store-object value nil references)))
  
  (declaim (notinline store-object/ref-count-phase))
  (defun store-object/ref-count-phase (value references)
    (generate-store-object value nil references))

  ;; Just synchronizing references slows us to 1.2 seconds and BIG slow down
  ;; while actually parallelizing the reference counting pass.
  ;; Normal: 483 ms, just synchronized hash-table single threaded 1.2 seconds.
  ;; brutal.  I suppose we could go lock-less eql hashing instead of eq hashing
  ;; for our objects
  (declaim (notinline store-objects)) ;; so storage can be specialized down the chain
  (defun store-objects (storage &rest stuff)
    (declare (optimize speed safety))
    (let* ((track-references *track-references*)
	   (references (when track-references
			 (make-hash-table :test 'eql :size 256 :synchronized nil)))
	   (struct-info (make-hash-table :test 'eql :synchronized nil)) ;; if use the metaclass object maybe can use eq?  can I do that for structs?
	   (*struct-info* struct-info))
      (declare (dynamic-extent struct-info references))
      (when track-references
	#+debug-csf (format t "Starting reference counting pass on ~A objects~%" (length stuff))
	(dolist (elt stuff)
       	  (store-object/ref-count-phase elt references))
	#+debug-csf (format t "Finished reference counting pass~%")
	(let ((new-ht (make-hash-table :test 'eql))
	      (ref-id 0))
	  (declare (type fixnum ref-id))
          (analyze-references-hash-table references) ;; debugging code
	  ;; Now clean up the references table: delete anyone who has no references
	  #+debug-csf (format t "Generating real reference hash-table~%")
	  (maphash (lambda (k v)
		     (when (> (the fixnum v) 1)
		       (setf (gethash k new-ht) (- (incf ref-id))))) ; signal it needs writing
		   references)
	  (clrhash references)		; help the gc?
	  (setf references new-ht)
	  (when (>= ref-id 8192)
	    (write-reference-count (1+ ref-id) storage))
	  #+debug-csf (format t "There are ~A actual references~%" (hash-table-count new-ht))))
      (dolist (elt stuff)
	(store-object elt storage references))
      (flush-write-storage storage)))

  (declaim (notinline restore-object))
  (defun restore-object (storage references &optional (tag (restore-ub8 storage)))
    (declare (notinline read-dispatch))
    (read-dispatch tag storage references))
  
  (defun restore-objects (storage)
    "Returns all the elements in storage.  If a single element just
 returns it, otherwise returns a list of all elements restored."
    ;; NOMINALLY WE SHOULD BE WRITING OUT THE NUMBER OF REFERENCES TO AVOID RESIZING
    (let* ((references-vector (make-array 8192))
	   (references (make-references :vector references-vector))
	   (first-code (maybe-restore-ub8 storage))
	   (first-result (when first-code (read-dispatch first-code storage references)))
	   (*version-being-read* nil))
      (declare (dynamic-extent references references-vector))
      (when first-code
	(let ((rest (loop for code = (maybe-restore-ub8 storage)
			  while code
			  collect (read-dispatch code storage references))))
	  (apply #'values first-result rest)))))

  (defun analyze-references-hash-table (&optional references)
    (declare (ignorable references))
    ;;(defparameter *saved-refs* references)
    #+debug-csf(let ((types (make-hash-table :test 'equal))
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
		 (format t "Total references emitted to file ~A with total reference ids allocated ~A~%~
               There were ~A possible objects being referenced~%"
			 total-references-used total-unique-multiply-referenced-objects (hash-table-count references))
		 (let (data)
                   (format t "Reference types are:~%")
                   (maphash (lambda (type total-count)
                              (push (cons type total-count) data))
                            types)
                   (setf data (sort data #'> :key #'cdr))
                   (map nil (lambda (d)
                              (let ((individual-counts (gethash (car d) individual-reference-counts)))
				(format t "~A of type ~A (~A unique)~%     ~
                                    avg ref count ~,3f / min ~,3f / max ~,3f / frac>1 ~,3f~%     ~
                                    most-refed: ~A)~%"
					(cdr d) (car d) (length individual-counts)
					(/ (cdr d) (length individual-counts) 1d0)
					(loop for i fixnum in individual-counts minimizing i)
					(loop for i fixnum in individual-counts maximizing i)
					(/ (count-if (lambda (x) (> x 1)) individual-counts)
                                           (length individual-counts) 1d0)
					(gethash (car d) max-refed))))
			data)))))

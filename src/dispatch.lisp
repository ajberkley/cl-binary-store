(in-package :cl-store-faster)

;; (push :precompute-dispatch *features*)
;; Do not enable this, it slows things down and is a work in progress
;; (setf *features* (delete :precompute-dispatch *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *restore-dispatch-table-names*
    (make-array 256)
    "For debugging, names of each code type")

  (defun make-read-dispatch ()
    `(progn
       (declaim (sb-ext:maybe-inline read-dispatch))
       (defun read-dispatch (code storage)
	 #+debug-csf (format t "Restoring from code ~A: ~A~%" code
			     (svref *restore-dispatch-table-names* code))
	 (case code
           ,@(loop for elt fixnum from 0
                   for value across *code-restore-info*
		   do (setf (svref *restore-dispatch-table-names* elt) value)
                   when value
                     collect (list elt (list value 'storage)))))))
  
  (defmacro eval-make-read-dispatch ()
    `,(make-read-dispatch))

  `,(eval-make-read-dispatch)

  (declaim (inline restore-object))
  (defun restore-object (storage &optional (tag (maybe-restore-ub8 storage)))
    (declare (notinline read-dispatch))
    (read-dispatch tag storage))

  (defun strict-subtype-ordering (type-specs &key (key #'identity))
    ;; This doesn't actually work
    (let* ((type-groups '(cons symbol integer;; fixnum bignum
			  array number structure-object standard-object t))
	   (groups (make-array (length type-groups) :initial-element nil)))
      (loop for item in type-specs
	    do (loop for count below (length type-groups)
		     for type-group in type-groups
		     until (subtypep (funcall key item) type-group)
		     finally
			(push item (svref groups count))))
      (loop for g across groups
	    appending (stable-sort (reverse g) #'subtypep :key key))))

  (declaim (type simple-vector *store-dispatch-table*))
  (defvar *store-dispatch-table*
    (make-array 256 :initial-element (lambda (&rest rest)
				       (declare (ignore rest))
				       (error "undefined dispatch slot"))))
  (defvar *store-dispatch-table-names*
    (make-array 256)
    "For debugging, names of each code type")
  
  #+precompute-dispatch (declaim (type (vector (unsigned-byte 8) *) *dispatch*))
  #+precompute-dispatch (defvar *dispatch* (make-array 0 :element-type '(unsigned-byte 8))
    "A UB8 extendable array holding the codes of objects to be stored in order (until we
     parallelize the reference and dispatch compilation pass)")

  #+precompute-dispatch (declaim (type (simple-array (unsigned-byte 8) (*)) *dispatch-compiled*))
  #+precompute-dispatch (defvar *dispatch-compiled*
			  (make-array 0 :element-type '(unsigned-byte 8)))

  #+precompute-dispatch (declaim (type (unsigned-byte 50) *dispatch-index*))
  #+precompute-dispathc (defvar *dispatch-index* 0
			  "An offset into the *dispatch* table.  Should not be a global bleh, add to function parameters")

  (defvar *references-already-fixed* nil
    "This is NIL during the reference counting phase and T when we are actually serializing data
     TODO replace me with function parameters so this can get resolved at compile time?")
  
  (defun make-store-object ()
    `(progn
       (declaim (notinline store-object/storage store-object/no-storage)
		(inline store-object))

       (defun store-object (value storage)
	 (if storage
	     (store-object/storage value storage)
	     (store-object/no-storage value)))
       
       (defun store-object/storage (value storage)
	 (declare (optimize speed safety))
	 #+precompute-dispatch
	 `(let ((index *dispatch-index*))
	    #+debug-csf (format t "Dispatching to code ~A: ~A~%"
				(aref *dispatch* index)
				(svref *store-dispatch-table-names*
				       (aref *dispatch* index)))
	    (setf *dispatch-index* (+ 1 index))
	    (locally (declare (optimize (speed 3) (safety 0)))
	      (funcall (the function
			    (svref *store-dispatch-table* (aref *dispatch-compiled* index)))
		       value storage)))
	 #-precompute-dispatch
	 (etypecase
	     value
	   ,@(strict-subtype-ordering
	      (loop for type-spec being the hash-keys of *code-store-info*
		    for idx fixnum from 0
		    for func = (gethash type-spec *code-store-info*)
		    collect (list type-spec
				  #+debug-csf
				  `(format t 
					   ,(format nil
						    "Dispatching to code ~A: ~A~~%"
						    idx
						    (aref *store-dispatch-table-names*
							  idx)))
				  #+debug-csf `(funcall ',func value storage)
				  #-debug-csf `(,func value storage)
				  ))
	      :key #'first)))
       
       (defun store-object/no-storage (value)
	 (declare (optimize speed safety))
	 (etypecase value
	   ;; We need to order these by subtypep, a simple sort won't work
	   ;; because we have disjoint sets.  So first, we have to sort into
	   ;; disjoint sets, then sort, then recombine.
           ,@(strict-subtype-ordering
	      (loop for type-spec being the hash-keys of *code-store-info*
		    for idx fixnum from 0
		    for func = (gethash type-spec *code-store-info*)
		    do (setf (svref *store-dispatch-table-names* idx) func)
		       (setf (svref *store-dispatch-table* idx)
			     (compile nil `(lambda (value storage)
					     #+debug-csf
					     (format t
						     ,(format nil "Dispatching to code ~A: ~A~~%"
							      idx (aref *store-dispatch-table-names* idx)))
					     #+debug-csf (funcall ',func value storage)
					     #-debug-csf(,func value storage)
					     )))
		       ;; this way the compiler knows storage is nil and
		       ;; we have compile time clean-up and dispatch.
		    collect (list type-spec
				  #+precompute-dispatch
				    (locally (declare (optimize (speed 3) (safety 0)))
				      `(vector-push-extend ,idx *dispatch*))
				  `(,func value nil)))
	      :key #'first)))))
  
  (defmacro eval-make-store-object ()
    `,(make-store-object))

  (eval-make-store-object) ;; macro-expand me for fun!

  (defun clear-cached-tables ()
    (eval (make-store-object))
    (eval (make-read-dispatch)))

  (declaim (inline store-objects/generic))  
  (defun store-objects/generic (storage &rest stuff)
    (declare (optimize speed safety)
	     (inline store-object))
    (let* ((references (make-hash-table :test 'eql :size 256))
	   (*references* references)
	   (struct-info (make-hash-table :test 'eql))
	   (*struct-info* struct-info)
	   #+precompute-dispatch
	   (dispatch (make-array 65536 :element-type '(unsigned-byte 8)
				       :fill-pointer 0 :adjustable t))
	   #+precompute-dispatch (*dispatch* dispatch))
      (declare (dynamic-extent #+precompute-dispatch dispatch struct-info references))
      ;; First reference pass and dispatch compilation
      #+debug-csf (format t "Starting reference counting pass~%")
      (dolist (elt stuff)
	(store-object/no-storage elt))
      #+debug-csf (format t "Did reference counting pass~%")
      (let ((old-ht *references*)
	    (new-ht (make-hash-table :test 'eql))
	    (ref-id 0))
	(declare (type fixnum ref-id))
	;; Now clean up the references list
	;; delete anyone who has no references
	#+debug-csf (format t "Generating real reference hash-table~%")
	(maphash (lambda (k v)
		   (when (> (the fixnum v) 1)
		     (setf (gethash k new-ht) (- (incf ref-id))))) ;; signal it needs writing
		 old-ht)
	(clrhash old-ht)  ; help the gc?
	(setf old-ht nil) ; help the gc? this should be the only necessary thing
	#+debug-csf (format t "There are ~A actual references~%" (hash-table-count new-ht))
	(setf *references* new-ht))
      (let ((*references-already-fixed* t)
	    #+precompute-dispatch (*dispatch-index* 0)
	    #+precompute-dispatch (*dispatch-compiled*
				   (make-array (length *dispatch*)
					       :element-type '(unsigned-byte 8)
					       :initial-contents *dispatch*)))
	#+debug-csf (format t "Compiled dispatch info for ~A objects~%" (length *dispatch*))
	(dolist (elt stuff)
	  (store-object/storage elt storage)))
      (flush-write-storage storage)))

  (defun store-objects/buffering-write-storage (storage &rest stuff)
    (declare (type buffering-write-storage storage))
    (apply #'store-objects/generic storage stuff))

  ;; (defun store-objects/sap-write-storage (storage &rest stuff)
  ;;   (declare (type sap-write-storage storage))
  ;;   (store-objects/generic storage))
  
  (defun restore-objects (storage)
    "Returns all the elements in storage.  If a single element just
 returns it, otherwise returns a list of all the elements."
    (let* ((references (make-array 1024 :adjustable t :fill-pointer nil))
	   (*references* references))
      (declare (dynamic-extent references))
      (let* ((first-code (maybe-restore-ub8 storage))
	     (first-result (when first-code (read-dispatch first-code storage))))
	(when first-code
	  (let ((rest (loop for code = (maybe-restore-ub8 storage)
			    while code
			    collect (read-dispatch code storage))))
	    (if rest
		(cons first-result rest)
		first-result)))))))

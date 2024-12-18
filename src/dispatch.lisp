(in-package :cl-store-faster)

;; This package registers and builds the dispatch mechanism.  You must call
;; (clear-cached-tables) if you happen

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-read-dispatch ()
    `(progn
       (declaim (sb-ext:maybe-inline read-dispatch))
       (defun read-dispatch (code storage)
	 (case code
           ,@(loop for elt fixnum from 0
                   for value across *code-restore-info*
                   when value
                     collect (list elt (list value 'storage)))))))
  
  (defmacro eval-make-read-dispatch ()
    `,(make-read-dispatch))

  `,(eval-make-read-dispatch)

  (declaim (inline restore-object))
  (defun restore-object (storage &optional (tag (restore-ub8 storage t)))
    (declare (notinline read-dispatch))
    (read-dispatch tag storage))

  (defun strict-subtype-ordering (type-specs &key (key #'identity))
    (let* ((type-groups '(number cons symbol array structure-object standard-object t))
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

  (declaim (type vector *dispatch*))
  (defvar *dispatch* (make-array 0)
    "A UB8 extendable array holding the codes of objects to be stored in order (until we
     parallelize the reference and dispatch compilation pass)")

  (defvar *dispatch-index* 0
    "An offset into the *dispatch* table.  Should not be a global bleh.")

  (defvar *references-already-fixed* nil
    "If t, do not attempt to record references... TODO replace me with function parameters
     so this can get resolved at compile time")
  
  (defun make-store-object ()
    `(progn
       (declaim (notinline store-object/storage store-object/no-storage)
		(inline store-object))

       (defun store-object (value storage)
	 (if storage
	     (store-object/storage value storage)
	     (store-object/no-storage value)))
       
       ;; We can't inline store-object
       (defun store-object/storage (value storage)
	 (declare (optimize speed safety))
	 ;; if STORAGE is nil, we want to build up a dispatch set
	 (if *do-explicit-reference-pass*
	     (prog1
		 (let ((index *dispatch-index*))
		   (format t "Dispatching to code ~A~%" (aref *dispatch* index))
		   (setf *dispatch-index* (+ 1 index))
		   (funcall (the function
				 (svref *store-dispatch-table* (aref *dispatch* index)))
			  value storage))
	       (incf (the fixnum *dispatch-index*)))
	     (etypecase value
	       ;; We need to order these by subtypep, a simple sort won't work
	       ;; because we have disjoint sets.  So first, we have to sort into
	       ;; disjoint sets, then sort, then recombine.
               ,@(strict-subtype-ordering
		  (loop for type-spec being the hash-keys of *code-store-info*
			for func = (gethash type-spec *code-store-info*)
			collect (list type-spec
				      (list func 'value 'storage)))
		  :key #'first))))
       
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
		    do (setf (svref *store-dispatch-table* idx)
			     (compile nil `(lambda (value storage)
					     (funcall ',func value storage) ; debug
					     ;; (,func value storage) ; normal
					     )))
		       ;; this way the compiler knows storage is nil and
		       ;; we have compile time clean-up and dispatch.
		    collect (list type-spec
				  `(vector-push-extend ,idx *dispatch*)
				  ;; TODO: WE COULD HAVE THE MACRO THAT BUILDS
				  ;; THE STORAGE FUNCTIONS NOTE THAT THERE ARE
				  ;; NO NESTED CALLS INSIDE TO AVOID THIS CALL
				  ;; AND THE (WHEN STORAGE) WRAPPERS IN EACH
				  ;; FUNCTION.  Though if they are inline this
				  ;; will be fine...
				  `(,func value nil)))
	      :key #'first)))))
  
  (defmacro eval-make-store-object ()
    `,(make-store-object))

  (eval-make-store-object)

  (defun clear-cached-tables ()
    (eval (make-store-object))
    (eval (make-read-dispatch)))

  (defvar *do-explicit-reference-pass* t)
  
  (defun store-objects (storage &rest stuff)
    (declare (inline store-object)) ;; can't inline it here which sux
    ;; would save one branch in store-object, but that will be hot
    (let ((*references* (make-hash-table :test 'eql :size 1024
					 ))
	  (*struct-info* (make-hash-table :test 'eql))
	  (*dispatch*
	    (when *do-explicit-reference-pass*
	      (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))))
      ;; I guess we could also record the objects?  Hm... not worth the space costs I don't
      ;; think.  But... hm...
      ;; First reference pass and dispatch compilation
      (when *do-explicit-reference-pass* ;; this will take 0.12 seconds on a 1.7 second thing
	#+debug-csf (format t "Starting reference counting pass~%")
	(dolist (elt stuff)
	  (store-object/no-storage elt))
	#+debug-csf (format t "Did reference counting pass~%")
	(let ((old-ht *references*)
	      (new-ht (make-hash-table :test 'eql))
	      (ref-id 0))
	  ;; Now clean up the references list
	  ;; delete anyone who has no references
	  #+debug-csf (format t "Generating real reference hash-table~%")
	  (maphash (lambda (k v)
		     (when (> v 1)
		       (setf (gethash k new-ht) (- (incf ref-id))))) ;; signal it needs writing
		   old-ht)
	  (clrhash old-ht)  ; help the gc?
	  (setf old-ht nil) ; help the gc? this should be the only necessary thing
	  #+debug-csf (format t "There are ~A actual references~%" (hash-table-count new-ht))
	  (setf *references* new-ht)))

      (let ((*references-already-fixed* *do-explicit-reference-pass*)
	    (*dispatch-index* 0))
	(dolist (elt stuff)
	  (store-object/storage elt storage)))
      (flush-storage storage)))
  
  (defun restore-objects (storage)
    "Returns all the elements in storage.  If a single element just
 returns it, otherwise returns a list of all the elements."
    (let ((*references* (make-array 1024 :adjustable t :fill-pointer 0))
	  (*references-already-fixed* *do-explicit-reference-pass*)) ;; format needs to be stored in file!
      (let ((result
	      (loop for code = (restore-ub8 storage t)
		    while code
		    collect (read-dispatch code storage))))
	(if (cdr result) result (car result))))))


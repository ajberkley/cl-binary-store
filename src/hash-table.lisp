(in-package :cl-binary-store)

(defun store-hash-table (ht storage store-object)
  (declare (optimize speed safety) (type hash-table ht) (type function store-object))
  (when storage
    (store-ub8/no-tag +hash-table-code+ storage)
    (store-tagged-unsigned-fixnum (hash-table-count ht) storage)
    (store-tagged-unsigned-fixnum (hash-table-size ht) storage))
  (funcall store-object (hash-table-test ht)) ;; a symbol
  (funcall store-object (hash-table-rehash-threshold ht)) ;; float
  (funcall store-object (hash-table-rehash-size ht)) ;; float
  #+sbcl (when storage (store-boolean (sb-ext:hash-table-synchronized-p ht) storage))
  #-sbcl (when storage (store-nil storage))
  #+sbcl (funcall store-object (sb-ext:hash-table-weakness ht))
  #-sbcl (when storage (store-nil storage))
  (maphash (lambda (k v)
	     (funcall store-object k)
	     (funcall store-object v))
	   ht))

(defun restore-hash-table (storage restore-object)
  (declare (type function restore-object))
  (let* ((hash-table-count (restore-tagged-unsigned-fixnum storage))
         (size (restore-tagged-unsigned-fixnum storage))
	 (ht
	   (let ((test (funcall restore-object))
		 (rehash-threshold (funcall restore-object))
		 (rehash-size (funcall restore-object))
		 (synchronized (funcall restore-object))
                 (weakness (funcall restore-object)))
             #-sbcl (declare (ignore synchronized weakness))
	     ;; weakness works as far as I can discern
	     ;; because of how we do reference restoration
             (unless (typep rehash-size '(or (integer 1 *) (float (1.0) *)))
               (unexpected-data "rehash-size is not correct"))
             (unless (< size (ash most-positive-fixnum -4))
               (unexpected-data "hash table too large"))
	     (check-if-too-much-data (read-storage-max-to-read storage)
				     (* 16 size)) ;; an estimate
	     (make-hash-table :test test :size size
			      :rehash-size rehash-size
			      :rehash-threshold rehash-threshold
			      #+sbcl :synchronized #+sbcl synchronized
			      #+sbcl :weakness #+sbcl weakness))))
    ;; the keys may not be fully reified yet, so we need to
    ;; potentially delay these settings.  Actually worse than this
    ;; everything referred to by the KEY must be re-ified, if this is
    ;; a non-eql hash table.  As far as I can tell, while Common Lisp
    ;; allows some insanely build order dependent situations with
    ;; EQUALP hash tables, but there is no way to discover or
    ;; reproduce that at serialization or deserialization time.
    (dotimes (i hash-table-count)
      (let ((key (funcall restore-object)))
	(restore-object-to (gethash key ht) restore-object)))
    ht))
			       
			       
      
    

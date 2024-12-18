(in-package :cl-store-faster)

(defun store-hash-table (ht storage)
  (declare (optimize speed safety) (type hash-table ht))
  (maybe-store-reference-instead (ht storage)
    (when storage
      (store-ub8 +hash-table-code+ storage nil)
      (store-tagged-unsigned-integer (hash-table-count ht) storage)
      (store-tagged-unsigned-integer (hash-table-size ht) storage))
    (store-object (hash-table-test ht) storage) ;; a symbol
    (store-object (hash-table-rehash-threshold ht) storage) ;; float
    (store-object (hash-table-rehash-size ht) storage) ;; float
    #+sbcl (store-boolean (sb-ext:hash-table-synchronized-p ht) storage)
    #+sbcl (store-object (sb-ext:hash-table-weakness ht) storage)
    (maphash (lambda (k v)
	       (store-object k storage)
	       (store-object v storage))
	     ht)))

(defun restore-hash-table (storage)
  ;; These integers
  (let ((hash-table-count (restore-object storage))
	(ht
	  (with-delayed-reference
	    (let ((size (restore-object storage))
		  (test (restore-object storage))
		  (rehash-threshold (restore-object storage))
		  (rehash-size (restore-object storage))
		  #+sbcl(synchronized (restore-object storage))
		  #+sbcl(weakness (restore-object storage)))
	      ;; weakness works as far as I can discern
	      ;; because of how we do reference restoration
	      (make-hash-table :test test :size size
			       :rehash-size rehash-size
			       :rehash-threshold rehash-threshold
			       #+sbcl :synchronized #+sbcl synchronized
			       #+sbcl :weakness #+sbcl weakness)))))
    ;; the keys may not be fully reified yet, so we need to
    ;; potentially delay these settings.  Actually worse than this
    ;; everything referred to by the KEY must be re-ified, if this is
    ;; a non-eql hash table.  As far as I can tell, while Common Lisp
    ;; allows some insanely build order dependent situations with
    ;; EQUALP hash tables, but there is no way to discover or
    ;; reproduce that at serialization or deserialization time.
    (dotimes (i hash-table-count)
      (let ((key (restore-object storage)))
	(restore-object-to (gethash key ht) storage)))
    ht))
			       
			       
      
    

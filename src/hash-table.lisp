(in-package :cl-store-faster)

(defun store-hash-table (ht storage references)
  (declare (optimize speed safety) (type hash-table ht))
  (maybe-store-reference-instead (ht storage references)
    (when storage
      (store-ub8 +hash-table-code+ storage nil)
      (store-tagged-unsigned-fixnum (hash-table-count ht) storage)
      (store-tagged-unsigned-fixnum (hash-table-size ht) storage))
    (store-object (hash-table-test ht) storage references) ;; a symbol
    (store-object (hash-table-rehash-threshold ht) storage references) ;; float
    (store-object (hash-table-rehash-size ht) storage references) ;; float
    #+sbcl (store-boolean (sb-ext:hash-table-synchronized-p ht) storage)
    #+sbcl (store-object (sb-ext:hash-table-weakness ht) storage references)
    (maphash (lambda (k v)
	       (store-object k storage references)
	       (store-object v storage references))
	     ht)))

(defun restore-hash-table (storage references)
  ;; These integers
  (let ((hash-table-count (restore-object storage references))
	(ht
	  (let ((size (restore-object storage references))
		(test (restore-object storage references))
		(rehash-threshold (restore-object storage references))
		(rehash-size (restore-object storage references))
		#+sbcl(synchronized (restore-object storage references))
		#+sbcl(weakness (restore-object storage references)))
	    ;; weakness works as far as I can discern
	    ;; because of how we do reference restoration
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
      (let ((key (restore-object storage references)))
	(restore-object-to (gethash key ht) storage references)))
    ht))
			       
			       
      
    

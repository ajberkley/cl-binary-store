(in-package :cl-store-faster)

(defun store-hash-table (ht storage)
  (declare (optimize speed safety) (type hash-table ht))
  (maybe-store-reference-instead (ht storage)
    (store-ub8 +hash-table-code+ storage nil)
    (store-tagged-unsigned-integer (hash-table-count ht) storage)
    (store-tagged-unsigned-integer (hash-table-size ht) storage)
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
	      (make-hash-table :test test :size size
			       :rehash-size rehash-size
			       :rehash-threshold rehash-threshold
			       #+sbcl :synchronized #+sbcl synchronized
			       #+sbcl :weakness #+sbcl weakness)))))
    (dotimes (i hash-table-count)
      (let ((key (restore-object storage))
	    (value (restore-object storage)))
	(setf (gethash key ht) value)))
    ht))
			       
			       
      
    

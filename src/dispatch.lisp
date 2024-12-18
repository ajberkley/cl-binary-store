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
  
  (defun make-store-object ()
    `(progn
       (declaim (sb-ext:maybe-inline store-object))
       (defun store-object (value storage)
	 (etypecase value
	   ;; We need to order these by subtypep, a simple sort won't work
	   ;; because we have disjoint sets.  So first, we have to sort into
	   ;; disjoint sets, then sort, then recombine.
           ,@(strict-subtype-ordering
	      (loop for type-spec being the hash-keys of *code-store-info*
			 for func = (gethash type-spec *code-store-info*)
			 collect (list type-spec
				       (list func 'value 'storage)))
	      :key #'first)))))
  
  (defmacro eval-make-store-object ()
    `,(make-store-object))

  (eval-make-store-object)

  (defun clear-cached-tables ()
    (eval (make-store-object))
    (eval (make-read-dispatch)))
  
  (defun store-objects (storage &rest stuff)
    (let ((*references* (make-hash-table :test 'eql))
	  (*struct-info* (make-hash-table :test 'eql)))
      (dolist (elt stuff)
	(store-object elt storage))
      (flush-storage storage)))
  
  (defun restore-objects (storage)
    "Returns all the elements in storage.  If a single element just
 returns it, otherwise returns a list of all the elements."
    (let ((*references* (make-array 1000 :adjustable t :fill-pointer 0)))
      (let ((result
	      (loop for code = (restore-ub8 storage t)
		    while code
		    collect (read-dispatch code storage))))
	(if (cdr result) result (car result))))))


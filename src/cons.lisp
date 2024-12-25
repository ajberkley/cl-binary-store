(in-package :cl-store-faster)

(defparameter *support-shared-list-structures* t
  "If this is T, then circular lists of all types and structures that
 share list parts will be serialized correctly.  This is very
 expensive.  When this is NIL, only the heads of lists may be multiply
 referenced (by elements in the list itself or other objects).  When
 this is NIL, though this is similar to the behavior of CL-STORE where
 circular lists make everything explode.  If you aren't storing large
 lists this is not that expensive.  For a 1 million long list with this
 feature enabled (full tracking of all elements of the list), we are
 similar speed to CL-STORE which is not tracking all the conses.

 WARNING: it is very very bad to set this to NIL and then try to write
 out data with general list circularites.  Worst case we fill your disk,
 best case you end up with multiple copies of parts of the list.")

(declaim (inline store-cons))
(defun store-cons (cons storage eq-refs store-object)
  "This is called during the actual storage output phase."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type storage storage) (type function store-object))
  (tagbody start
     (when (referenced-already cons storage eq-refs)
       (return-from store-cons (values)))
     (storage-write-byte storage +cons-code+)
     (funcall store-object (car cons))
     (let ((cdr (cdr cons)))
       (if (consp cdr)
           (progn (setf cons cdr) (go start))
	   (funcall store-object (the (not cons) cdr))))))

(declaim (inline search-cons))
(defun search-cons (cons references store-object
		   &optional (write-new-references t)
		     (support-shared-list-structures *support-shared-list-structures*))
  "This is called during the reference counting phase"
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type function store-object))
  ;; We always record the first cons at the head of a list
  (tagbody start
     (when (check-reference cons references write-new-references)
       (return-from search-cons (values)))
     (funcall store-object (car cons))
     (let ((cdr (cdr cons)))
       (if (consp cdr)
           (progn
             (setf cons cdr write-new-references support-shared-list-structures)
             (go start))
	   (funcall store-object cdr))))
  (values))

(declaim (notinline restore-cons))
(defun restore-cons (storage restore-object)
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  (let ((first-cons (cons nil nil)))
    (loop
      with cons = first-cons
      do
	 (restore-object-to (car cons) restore-object)
	 (let ((next (restore-ub8 storage)))
	   (case next
	     (#.+cons-code+
	       (let ((next (cons nil nil)))
		 (setf (cdr cons) next)
		 (setf cons next)))
	     (#.+nil-code+
	      (setf (cdr cons) nil)
	      (return-from restore-cons first-cons))
	     (t
	      (restore-object-to (cdr cons) restore-object next)
	      (return-from restore-cons first-cons)))))))

(in-package :cl-store-faster)

(defparameter *support-shared-list-structures* t
  "If this is T, then circular lists of all types and structures that
 share list parts will be serialized correctly.  This is very
 expensive.  When this is NIL, only the heads of lists may be multiply
 referenced (by elements in the list itself or other objects).  When
 this is NIL, though this is similar to the behavior of CL-STORE where
 circular lists make everything explode.  If you aren't storing large
 lists this is not that expensive.  For a 1 million long list we are
 about 3x slower than CL-STORE when it has this feature disabled.

 WARNING: it is very very bad to set this to NIL and then try to write
 out data with general list circularites.  Worst case we fill your disk,
 best case you end up with multiple copies of parts of the list.")

(declaim (inline store-cons))
(defun store-cons (cons storage references)
  (if storage
      (store-cons! cons storage references t *support-shared-list-structures*)
      (search-cons cons references *track-references* *support-shared-list-structures*)))

(declaim (notinline store-cons))
(defun store-cons! (cons storage references
		   &optional (write-new-references t)
		     (support-shared-list-structures *support-shared-list-structures*))
  "If TAGGED is NIL, then we elide writing out the first cons tag, but
 the rest of the list will be tagged as usual.

 STORAGE can be NIL, in which case we should do no writing to it but we want to
 traverse the lists anyway to count references."
  (declare (optimize (speed 3) (safety 0)) (type storage storage))
  ;; We always record the first cons at the head of a list
  (when (check/store-reference cons storage references write-new-references)
    (return-from store-cons! (values)))
  (storage-write-byte storage +cons-code+)
  ;; (if (typep (car cons) 'fixnum)
  ;;     (store-fixnum (car cons) storage)
  (store-object/store-phase (car cons) storage references)
  (let ((cdr (cdr cons)))
    (if (consp cdr)
	(store-cons! cdr storage references support-shared-list-structures
		    support-shared-list-structures)
	(store-object/store-phase (the (not cons) cdr) storage references))))

(defun search-cons (cons references
		   &optional (write-new-references t)
		     (support-shared-list-structures *support-shared-list-structures*))
  "If TAGGED is NIL, then we elide writing out the first cons tag, but
 the rest of the list will be tagged as usual.

 STORAGE can be NIL, in which case we should do no writing to it but we want to
 traverse the lists anyway to count references."
  (declare (optimize (speed 3) (safety 0)))
  ;; We always record the first cons at the head of a list
  (when (check/store-reference cons nil references write-new-references)
    (return-from search-cons (values)))
  (store-object/ref-count-phase (car cons) references)
  (let ((cdr (cdr cons)))
    (if (consp cdr)
	(search-cons cdr references support-shared-list-structures
		    support-shared-list-structures)
	(store-object/ref-count-phase cdr references))))

(declaim (notinline restore-cons))
;; Has to be careful to not blow the stack
(defun restore-cons (storage references)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((first-cons (cons nil nil)))
    (loop
      with cons = first-cons
      do
	 (restore-object-to (car cons) storage references)
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
	      (restore-object-to (cdr cons) storage references next)
	      (return-from restore-cons first-cons)))))))

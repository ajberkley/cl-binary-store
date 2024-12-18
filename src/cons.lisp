(in-package :cl-store-faster)


;; UH OH WE BROKE CIRCULARITY DETECTION EVEN WHEN THIS IS T WITH
;; THE ADDITIONAL PASS
(defparameter *support-shared-list-structures* t
  "If this is T, then circular lists of all types and structures that
 share list parts will be serialized correctly.  This is very
 expensive.  When this is NIL, only the heads of lists may be multiply
 referenced.  When this is NIL, this is similar to the behavior of
 CL-STORE.  For a 1 million long list we are about 3x slower than CL-STORE
 when it has this feature disabled.  Basic support for sharing heads of lists
 is always enabled (so a circular list with the tail equal to the head is 
 supported even if this is NIL, but not one that loops back to its middle.")

(declaim (inline store-cons))
(defun store-cons (cons storage &optional (referrable-or-possibly-circular-p t) (tagged t))
  "If you specify REFERRABLE-OR-POSSIBLY-CIRCULAR-P NIL, then any object
 which is referenced in this cons (or in any other objects referred to
 in the car or rest of the list) must *not* refer back to this cons or
 any other directly connected cons.  This is used for things like
 array-dimensions which we know are lists of numbers, etc.  If TAGGED is
 NIL, then we elide the first cons tag, but the rest of the list will be
 tagged as usual.

 STORAGE can be NIL, in which case we should do no writing to it but we want to
 traverse the lists anyway to count references."
  (let ((support-shared-list-structures *support-shared-list-structures*))
    (tagbody
     next-cdr
       (or (and referrable-or-possibly-circular-p
		(check/store-reference cons storage))
	   (progn
	     (when storage
	       (ensure-enough-room storage 3)
	       (let ((offset (storage-offset storage))
		     (array (storage-store storage)))
		 (when tagged
		   (setf (aref array offset) +cons-code+)
		   (setf (storage-offset storage) (+ 1 offset)))))
	     (store-object (car cons) storage)
	     (let ((cdr (cdr cons)))
	       (if (not (consp cdr))
		   (store-object cdr storage)
		   (progn ;; optimize for proper lists
		     (setf tagged t cons cdr
			   referrable-or-possibly-circular-p
			   (and referrable-or-possibly-circular-p
				support-shared-list-structures))
		     (go next-cdr)))))))))

(declaim (inline restore-cons))
;; Has to be careful to not blow the stack
(defun restore-cons (storage &optional (referrable-or-possibly-circular-p t))
  (let ((first-cons (cons nil nil)))
    (loop
      with cons = first-cons
      do
	 (when referrable-or-possibly-circular-p
	   (record-reference cons))
	 (restore-object-to (car cons) storage)
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
	      (restore-object-to (cdr cons) storage)
	      (return-from restore-cons first-cons)))))))

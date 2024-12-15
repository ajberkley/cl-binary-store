(in-package :cl-store-faster)

(declaim (inline store-cons))
(defun store-cons (cons storage &optional (referrable-or-possibly-circular-p t) (tagged t))
  "If you specify REFERRABLE-OR-POSSIBLY-CIRCULAR-P NIL, then any object
 which is referenced in this cons (or in any other objects referred to
 in the car or rest of the list) must *not* refer back to this cons or
 any other directly connected cons.  This is used for things like
 array-dimensions which we know are lists of numbers, etc.  If TAGGED is
 NIL, then we elide the first cons tag, but the rest of the list will be
 tagged as usual."
  (tagbody
   next-cdr
     (or (and referrable-or-possibly-circular-p
	      (check/store-reference cons storage))
	 (progn
	   (ensure-enough-room storage 3)
	   (let ((offset (storage-offset storage))
		 (array (storage-store storage)))
	     (when tagged
	       (setf (aref array offset) +cons-code+)
	       (setf (storage-offset storage) (+ 1 offset)))
	     (store-object (car cons) storage)
	     (let ((cdr (cdr cons)))
	       (if (not (consp cdr))
		   (store-object cdr storage)
		   (progn
		     (setf tagged t cons cdr)
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

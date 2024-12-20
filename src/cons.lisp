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
(defun store-cons (cons storage references &optional (tagged t))
  "If TAGGED is NIL, then we elide writing out the first cons tag, but
 the rest of the list will be tagged as usual.

 STORAGE can be NIL, in which case we should do no writing to it but we want to
 traverse the lists anyway to count references."
  (let ((support-references-to-this-cons t)) ;; always allow references to list head
    (tagbody
     next-cdr
       (maybe-store-reference-instead (cons storage references support-references-to-this-cons)
	 (when tagged
	   (with-write-storage (storage)
	     (storage-write-byte storage +cons-code+)))
	 #+debug-csf (format t "~A CAR~%" (if storage "Analyzing" "Storing"))
	 (store-object (car cons) storage references)
	 (let ((cdr (cdr cons)))
	   (if (not (consp cdr))
	       (progn
		 #+debug-csf
		 (format t "~A non cons CDR~%" (if storage "Analyzing" "Storing"))
		 (store-object cdr storage references))
	       (progn ;; optimize for proper lists
		 #+debug-csf (format t "~A CDR~%" (if storage "Analyzing" "Storing"))
		 (setf tagged t cons cdr
		       support-references-to-this-cons
		       (and support-references-to-this-cons
			    *support-shared-list-structures*))
		 (go next-cdr))))))))

(declaim (notinline restore-cons))
;; Has to be careful to not blow the stack
(defun restore-cons (storage references)
  (let ((first-cons (cons nil nil)))
    (loop
      with cons = first-cons
      do
	 (restore-object-to (car cons) storage references)
	 (let ((next (restore-ub8 storage)))
	   #+debug-csf (format t "CDR is a ~A~%" next)
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

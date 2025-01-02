(in-package :cl-binary-store)

(defvar *support-shared-list-structures* nil
  "If this is T, then circular lists of all types and structures that
 share list parts will be serialized correctly.  This is very
 expensive.  When this is NIL, only the heads of lists may be multiply
 referenced (by elements in the list itself or other objects).  If you
 aren't storing large lists this is not that expensive.

 If you are storing complex list data (with for example the tail of
 a list connected to something other than its head, or data structures
 that share conses), then you must let/set this to T.")

;; There is one trick we play here.  If we have *track-references* t
;; and *support-shared-list-structures* nil then we can determine the
;; length of every list in advance before writing it out, and can then
;; write out the list without cons tags.  That is a
;; +finite-length-list+ versus a +cons-code+.  This saves a lot of
;; space.

(declaim (inline length/detect-dotted))
(defun length/detect-dotted (list)
  "length of a list, returns nil if the list is dotted"
  (loop for c on list
	for count fixnum from 1
	for next = (cdr c)
	unless (typep next '(or null cons))
	  do (return-from length/detect-dotted nil)
	finally (return count)))

(declaim (inline store-cons/indefinite))
(defun store-cons/indefinite (cons storage eq-refs store-object assign-new-reference-id)
  "This is called during the actual storage output phase."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type write-storage storage) (type function store-object))
  (tagbody start
     (when (referenced-already cons storage eq-refs assign-new-reference-id)
       (return-from store-cons/indefinite (values)))
     (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
       (set-sap-ref-8 sap offset +cons-code+))
     (funcall store-object (car cons))
     (let ((cdr (cdr cons)))
       (if (consp cdr)
           (progn (setf cons cdr) (go start))
	   (if (null cdr)
	       (when storage (store-nil storage))
	       (funcall store-object (the (and (not null) (not cons)) cdr)))))))

(declaim (inline store-cons/finite-length))
(defun store-cons/finite (cons storage eq-refs store-object assign-new-reference-id list-lengths)
  "This is called during the actual storage output phase if we have already computed the list
 length.  track-references is true, but not *support-shared-list-structures*"
  (declare (optimize (debug 3) safety) (type write-storage storage) (type function store-object))
  (maybe-store-reference-instead (cons storage eq-refs assign-new-reference-id)
    (let ((length (or (and list-lengths (gethash cons list-lengths))
		      (length/detect-dotted cons))))
      (when (not length)
	(return-from store-cons/finite
	  (store-cons/indefinite cons storage eq-refs store-object assign-new-reference-id)))
      (locally
	  (declare (type fixnum length))
	(with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
	  (set-sap-ref-8 sap offset +finite-length-list-code+))
	(store-tagged-unsigned-fixnum length storage)
	(dotimes (count length)
	  (cond
	    ((= count (- length 1))
	     (funcall store-object (car cons))
	     (let ((cdr (cdr cons)))
	       (if (null cdr)
		   (store-nil storage) ;; need to store nil in case last element is dotted
		   (funcall store-object (cdr cons)))))
	    (t
	     (funcall store-object (car cons))
	     (setf cons (cdr cons)))))))))

(declaim (inline search-cons/indefinite))
(defun search-cons/indefinite (cons references store-object)
  "This is only called when *track-references* is nil and *support-shared-list-structures* is nil."
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type function store-object))
  (tagbody start
     (when (check-reference cons references)
       (return-from search-cons/indefinite (values)))
     (funcall store-object (car cons))
     (let ((cdr (cdr cons)))
       (if (consp cdr)
           (progn
             (setf cons cdr)
             (go start))
	   (funcall store-object cdr)))))

(declaim (inline store-cons))
(defun store-cons (cons storage eq-refs store-object assign-new-reference-id list-lengths
		   support-shared-list-structures)
  (if support-shared-list-structures
      (store-cons/indefinite cons storage eq-refs store-object assign-new-reference-id)      
      (store-cons/finite cons storage eq-refs store-object assign-new-reference-id list-lengths)))

(declaim (inline search-cons/finite))
(defun search-cons/finite (cons references store-object list-lengths)
  "This is called during the reference counting phase and only when we have only very
 simple list circularity (CDR (LAST LIST)) -> LIST or another reference list that we
 have seen before."
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type function store-object))
  (let ((length 1)
	(head cons))
    (declare (type fixnum length))
    (unless (check-reference cons references)
      (tagbody continue
	 (setf (gethash head list-lengths) length)
	 (progn
	   (funcall store-object (car cons))
	   (setf cons (cdr cons))
	   (cond
	     ((not (consp cons))
	      (setf (gethash head list-lengths) length)
	      (funcall store-object cons))
	     (t
	      (truly-the fixnum (incf length))
	      (go continue))))))))

(declaim (inline search-cons))
(defun search-cons (cons eq-refs store-object list-lengths support-shared-list-structures)
  (if support-shared-list-structures
      (search-cons/indefinite cons eq-refs store-object)
      (search-cons/finite cons eq-refs store-object list-lengths)))

(declaim (inline restore-cons/indefinite))
(defun restore-cons/indefinite (storage restore-object)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
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
	      (return-from restore-cons/indefinite first-cons))
	     (t
	      (restore-object-to (cdr cons) restore-object next)
	      (return-from restore-cons/indefinite first-cons)))))))

(declaim (inline restore-list/known-length))
(defun restore-list/known-length (storage restore-object)
  (declare (optimize speed safety))
  (let* ((length (restore-tagged-unsigned-fixnum storage)))
    (let* ((head (cons nil nil))
	   (cons head))
      (dotimes (count length)
	(restore-object-to (car cons) restore-object)
	(if (= count (- length 1)) ;; need to support dotted end
	    (restore-object-to (cdr cons) restore-object)
	    (let ((new-cons (cons nil nil)))
	      (setf (cdr cons) new-cons)
	      (setf cons new-cons))))
      head)))

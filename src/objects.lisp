(in-package :cl-store-faster)

;; Here we deal with STRUCTURE-OBJECT and STANDARD-OBJECT

(defvar *store-class-slots* nil
  "If set / let to T, then slots in standard-objects with class allocation will
 be stored, otherwise not")

;; We store some meta-information about the structure-object or
;; standard-object We generate and store this once for each object type
;; into *struct-info* and also into the output stream.

(defparameter *struct-info* nil
  "An eql hash table which maps from structure-object or standard-class type name
 to a struct-informations structure")

(declaim (inline struct-info-type struct-info-slot-names struct-info-types))
(defstruct struct-info
  (type 'blarg :type symbol)
  (slot-names #() :type simple-vector))

(defun compute-struct-info (obj &optional (type (type-of obj)))
  (let* ((is-structure-object (typep obj 'structure-object))
	 (store-class-slots *store-class-slots*)
	 (names
	   (coerce
	    (loop for slot in (class-slots (find-class type))
		  when (or is-structure-object
			   store-class-slots
			   (not (eql (slot-definition-allocation slot) :class)))
		    collect (slot-definition-name slot))
	    'simple-vector)))
    (make-struct-info
     :type type
     :slot-names names)))

(defun store-struct-info (struct-info storage)
  (declare (optimize speed safety) (type struct-info struct-info))
  (maybe-store-reference-instead (struct-info storage)
    #+debug-csf (format t "STORE-STRUCT-INFO ~A~%" struct-info)
    (store-ub8 +struct-info-code+ storage nil)
    (store-object (struct-info-type struct-info) storage)
    (let ((slot-names (struct-info-slot-names struct-info)))
      (store-ub32 (length slot-names) storage nil)
      (loop for name across slot-names
	    do (store-object name storage)))))

(defun restore-struct-info (storage)
  (declare (optimize (debug 3)))
  (let ((si (record-reference (make-struct-info))))
    (setf (struct-info-type si) (restore-object storage))
    (let ((num-slots (restore-ub32 storage)))
      (let ((names (make-array num-slots)))
	(loop for idx fixnum from 0 below num-slots
	      do (setf (svref names idx) (restore-object storage)))
	(setf (struct-info-slot-names si) names)
	si))))
	
(defun get-struct-info (object)
  (let ((type (type-of object)))
    (or (gethash type *struct-info*)
	(setf (gethash type *struct-info*)
	      (compute-struct-info object type)))))

(defun store-struct (struct storage)
  (declare (optimize speed safety) (type structure-object struct))
  (maybe-store-reference-instead (struct storage)
    (store-ub8 +structure-object-code+ storage nil)
    (let ((struct-info (get-struct-info struct)))
      (store-struct-info struct-info storage)
      #+debug-csf (format t "store-struct dumping slot values~%")
      (loop for name across (struct-info-slot-names struct-info)
	    do (store-object (slot-value struct name) storage)))))

(defun restore-struct (storage)
  (let* (struct-info
	 struct)
    (with-delayed-reference
      (setf struct-info (restore-object storage))
      ;;(make-instance (struct-info-type struct-info)) ;; 2x slower
      (setf struct (allocate-instance (find-class (struct-info-type struct-info)))))
    (loop for name across (struct-info-slot-names struct-info)
	  do (setf (slot-value struct name) (restore-object storage)))
    struct))

;; We use the same methods as structure-objects, but must deal with unbound slots

(declaim (inline store-unbound))
(defun store-unbound (storage)
  (store-ub8 +unbound-code+ storage nil))

(declaim (inline restore-unbound))
(defun restore-unbound (storage)
  (declare (ignore storage))
  'unbound-slot)

(defun store-standard-object (obj storage)
  (declare (optimize speed safety) (type standard-object obj))
  (maybe-store-reference-instead (obj storage)
    (store-ub8 +standard-object-code+ storage nil)
    (let ((struct-info (get-struct-info obj)))
      (store-struct-info struct-info storage)
      (loop for name across (struct-info-slot-names struct-info)
	    do (if (slot-boundp obj name)
		   (store-object (slot-value obj name) storage)
		   (store-unbound storage))))))

(defun restore-standard-object (storage)
  (declare (optimize speed safety))
  (let (struct-info
	obj)
    (with-delayed-reference
      (setf struct-info (restore-object storage))
      (setf obj (allocate-instance (find-class (struct-info-type struct-info)))))
    (loop for name across (struct-info-slot-names struct-info)
	  for slot-value = (restore-object storage)
	  if (eq slot-value 'unbound-slot)
	    do
	       (slot-makunbound obj name)
	  else
	    do (setf (slot-value obj name) slot-value))
    obj))

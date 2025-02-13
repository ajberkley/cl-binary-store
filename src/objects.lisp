(in-package :cl-binary-store)

;; Here we deal with `STRUCTURE-OBJECT's and `STANDARD-OBJECT's
;; For each object type we meet, we serialize / deserialize a
;; single description of it an `object-info'.  That contains the
;; type-name and slot-values.  Locally during store and restore we
;; also keep track of any user specified extension behaviors within
;; this `object-info'.

;; During restore, we provide reasonable and comprehensive restarts
;; for missing defstruct, defclass, and slots.

(defvar *store-class-slots* nil
  "If set / let to T, then slots in standard-objects with :class allocation
 will be stored, otherwise not.")

;; We provide three extension points to customize how objects are
;; serialized and deserialized.

;; First is `SERIALIZABLE-OBJECT-INFO' which may return slot-names to
;; serialize or a function that will be called on each object and
;; slot-name to determine what to serialize for it.
;;
;; For example:
;; (defstruct blarg
;;   (a-may-contain-sensitive-info)
;;   (b-do-not-serialize))
;;
;; (defmethod serializable-object-info ((type (eql 'blarg)))
;;   (values (vector 'a-may-contain-sensitive-info) nil))
;;
;; If you use the above technique for a structure-object, you may want
;; to provide a `specialized-object-constructor' too as the
;; unserialized slots are undefined.  For a standard-object they are
;; just unbound which is fine, but for a structure-object it's
;; undefined what they are.
;;
;; Or if you want to filter out sensitive information on a per object basis:
;;
;; (defmethod serializable-object-info ((type (eql 'blarg)))
;;   (values nil
;;     (lambda (slot-name slot-value)
;;       (if (sensitive-information-p slot-name slot-value)
;;           "censored"
;;           slot-value))))

;; Second is `SPECIALIZED-OBJECT-CONSTRUCTOR' which can return a function
;; which will be used to build an object from restored slot values.  The
;; function will be called with an object-info structure and a dynamic-extent
;; simple-vector of slot values equal to the length and in the same order as
;; (object-info-slot-names object-info).

;; Warning: be careful here if you have circular references.

;; Convenient if you want to have, for example, initialize-instance called on your object
;; instead of the default allocate-instance:
;;
;; (defmethod specialized-object-constructor ((type (eql 'my-type)))
;;   (lambda (object-info slot-values)
;;     (apply #'make-instance 'my-type
;;                            (garble (object-info-slot-names object-info) slot-values))))


;; (defmethod initialize-instance :after
;;            ((obj my-object-type &rest initargs &key &allow-other-keys))
;;   ;; do something here to touch up obj or cause side effects
;;   )

;; Third is `SPECIALIZED-SERIALIZER/DESERIALIZER' which can return as
;; two values, two functions which will replace the default
;; serialization / deserialization of objects of that type.  If you do
;; this, you probably want to define a new codespace anyway, so you
;; could just do it directly with defstore / defrestore functions, but
;; there is no penalty to doing it this way.

;; The specialized-serializer function will be called with
;;  (lambda (object storage eq-refs store-object assign-new-reference-id))
;; which should have the side effect of modifying storage, eq-refs, calling store-object
;; and or assign-new-reference-id.
;; Correspondingly, the specialized-deserializer function will be called with:
;;  (lambda (storage restore-object)

(defun get-slot-names (class)
  "Return a list of slot names (symbols) skipping :class allocation slots if
 *store-class-slots* is t."
  (assert class)
  (loop with store-class-slots = *store-class-slots*
	with is-structure-object = (or (typep class 'structure-class) ;; allegro work around
				       (subtypep class 'structure-class))
	for slot in (class-slots class)
	when (or is-structure-object
		 store-class-slots
		 (not (eql (slot-definition-allocation slot) :class)))
	  collect
	#+abcl (if is-structure-object
		   (sys::dsd-name slot)
		   (slot-definition-name slot))
	#-abcl (slot-definition-name slot)))

(defgeneric serializable-object-info (type)
  (:documentation
   "Must return two values.  The first value must be a
 list of slot-names (symbols) which should be serialized for this
 object.

 The second value may be NIL or a function which will be
 called with each slot-name and slot-value and should return a
 serializable object (like nil) for that slot.")
  (:method (type)
    (get-slot-names (find-class type))))

(defgeneric specialized-object-constructor (type)
  (:documentation "May return a function that will be used to construct an object from
 an `object-info' structure and a simple vector of slot-values in the same order as
 (object-info-slot-names object-info):
  (lambda (object-info slot-values) -> object)
 Be careful in the case of circular references: it may be in that case that a slot-value
 is a `fixup', in which case you have to provide a function to be called back when the
 object is fully reified.  See restore-object-to for the logic.")
  (:method (type)
    (declare (ignorable type))
    nil))

(defgeneric specialized-serializer/deserializer (type)
  (:documentation "Returns two values, the first value is a
 function (or nil) that will be called as a:
  (lambda (object storage eq-refs store-object assign-new-reference-id))
 and as side effects should write to storage, etc.  The second value should be a function
 that has a signature (lambda (storage restore-object) -> object)")
  (:method (type)
    (declare (ignorable type))
    (values nil nil)))

(defun compute-object-info (type implicit-ref-id)
  "Takes a symbol denoting the type of an object and returns an `object-info' allowing for
 the various user methods to override behaviors."
  (declare (optimize speed safety))
  (multiple-value-bind (slot-names slot-value-filter-func)
      (serializable-object-info type)
    (let* ((slot-names-vector (coerce (the list slot-names) 'simple-vector)))
      (multiple-value-bind (specialized-serializer specialized-deserializer)
	  (specialized-serializer/deserializer type)
	(make-object-info
	 :class (find-class type)
	 :type type
	 :slot-names slot-names-vector
	 :slot-value-filter-func slot-value-filter-func
	 :specialized-constructor (specialized-object-constructor type)
	 :specialized-serializer specialized-serializer
	 :specialized-deserializer specialized-deserializer
	 :ref-id (- (the fixnum (incf (the fixnum (car implicit-ref-id))))))))))

(defmacro maybe-store-local-reference-instead ((object-info storage eql-refs) &body body)
  "Called during the serialization / storage phase.  This is a kludged
 implicit referencing scheme used when the global *track-references*
 is disabled.  This lets us avoid calculating and writing and reading
 what each object type is."
  (assert (atom storage)) ;; or rewrite to use alexandria:once-only
  (assert (atom object-info))
  (assert (atom eql-refs))
  `(unless (and ,eql-refs storage
		(cond
		  ((gethash ,object-info ,eql-refs)
		   ;; We have already stored this object-info, write a reference to it
		   (store-ub8/no-tag +object-info-code+ ,storage)
		   (store-fixnum (object-info-ref-id ,object-info) ,storage)
		   t)
		  (t
		   (setf (gethash ,object-info ,eql-refs) t)
		   nil)))
     ,@body))

(defun maybe-store-to-reference-table (object-info implicit-eql-refs)
  "Called during the deserialization / restore phase.  We store the constructed
 object-info into our reference hash table using its reference id."
  (when implicit-eql-refs
    (setf (gethash (object-info-ref-id object-info) implicit-eql-refs) object-info))
  object-info)

(defun store-object-info (object-info storage eq-refs store-object implicit-eql-refs
			  assign-new-reference-id)
  (declare (optimize speed safety) (type object-info object-info))
  (maybe-store-reference-instead (object-info storage eq-refs assign-new-reference-id)
    (maybe-store-local-reference-instead (object-info storage implicit-eql-refs)
      (let ((slot-names (object-info-slot-names object-info)))
	(when storage
	  (store-ub8/no-tag +object-info-code+ storage)
	  (store-tagged-unsigned-fixnum (length slot-names) storage))
	(store-symbol (object-info-type object-info) storage eq-refs store-object
		      assign-new-reference-id)
	(loop for name across slot-names
	      do (store-symbol name storage eq-refs store-object assign-new-reference-id))))))

(define-condition object-type-not-found (maybe-expected-error)
  ((object-info :initarg :object-info :reader object-type-not-found-object-info)))

(defmethod print-object ((obj object-type-not-found) str)
  (let ((object-info (object-type-not-found-object-info obj)))
    (format str "Class or struct of type ~S not found" (object-info-type object-info))))

(defun ask-for-new-class ()
  (format t "Enter a new class or struct type name (unquoted symbol): ")
  (list (read)))

(defun use-custom-building-function ()
  (format t "Enter a function which will build instances from slot-values~%That is with signature (lambda (object-info slot-values) constructed-object): ")
  (let ((read (read)))
    (list (eval read))))

(defun function-designator-p (thing)
  (if (symbolp thing) (symbol-function thing) (functionp thing)))

(defun signal-object-type-not-found (object-info)
  (restart-case
      (error 'object-type-not-found :object-info object-info)
    (use-different-class (new-type)
      :report "USE DIFFERENT CLASS"
      :interactive ask-for-new-class
      (setf (object-info-type object-info) new-type))
    (create-standard-object ()
      :report "CREATE STANDARD OBJECT"
      (eval `(defclass ,(object-info-type object-info) ()
		,(loop for slot-name across (object-info-slot-names object-info)
		       collect (list slot-name))))
      (finalize-inheritance (find-class (object-info-type object-info))))
    (create-structure-object ()
      :report "CREATE STRUCTURE OBJECT"
      (eval `(defstruct ,(object-info-type object-info)
	       ,@(loop for slot-name across (object-info-slot-names object-info)
		       collect (list slot-name nil))))
      (finalize-inheritance (find-class (object-info-type object-info))))))

(defun really-find-class (object-info)
  (loop for class = (setf (object-info-class object-info)
			  (find-class (object-info-type object-info) nil))
	until class
	do (signal-object-type-not-found object-info)
	finally (return class)))

(define-condition missing-slot (maybe-expected-error)
  ((slot-name :initarg :slot-name :reader missing-slot-name)
   (type :initarg :type :reader missing-slot-type)
   (data-slots :initarg :data-slots :reader missing-slot-data-slots)
   (image-slots :initarg :image-slots :reader missing-slot-image-slots)))

(defmethod print-object ((obj missing-slot) stream)
  (format stream "Missing slot ~S in ~S, data file has slots ~A, current image has slots ~A"
	  (missing-slot-name obj) (missing-slot-type obj)
	  (missing-slot-data-slots obj) (missing-slot-image-slots obj)))

(defun ask-for-new-slot-name ()
  (format t "Enter a slot-name to store value into (string or symbol): ")
  (let ((read (read)))
     (list (if (stringp read) (intern read) read))))

(defun validate-slot-names (type restored-slot-names image-slot-names)
  "If all of restored-slot-names are a subset of current-image-slot-names just
 return restored-slot-names.  If not, provide restarts to allow building of a
 work around for the user.  May return a specialized-object-constructor."
  (declare (type simple-vector restored-slot-names) (type list image-slot-names))
  (let ((ignorable-slots)
	(renamed-slots))
    (loop for slot-name across restored-slot-names
	  unless (find slot-name image-slot-names)
	    do (restart-case
		   (error 'missing-slot :slot-name slot-name :type type
					:data-slots restored-slot-names
					:image-slots image-slot-names)
		 (discard ()
		   :report "DISCARD SLOT DATA"
		   (push slot-name ignorable-slots))
		 (map-to-new-slot-name (new-slot-name)
		   :report "STORE TO DIFFERENT SLOT"
		   :interactive ask-for-new-slot-name
		   (push (cons slot-name new-slot-name) renamed-slots))))
    (when (or ignorable-slots renamed-slots)
      (lambda (object-info slot-values)
        (let* ((class (object-info-class object-info))
	       (struct (allocate-instance class)))
	  (loop for name across (object-info-slot-names object-info)
		for value in slot-values
		unless (find name ignorable-slots)
		  do (setf
		      (slot-value struct (or (cdr (assoc name renamed-slots)) name))
		      value))
	  struct)))))

(defun restore-object-info (storage restore-object implicit-eql-refs implicit-ref-id)
  (declare (optimize speed safety) (type function restore-object))
  (let* ((num-slots (restore-tagged-fixnum storage)))
    (if (< num-slots 0) ; it's a reference id, look it up in our implicit tracking table
	(gethash num-slots implicit-eql-refs)
        (progn
          (if (> num-slots (ash most-positive-fixnum -3))
              (unexpected-data "too many slots in object-info" num-slots)
              (check-if-too-much-data (read-storage-max-to-read storage) (* 8 num-slots)))
          (let ((slot-name-vector (make-array num-slots))
	        (type (funcall restore-object))
	        (ref-id (- (the fixnum (incf (the fixnum (car implicit-ref-id)))))))
            (unless (symbolp type)
              (unexpected-data "expected a symbol"))
	    ;; No circularity possible below as these are symbols
	    (loop for idx fixnum from 0 below num-slots
		  do (setf (svref slot-name-vector idx) (funcall restore-object)))
	    (multiple-value-bind (specialized-serializer specialized-deserializer)
	        (specialized-serializer/deserializer type)
	      (declare (ignore specialized-serializer))
	      (let ((specialized-constructor (specialized-object-constructor type)))
	        (cond
		  ((or specialized-constructor specialized-deserializer)
		   (maybe-store-to-reference-table
		    (make-object-info :class nil :type type
				      :specialized-constructor specialized-constructor
				      :specialized-deserializer specialized-deserializer
				      :slot-names slot-name-vector
				      :ref-id ref-id)
		    implicit-eql-refs))
		  (t
		   (let* ((si (maybe-store-to-reference-table
			       (make-object-info
			        :type type :slot-names slot-name-vector :ref-id ref-id)
			       implicit-eql-refs))
			  (class (really-find-class si)))
		     (setf (object-info-class si) class)
		     (setf (object-info-type si) (class-name class))
		     (let ((image-slot-names (get-slot-names class)))
		       ;; Now validate that the slot-names we restored are a subset of
		       ;; those in the current image object	    
		       (setf (object-info-specialized-constructor si)
			     (validate-slot-names type slot-name-vector image-slot-names))
		       ;; The order of slot names may be different, use the order
		       ;; stored in the file!
		       (setf (object-info-slot-names si) slot-name-vector))
		     si))))))))))
	
(defun get-object-info (object object-info implicit-ref-id)
  (let ((type (type-of object)))
    (or (gethash type object-info)
	(setf (gethash type object-info)
	      (compute-object-info type implicit-ref-id)))))

(declaim (inline store-unbound))
(defun store-unbound (storage)
  (store-ub8/no-tag +unbound-code+ storage))

(declaim (inline restore-unbound))
(defun restore-unbound ()
  'unbound-slot)

(defun store-standard/structure-object
    (obj storage eq-refs store-object assign-new-reference-id is-standard-object object-info
     implicit-eql-refs implicit-ref-id)
  (declare (optimize speed safety) (type (or structure-object standard-object condition) obj))
  (maybe-store-reference-instead (obj storage eq-refs assign-new-reference-id)
    (when storage
      (store-ub8/no-tag +standard/structure-object-code+ storage))
    (let* ((object-info (get-object-info obj object-info implicit-ref-id))
	   (object-info-specialized-serializer (object-info-specialized-serializer object-info)))
      (cond
	(object-info-specialized-serializer
	 (funcall object-info-specialized-serializer
		  obj storage eq-refs store-object assign-new-reference-id))
	(t
	 (store-object-info object-info storage eq-refs store-object implicit-eql-refs
			    assign-new-reference-id)
	 (let ((filter (object-info-slot-value-filter-func object-info)))
	   (declare (type function store-object))
	   (if is-standard-object
	       (loop for name across (object-info-slot-names object-info)
		     do (if (slot-boundp obj name)
			    (let* ((value (slot-value obj name))
				   (filtered-value (if filter (funcall filter name value) value)))
			      (funcall store-object filtered-value))
			    (when storage (store-unbound storage))))
	       (loop for name across (object-info-slot-names object-info) ;; structure-object
		 for value = (slot-value obj name)
		 for filtered-value = (if filter (funcall filter name value) value)
		 do (funcall store-object filtered-value)))))))))

(defun (setf slot-value*) (value object name)
  "Handle internal 'unbound-slot value"
  (if (eq value 'unbound-slot)
      (slot-makunbound object name)
      (setf (slot-value object name) value)))

(defun restore-standard/structure-object (storage restore-object)
  (declare (type function restore-object) (ignorable storage) (optimize speed safety))
  (let ((object-info (funcall restore-object)))
    (unless (object-info-p object-info)
      (unexpected-data "expected an object-info"))
    (let* ((specialized-deserializer (object-info-specialized-deserializer object-info))
	   (constructor (object-info-specialized-constructor object-info)))
      (cond
        (specialized-deserializer
         (funcall specialized-deserializer storage restore-object))
        (constructor
         (let* ((slot-names (object-info-slot-names object-info))
	        (num-slots (length slot-names))
	        (slot-values (make-list num-slots)))
	   (declare (dynamic-extent slot-values) (type (unsigned-byte 16) num-slots))
	   (loop for value on slot-values
	         do (setf (car value) (funcall restore-object)))
	   (funcall constructor object-info slot-values)))
        (t
         (let* ((class (object-info-class object-info))
	        (obj (allocate-instance class)))
	   (if (typep class 'structure-class)
	       (loop for name across (object-info-slot-names object-info)
		     do (restore-object-to (slot-value obj name) restore-object))
	       (loop for name across (object-info-slot-names object-info)
		     do (restore-object-to (slot-value* obj name) restore-object)))
	   obj))))))

(in-package :cl-binary-store)
;; We store some meta-information about the structure-object or
;; standard-object We generate and store this once for each object type
;; into *class-info* and also into the output stream.

(declaim (inline object-info-class object-info-slot-names object-info-type
		 object-info-specialized-constructor
		 object-info-slot-value-filter-func
		 object-info-specialized-serializer
		 object-info-specialized-deserializer
		 object-info-use-initialize-instance
		 object-info-ref-id
		 make-object-info))
(defstruct object-info
  (class (find-class 'structure-object))
  (slot-names #() :type simple-vector)
  (type 'object-info :type symbol)
  (slot-value-filter-func nil :type (or null function))
  (use-initialize-instance nil :type boolean)
  (specialized-constructor nil :type (or null function))
  (specialized-serializer nil :type (or null function))
  (specialized-deserializer nil :type (or null function))
  (ref-id (when *eql-refs-ref-id* ;; ref-id is always negative number
	    (- (the fixnum (incf (the fixnum *eql-refs-ref-id*))))) :type (or null fixnum)))

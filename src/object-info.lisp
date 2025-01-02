(in-package :cl-binary-store)

;; We store some meta-information about the structure-object or
;; standard-object.  We store part of this information (slot-names and
;; type into the serialization stream... only once though, with either
;; an explicit reference scheme if *track-references* is T or an implicit
;; tracking scheme that uses object-info-ref-id and overloads the length
;; of the slot-name vector as a reference id.)

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
  (ref-id nil :type (or null (and fixnum (integer * -1)))))

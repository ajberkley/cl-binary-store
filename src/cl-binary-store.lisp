(defpackage :cl-binary-store
  (:use :common-lisp
	#+sbcl #:sb-mop
	#+allegro #:mop
	#+abcl #:mop
	#+lispworks #:clos
	#+clasp #:clos
	#+ecl #:clos
	#+ccl #:ccl)
  (:documentation "A package that exports tools used inside cl-binary-store for use by
 someone writing their own specialized serialization or deserialization routine.")
  (:export
   #:register-code

   ;; Complex circularity handling during restore
   #:restore-object-to

   ;; Normal circularity handling
   #:record-reference ; during restore
   #:check/store-reference ; during store

   #:store-boolean
   #:store-t
   #:store-nil
   
   #:store-ub8 ; critically used for writing tag ids
   #:store-ub16
   #:store-ub32
   #:store-fixnum
   #:store-tagged-unsigned-fixnum
   #:store-tagged-unsigned-integer

   #:store-double-float
   #:restore-double-float
   #:restore-double-float-to
   #:store-single-float
   #:restore-single-float
   
   #:store-ratio
   #:restore-ratio
   #:store-bignum
   #:restore-bignum
   #:store-complex
   #:restore-complex
   #:store-complex-single-float
   #:restore-complex-single-float
   #:store-complex-double-float
   #:restore-complex-double-float

   #:store-array
   #:restore-array

   #:store-simple-specialized-array
   #:restore-simple-specialized-array
   #:store-simple-specialized-vector
   #:restore-simple-specialized-vector

   #:store-string
   #:store-string/no-refs
   #:restore-string
   
   #:store-simple-vector
   #:restore-simple-vector

   #:store-standard/structure-object
   #:restore-standard/structure-object

   #:slot-info
   #:slot-info-class
   #:slot-info-slot-names
   #:slot-info-call-initialize-instance
   #:make-slot-info
   #:compute-slot-info
   
   #:ensure-enough-room-to-write
   #:storage-store
   #:storage-offset
   #:storage-max
   #:storage-size

   #:action
   #:action-code

   #:with-pinned-objects
   #:vector-sap

      ;; General user interface
   #:store
   #:restore
   ;; Serializing to / from sbcl specific raw memory SAP
   #:store-to-sap
   #:restore-from-sap
   #:replace-store-sap-buffer
   #:out-of-space
   ;; Streams
   #:restore-from-stream
   #:store-to-stream
   ;; In memory ub8 vectors
   #:restore-from-vector
   #:store-to-vector
   #:store-to-extant-vector
   #:out-of-space-in-fixed-vector

   #:out-of-data
   #:store-to-file
   #:restore-from-file
   ;; Support complex circular lists
   #:*support-shared-list-structures*
   ;; Do any sort of reference tracking
   #:*track-references*
   ;; Write an end marker
   #:*write-end-marker*

   ;; Versioning
   #:*write-version*
   #:*version-being-read*
   #:*write-magic-number*
   #:*current-codespace*

   #:out-of-space-current-offset
   #:out-of-space-wanted-bytes

   ;; Adding a new type
   #:defstore
   #:defrestore
   #:storage
   #:obj
   #:store-object
   #:restore-object
   #:rebuild-dispatch
   #:make-end-marker
   #:with-write-storage
   #:storage-write-byte
   #:storage-write-byte!
   #:copy-sap
   #:ensure-enough-data
   #:copy-n-bytes
   #:storage-sap
   #:eq-refs
   #:double-float-refs
   #:num-eq-refs
   #:define-codespace
   #:+action-code+
   #:store-action&
   #:restore-action&
   #:references
   #:+basic-codespace+
   #:*read-version*
   #:store-symbol
   #:restore-symbol
   #:assign-new-reference-id
   #:*eq-refs-table-size*
   #:*double-float-refs-table-size*
   #:*num-eq-refs-table-size*

   ;; Error if symbol package does not exist and handling the case
   #:missing-package-during-restore
   #:change-package
   #:create-package

   ;; Structure-object or standard-object type does not exist during restore
   #:object-type-not-found
   #:object-type-not-found-object-info
   ;; restarts
   #:create-structure-object
   #:create-standard-object
   #:use-different-class

   ;; Missing slots during restore
   #:missing-slot
   #:missing-slot-slot-name
   #:missing-slot-type
   #:missing-slot-data-slots
   #:missing-slot-image-slots
   ;; Restarts
   #:discard
   #:map-to-new-slot-name

   ;; A parameter to specialized-object-constructor is an object-info
   #:object-info
   #:object-info-slot-names
   #:object-info-type

   ;; Extensions for modifying object serialization
   #:serializable-object-info
   #:specialized-object-constructor
   #:specialized-serializer/deserializer
   #:delete-restore
   #:delete-store
   #:delete-codespace
   #:*codespaces*
   #:*allow-codespace-switching*))


(in-package :cl-binary-store)

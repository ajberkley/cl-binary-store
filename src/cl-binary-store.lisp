(defpackage :cl-binary-store
  (:use :common-lisp
	#+sbcl #:sb-mop
	#+allegro #:mop
	#+abcl #:mop
	#+lispworks #:clos
	#+clasp #:clos
	#+ecl #:clos
	#+ccl #:ccl)
  (:import-from #:alexandria #:once-only)
  (:documentation "A package that exports tools used inside cl-binary-store for use by
 someone writing their own specialized serialization or deserialization routine.")
  (:export
   ;; Complex circularity handling during restore
   #:restore-object-to

   ;; Normal circularity handling
   #:check/store-reference ; during store

   #:store-boolean
   #:store-t
   #:store-nil
   #:store-ub8/tag
   #:store-ub8/no-tag   
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

   #+sbcl #:store-simple-specialized-array
   #+sbcl #:restore-simple-specialized-array
   #+sbcl #:store-simple-specialized-vector
   #+sbcl #:restore-simple-specialized-vector

   #:store-string
   #:store-string/no-refs
   #:restore-string
   
   #:store-simple-vector
   #:restore-simple-vector

   #:store-standard/structure-object
   #:restore-standard/structure-object

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
   #:*output-end-marker*

   ;; Versioning
   #:*write-version*
   #:*version-being-read*
   #:*output-magic-number*
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
   #:make-end-marker

   ;; Low level stuff for serializing/deserializing data from read-storage-sap and write-storage-sap
   #:set-sap-ref-double
   #:sap-ref-double
   #:set-sap-ref-single
   #:sap-ref-single
   #:set-signed-sap-ref-64
   #:signed-sap-ref-64
   #:set-sap-ref-64
   #:sap-ref-64
   #:set-sap-ref-32
   #:sap-ref-32
   #:set-sap-ref-16
   #:sap-ref-16
   #:set-sap-ref-8
   #:sap-ref-8
   #:ensure-enough-data
   #:with-write-storage
   #:write-storage-offset
   #:read-storage-offset
   #:write-storage-sap
   #:read-storage-sap
   #:write-storage-store
   #:read-storage-store
   #:copy-sap

   ;; Basic codespace names
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
   #:implicit-eql-refs
   #:implicit-ref-id

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

   ;; Codespace manipulations
   #:delete-restore
   #:delete-store
   #:delete-codespace
   #:*codespaces*
   #:*allow-codespace-switching*
   #:*max-to-write*
   #:*max-to-read*
   #:*output-magic-number*

   ;; Conditions
   #:invalid-input-data
   #:too-much-data
   #:maybe-expected-error))


(in-package :cl-binary-store)

(define-condition invalid-input-data (simple-error)
  ())

(defun unexpected-data (expected &optional (data nil data-provided-p))
  (error 'invalid-input-data :format-control "Expected ~A~A" :format-arguments (list expected (if data-provided-p (format nil ", found ~A" data) ""))))

(define-condition maybe-expected-error (invalid-input-data)
  ()
  (:documentation "Things like MISSING-PACKAGE-DURING-RESTORE, MISSING-SLOT"))

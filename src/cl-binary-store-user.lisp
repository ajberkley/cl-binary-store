(defpackage #:cl-binary-store-user
  (:use #:common-lisp #:cl-binary-store)
  (:documentation "A Common Lisp serialization/deserialization library
 for Common Lisp objects to a binary format.  Optimized for speed and
 lightly for flexibility.  It is about 10x faster than cl-store and
 produces reasonably compact files (plug it into a gzip stream if you
 want more).  Currently only works on SBCL.

 Out of the box we support reading/writing from/to streams,
 reading/writing from/to in-memory vectors, reading/writing to raw
 memory.

 Extending this with your own serializer / deserializer is
 straightforward as it is expected that the default standard-object
 and structure-object serialization may not meet everyones needs.

 All number types are supported, but we provide specialized compact writers for:
  ub8, ub16, ub32, ub64, fixnum, single-float, double-float
  (complex double-float) (complex single-float)
 
 All array types are supported.

 On SBCL we provide fast and compact serialization of vectors and simple-arrays of:
  bit        (simple-bit-vector in 1D or multi-dimensional)
  base-char  (simple-base-string in 1D or multi-dimensional)
  character  (simple-string in 1D or multi-dimensional)
  single-float
  double-float
  fixnum
  signed-byte: 8 16 32 64
  unsigned-byte: 2 4 7 8 15 16 31 32 62 64")
  (:export
   ;; General user interface
   #:store
   #:restore
   ;; Serializing to / from sbcl specific raw memory SAP
   #:store-to-sap
   #:restore-from-sap
   #:replace-store-sap-buffer
   #:out-of-space
   #:out-of-space-current-offset
   #:out-of-space-wanted-bytes
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
   #:*current-codespace*
   #:*write-version*
   #:*version-being-read*
   #:*output-magic-number*

   ;; Safety rails
   #:*max-to-write*
   #:*max-to-read*

   ;; Fun
   #:*load/save-progress-indicator*
   
   ;; Error if symbol package does not exist
   #:missing-package-during-restore
   ;; Restarts to recover from the above error
   #:create-package
   #:change-package

   ;; Hinting how many objects you will want to track
   #:*eq-refs-table-size*
   #:*num-eq-refs-table-size*
   #:*double-float-refs-table-size*

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
   ))

(in-package #:cl-binary-store-user)
  

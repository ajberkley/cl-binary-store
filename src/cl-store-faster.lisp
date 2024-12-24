(defpackage :cl-store-faster
  (:use :common-lisp
        #+sbcl #:sb-mop
	#+allegro #:mop
	#+abcl #:mop
	#+lispworks #:clos
	#+clasp #:clos
	#+ecl #:clos
	)
  (:documentation "Package containing all internal and extension symbols of cl-store-faster")
  (:export
   ;; User interface
   #:store
   #:restore
   ;; Some specific bits
   #:restore-from-stream
   #:store-to-stream
   #:restore-from-vector
   #:store-to-vector
   #:store-to-extant-vector
   #:end-of-data
   #:store-to-file
   #:restore-from-file
   ;; Support complex circular lists
   #:*support-shared-list-structures*

   #:make-output-storage/stream
   #:ensure-enough-room-to-write
   #:storage-store
   #:storage-offset
   #:flush-storage
   #:*write-magic-number*
   #:*write-version*
   #:magic-number
   #:magic-number-number
   #:*version-being-read*
   #:*supported-versions*
   #:store-to-sap
   #:restore-from-sap
   #:out-of-space
   #:out-of-space-current-offset
   #:out-of-space-wanted-bytes
   #:replace-store-sap-buffer
   #:*track-references*))

(in-package :cl-store-faster)

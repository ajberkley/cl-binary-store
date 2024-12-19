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
   #:restore-from-stream
   #:store-to-stream
   #:restore-from-vector
   #:store-to-vector
   #:end-of-data
   #:store-to-file
   #:restore-from-file
   ;; Support complex circular lists
   #:*support-shared-list-structures*

   #:make-output-storage/stream
   #:ensure-enough-room-to-write
   #:storage-store
   #:storage-offset
   #:flush-storage))

(in-package :cl-store-faster)

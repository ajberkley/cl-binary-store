(in-package :cl-store-faster)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *code-store-info* (make-hash-table :test 'equalp))
  (defparameter *code-restore-info* (make-array 256 :initial-element nil))

  ;; meh, need maybe keys or a better way of doing this.
  (defun register-code (code restore &optional store type)
    (declare (type (or symbol function) restore)
	     (type (or null symbol function) store)
	     (type (unsigned-byte 8) code))
    (when store
      (when type
	(unless (or (null (gethash type *code-store-info*))
		    (eql (gethash type *code-store-info*) store))
	  (cerror "OVERRIDE-STORE" (format nil "Store function for ~A already exists" type)))
	(setf (gethash type *code-store-info*) store)))
    (unless (or (null (svref *code-restore-info* code))
		(eql (svref *code-restore-info* code) restore))
      (cerror "OVERRIDE" (format nil "Restore code ~A already exists!" code)))
    (setf (svref *code-restore-info* code) restore)
    code))

;; NUMBERS
(defconstant +ub8-code+ (register-code 0 'restore-ub8 'store-ub8 '(unsigned-byte 8)))
(defconstant +ub16-code+ (register-code 1 'restore-ub16 'store-ub16 '(unsigned-byte 16)))
(defconstant +ub32-code+ (register-code 2 'restore-ub32 'store-ub32 '(unsigned-byte 32)))
(defconstant +sb8-code+ (register-code 3 'restore-sb8 'store-sb8 '(integer -255 0)))
(defconstant +sb16-code+ (register-code 4 'restore-sb16 'store-sb16 '(integer -65535 0)))
(defconstant +sb32-code+ (register-code 5 'restore-sb32 'store-sb32 '(integer -4294967295 0)))
(defconstant +fixnum-code+ (register-code 6 'restore-fixnum 'store-fixnum 'fixnum))
(defconstant +bignum-code+ (register-code 7 'restore-bignum 'store-bignum 'bignum))
(defconstant +single-float-code+ (register-code 8 'restore-single-float 'store-single-float
						'single-float))
(defconstant +double-float-code+ (register-code 9 'restore-double-float 'store-double-float
						'double-float))
(defconstant +ratio-code+ (register-code 10 'restore-ratio 'store-ratio 'ratio))
(defconstant +complex-code+ (register-code 11 'restore-complex 'store-complex 'complex))
;; STRUCTURE-OBJECTS (defstruct) and STANDARD-CLASS (defclass)
(defconstant +structure-object-code+ (register-code 12 'restore-struct 'store-struct 'structure-object))
(defconstant +standard-object-code+
  (register-code 13 'restore-standard-object 'store-standard-object  'standard-object))
;; LISTS
(defconstant +cons-code+ (register-code 14 'restore-cons 'store-cons 'cons))
;; SYMBOLS
(defconstant +nil-code+ (register-code 15 'restore-nil 'store-boolean 'null))
(defconstant +t-code+ (register-code 16 'restore-t 'store-boolean '(eql t)))
(defconstant +symbol-code+ (register-code 17 'restore-symbol 'store-symbol 'symbol))
(defconstant +gensym-code+ (register-code 18 'restore-gensym))
;; REFERENCES
(defconstant +referrer-ub8-code+ (register-code 19 'restore-referrer-ub8))
(defconstant +referrer-ub16-code+ (register-code 20 'restore-referrer-ub16))
(defconstant +referrer-ub32-code+ (register-code 21 'restore-referrer-ub32))
(defconstant +referrer-code+ (register-code 22 'restore-referrer))
(defconstant +record-reference-ub8-code+ (register-code 23 'restore-reference-id-ub8))
(defconstant +record-reference-ub16-code+ (register-code 24 'restore-reference-id-ub16))
(defconstant +record-reference-ub32-code+ (register-code 25 'restore-reference-id-ub32))
(defconstant +record-reference-code+ (register-code 26 'restore-reference-id))
;; SIMPLE VECTORS
#+sbcl
(defconstant +simple-specialized-vector+
  (register-code 27 'restore-simple-specialized-vector 'store-simple-specialized-vector
		 '(simple-array * (*))))

(defconstant +simple-vector+
  (register-code 28 'restore-simple-vector 'store-simple-vector 'simple-vector))
;; SIMPLE ARRAYS
#+sbcl
(defconstant +simple-specialized-array+
  (register-code 29 'restore-simple-specialized-array 'store-simple-specialized-array
		 '(and (simple-array * *) (not (simple-array t *)))))

;; COMPLEX VECTORS AND ARRAYS
(defconstant +array+ (register-code 30 'restore-array 'store-array 'array))

(defconstant +complex-double-float-code+
  (register-code 31 'restore-complex-double-float 'store-complex-double-float
		 '(complex double-float)))

(defconstant +complex-single-float-code+
  (register-code 32 'restore-complex-single-float 'store-complex-single-float
		 '(complex single-float)))

(defconstant +struct-info-code+
  (register-code 33 'restore-struct-info 'store-struct-info 'struct-info))

(defconstant +unbound-code+ (register-code 34 'restore-unbound 'store-unbound))

(defconstant +pathname-code+ (register-code 35 'restore-pathname 'store-pathname 'pathname))

(defconstant +hash-table-code+ (register-code 36 'restore-hash-table 'store-hash-table 'hash-table))

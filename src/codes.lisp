(in-package :cl-store-faster)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Maps code -> code-info or type -> code-info
  (defparameter *code-info* (make-hash-table :test 'equalp))

  (defstruct code-info
    (code 255 :type (unsigned-byte 8))
    (restore-func-name 'error :type symbol)
    (store-func-name 'error :type symbol)
    (type nil)
    (store-references nil)
    (restore-references nil))
  ;; meh, need maybe keys or a better way of doing this.
  (defun register-code (code &key restore store type store-references restore-references)
    (declare (type (or symbol function) restore)
	     (type (or null symbol function) store)
	     (type (unsigned-byte 8) code))
    (let ((code-info (make-code-info
		      :code code
		      :restore-func-name restore
		      :store-func-name store
		      :type type
		      :store-references store-references
		      :restore-references restore-references)))
      (when type
	(unless (or (null (gethash type *code-info*))
		    (equalp (gethash type *code-info*) code-info))
	  (cerror "OVERRIDE-STORE" (format nil "Store function for ~A already exists" type)))
	(setf (gethash type *code-info*) code-info))
      (unless (or (null (gethash code *code-info*))
		  (equalp (gethash code *code-info*) code-info))
	(cerror "OVERRIDE-STORE" (format nil "Restore function for ~A already exists" type)))
      (setf (gethash code *code-info*) code-info))
    code))

;; NUMBERS
(defconstant +ub8-code+ (register-code 0 :restore 'restore-ub8 :store 'store-ub8
				       :type '(unsigned-byte 8) :store-references nil))
(defconstant +ub16-code+ (register-code 1 :restore 'restore-ub16 :store 'store-ub16
					:type '(unsigned-byte 16) :store-references nil))
(defconstant +ub32-code+ (register-code 2 :restore 'restore-ub32 :store 'store-ub32
					:type '(unsigned-byte 32) :store-references nil))
(defconstant +sb8-code+ (register-code 3 :restore 'restore-sb8 :store 'store-sb8
					 :type '(integer -255 0) :store-references nil))
(defconstant +sb16-code+ (register-code 4 :restore 'restore-sb16 :store 'store-sb16
					  :type '(integer -65535 0) :store-references nil))
(defconstant +sb32-code+ (register-code 5 :restore 'restore-sb32 :store 'store-sb32
					:type '(integer -4294967295 0) :store-references nil))
(defconstant +fixnum-code+ (register-code 6 :restore 'restore-fixnum :store 'store-fixnum
					  :type 'fixnum :store-references nil))
(defconstant +bignum-code+ (register-code 7 :restore 'restore-bignum :store 'store-bignum
					  :type 'bignum :store-references :number))
(defconstant +single-float-code+ (register-code 8 :restore 'restore-single-float
						  :store 'store-single-float
						  :type 'single-float :store-references nil))
(defconstant +double-float-code+ (register-code 9 :restore 'restore-double-float
						  :store 'store-double-float
						  :type 'double-float
						  :store-references :number
						  :restore-references nil))
(defconstant +ratio-code+ (register-code 10 :restore 'restore-ratio :store 'store-ratio
					    :type 'ratio :store-references :number
					    :restore-references :number))
(defconstant +complex-code+ (register-code 11 :restore 'restore-complex :store 'store-complex
					      :type 'complex :store-references :number
					      :restore-references :number))
;; STRUCTURE-OBJECTS (defstruct) and STANDARD-CLASS (defclass)
(defconstant +structure-object-code+
  (register-code 12 :restore 'restore-struct :store 'store-struct
		    :type 'structure-object :store-references t :restore-references t))
(defconstant +standard-object-code+
  (register-code 13 :restore 'restore-standard-object :store 'store-standard-object
		 :type 'standard-object :store-references t :restore-references t))
;; LISTS
(defconstant +cons-code+ (register-code 14 :restore 'restore-cons :store 'store-cons
					:type 'cons :store-references t :restore-references t))
;; SYMBOLS
(defconstant +nil-code+ (register-code 15 :restore 'restore-nil :store 'store-boolean
				       :type 'null :store-references nil))
(defconstant +t-code+ (register-code 16 :restore 'restore-t :store 'store-boolean
				     :type'(eql t) :store-references nil))
(defconstant +symbol-code+ (register-code 17 :restore 'restore-symbol :store 'store-symbol
					     :type 'symbol :store-references t
					     :restore-references t))
(defconstant +gensym-code+ (register-code 18 :restore 'restore-gensym))
;; REFERENCES
(defconstant +referrer-ub8-code+ (register-code 19 :restore 'restore-referrer-ub8
						:store-references t :restore-references t))
(defconstant +referrer-ub16-code+ (register-code 20 :restore 'restore-referrer-ub16
						 :store-references t :restore-references t))
(defconstant +referrer-ub32-code+ (register-code 21 :restore 'restore-referrer-ub32
						 :store-references t :restore-references t))
(defconstant +referrer-code+ (register-code 22 :restore 'restore-referrer
					       :store-references t :restore-references t))
(defconstant +record-reference-ub8-code+ (register-code 23 :restore 'restore-reference-id-ub8
							:store-references t :restore-references t))
(defconstant +record-reference-ub16-code+ (register-code 24 :restore 'restore-reference-id-ub16
							 :store-references t :restore-references t))
(defconstant +record-reference-ub32-code+ (register-code 25 :restore 'restore-reference-id-ub32
							 :store-references t :restore-references t))
(defconstant +record-reference-code+ (register-code 26 :restore 'restore-reference-id
						    :store-references t :restore-references t))
;; SIMPLE VECTORS
#+sbcl
(defconstant +simple-specialized-vector+
  (register-code 27 :restore 'restore-simple-specialized-vector
		    :store 'store-simple-specialized-vector
		    :type '(simple-array * (*))
		    :store-references t))

(defconstant +simple-vector+
  (register-code 28 :restore 'restore-simple-vector :store 'store-simple-vector
		 :type 'simple-vector :store-references t :restore-references t))
;; SIMPLE ARRAYS
#+sbcl
(defconstant +simple-specialized-array+
  (register-code 29 :restore 'restore-simple-specialized-array
		    :store 'store-simple-specialized-array
		    :type '(and (simple-array * *) (not (simple-array t *)))
		    :store-references t))

;; COMPLEX VECTORS AND ARRAYS
(defconstant +array+ (register-code 30 :restore 'restore-array :store 'store-array
				    :type 'array :store-references t :restore-references t))

(defconstant +complex-double-float-code+
  (register-code 31 :restore 'restore-complex-double-float :store 'store-complex-double-float
		 :type '(complex double-float) :store-references t))

(defconstant +complex-single-float-code+
  (register-code 32 :restore 'restore-complex-single-float :store 'store-complex-single-float
		 :type '(complex single-float) :store-references t))

(defconstant +slot-info-code+
  (register-code 33 :restore 'restore-slot-info :store 'store-slot-info
		 :type 'slot-info :store-references t :restore-references t))

(defconstant +unbound-code+ (register-code 34 :restore 'restore-unbound :store 'store-unbound))

(defconstant +pathname-code+
  (register-code 35 :restore 'restore-pathname :store 'store-pathname :type 'pathname
		    :store-references t :restore-references t))

(defconstant +hash-table-code+
  (register-code 36 :restore 'restore-hash-table :store 'store-hash-table :type 'hash-table
		    :store-references t :restore-references t))

(defconstant +simple-base-string-code+
  (register-code 37 :restore 'restore-simple-base-string :store 'store-simple-base-string
		    :type 'simple-base-string
		    :store-references t))

(defconstant +simple-string-code+
  (register-code 38 :restore 'restore-simple-string :store 'store-simple-string
		    :type 'simple-string
		    :store-references t))

(defconstant +keyword-code+ (register-code 39 :restore 'restore-keyword :store 'store-keyword
					     :type 'keyword :store-references t
					      :restore-references nil))

(defconstant +action-code+ (register-code 40 :restore 'restore-action& 
					     :store 'store-action&
					     :type 'action
					     :restore-references t
					     :store-references t))

;; On sbcl a condition is neither a structure-object nor a standard-object
#+sbcl (defconstant +condition-object+ (register-code 41 :restore 'restore-standard-object
                                                  :store 'store-standard-object
                                                  :type 'condition
                                                  :restore-references t
                                                  :store-references t))

(in-package :cl-binary-store)

;; This codespace uses codes 0 to 32 and reserves 192 to 255 for the
;; user to extend things (those have the two highest bits set).  Codes
;; between 34 and 43 are small integer codes, and codes from 44 to 63
;; are direct encoded references.  Reference codes from 64 to 127 are
;; 6 bits plus the next 8 bits read.  Then codes from 128-191 (highest
;; bit set) are 6 lowest bits plus the next 16 bits read.  This gives
;; us a codespace of 25 million references.  We do not distinguish the
;; first instance of a reference --- the state in the references
;; vector tells us whether or not we have seen an object before --- if
;; we haven't, we assign the next object to the reference id.  This
;; means that parallel restore isn't really practical in this
;; codespace.  One would have to shrink down to a 5 bit direct
;; reference codespace for that... which might be good enough?  Anyhow,
;; easy to fixup with a different codespace.

;; TODO Handle the encoding of -5 to 5.
;; TODO Handle the reference encoding

(defconstant +basic-codespace+ #x0001)

(defconstant +ub8-code+ 0)
(defconstant +ub16-code+ 1)
(defconstant +ub32-code+ 2)
(defconstant +fixnum-code+ 3)
(defconstant +cons-code+ 4)
(defconstant +nil-code+ 5)
(defconstant +sb8-code+ 6)
(defconstant +sb16-code+ 7)
(defconstant +sb32-code+ 8)
(defconstant +bignum-code+ 9)
(defconstant +single-float-code+ 10)
(defconstant +double-float-code+ 11)
(defconstant +double-float-zero-code+ 12)
(defconstant +ratio-code+ 13)
(defconstant +complex-code+ 14)
(defconstant +complex-double-float-code+ 15)
(defconstant +complex-single-float-code+ 16)
(defconstant +symbol-code+ 17)
(defconstant +uninterned-symbol-code+ 18)
(defconstant +structure-object-code+ 19)
(defconstant +standard-object-code+ 20)
(defconstant +t-code+ 21)
(defconstant +simple-specialized-vector-code+ 22)
(defconstant +simple-vector-code+ 23)
(defconstant +simple-specialized-array-code+ 24)
(defconstant +array-code+ 25)
(defconstant +slot-info-code+ 26)
(defconstant +unbound-code+ 27)
(defconstant +pathname-code+ 28)
(defconstant +hash-table-code+ 29)
(defconstant +simple-base-string-code+ 30)
(defconstant +simple-string-code+ 31)
(defconstant +action-code+ 32)
(defconstant +tagged-reference-code+ 33) ;; for more than 4194322 references!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline decode-reference-direct))
  (defun decode-reference-direct (raw-8-bit)
    (- raw-8-bit 43))

  (declaim (inline decode-reference-one-byte))
  (defun decode-reference-one-byte (tag-byte next-byte)
    ;;(format t "Tag byte is ~A, next-byte is ~A~%" tag-byte next-byte)
    (+ 19 (- tag-byte #x40) (ash next-byte 6)))

  (declaim (inline decode-reference-two-bytes))
  (defun decode-reference-two-bytes (tag-byte next-16-bits)
    ;;(format t "Tag byte is ~A, next-16-bits is ~A~%" tag-byte next-16-bits)
    (+ 16403 (- tag-byte #x80) (ash next-16-bits 6))))

(define-codespace ("basic codespace" +basic-codespace+)
  (register-references num-eq-refs (make-hash-table :test #'eq)
		       :priority 3)
  (register-references double-float-refs (make-hash-table :test #'double-float-=)
		       :priority 2)
  (register-references eq-refs (make-hash-table :test #'eq) :priority 1)
  (register-references symbol-refs (make-hash-table :test #'eq) :never-disable t
		       :priority 0)

  (defstore fixnum (store-fixnum obj storage) :call-during-reference-phase nil)
  (defrestore +ub8-code+ (restore-ub8 storage))
  (defrestore +ub16-code+ (restore-ub16 storage))
  (defrestore +ub32-code+ (restore-ub32 storage))
  (defrestore +fixnum-code+ (restore-fixnum storage))
  (defrestore +sb8-code+ (restore-sb8 storage))
  (defrestore +sb16-code+ (restore-sb16 storage))
  (defrestore +sb32-code+ (restore-sb32 storage))

  (defstore bignum (store-bignum obj storage) :check-for-ref-in num-eq-refs
    :call-during-reference-phase nil)
  (defrestore +bignum-code+ (restore-bignum storage))
  
  (defstore single-float (store-single-float obj storage) :call-during-reference-phase nil)
  (defrestore +single-float-code+ (restore-single-float storage))
  
  (defstore double-float (store-double-float obj storage double-float-refs))
  (defrestore +double-float-code+ (restore-double-float storage))
  (defrestore +double-float-zero-code+ (restore-double-float-zero))
  
  (defstore ratio (store-ratio obj storage num-eq-refs))
  (defrestore +ratio-code+ (restore-ratio restore-object))
  
  (defstore complex (store-complex obj storage store-object) :check-for-ref-in num-eq-refs)
  (defrestore +complex-code+ (restore-complex restore-object))
  (defrestore +complex-double-float-code+ (restore-complex-double-float storage))
  (defrestore +complex-single-float-code+ (restore-complex-single-float storage))

  ;; CONS
  
  (defstore cons
      (store-cons obj storage eq-refs store-object)
    :call-during-reference-phase (search-cons obj eq-refs store-object))
  
  (defrestore +cons-code+ (restore-cons storage restore-object))
  
  ;; T and NIL (STORED DISJOINT FROM SYMBOLS)
  
  (defstore null (store-nil storage) :call-during-reference-phase nil)
  (defrestore +nil-code+ (restore-nil))
  (defstore (eql t) (store-t storage) :call-during-reference-phase nil)
  (defrestore +t-code+ (restore-t))

  ;; INTERNED SYMBOLS / KEYWORDS / UNINTERNED SYMBOLS
  (defstore (and symbol (not null) (not (eql t))) (store-symbol obj storage eq-refs store-object))
  (defrestore +symbol-code+ (restore-symbol storage restore-object))
  (defrestore +uninterned-symbol-code+ (restore-uninterned-symbol storage))
  
  ;; STRUCTURE-OBJECTS (defstruct) and STANDARD-CLASS (defclass)
  (defstore structure-object (store-struct obj storage eq-refs store-object))
  (defstore standard-object (store-standard-object obj storage eq-refs store-object))
  (defrestore +structure-object-code+ (restore-struct restore-object))
  (defrestore +standard-object-code+ (restore-standard-object restore-object))

  ;; REFERENCES
  (defrestore (and (> code 33) (<= code 43)) (- code 38)) ;; direct integer encoding -4 .. 5
  ;; 44 - 63 directly coded references (0 .. 19)
  (defrestore (and (> code 43) (< code 63))
      (restore-reference (decode-reference-direct code) references restore-object))
  ;; 64 - 127 14 bit references (tag byte plus another byte)
  ;; Handles references from 19 to 16402
  (defrestore (logtest code #b01000000)
      (restore-reference (decode-reference-one-byte code (restore-ub8 storage))
			 references restore-object))
  ;; 128 - 191 22 bit references (tag byte plus another two bytes)
  ;; Handles references from 16403 to 4194322
  (defrestore (logtest code #b10000000)
      (restore-reference (decode-reference-two-bytes code (restore-ub16 storage))
			 references
			 restore-object))
  (defrestore +tagged-reference-code+ (restore-reference (funcall restore-object)
							 references
							 restore-object))

  ;; SIMPLE VECTORS
  #+sbcl
  (defstore (simple-array * (*)) (store-simple-specialized-vector obj storage) :check-for-ref-in eq-refs)
  (defrestore +simple-specialized-vector-code+ (restore-simple-specialized-vector storage))
  
  (defstore simple-vector (store-simple-vector obj storage store-object) :check-for-ref-in eq-refs)
  (defrestore +simple-vector-code+ (restore-simple-vector storage restore-object))
  
  ;; SIMPLE ARRAYS
  #+sbcl
  (defstore (and (simple-array * *) (not (simple-array t *)))
      (store-simple-specialized-array obj storage) :check-for-ref-in eq-refs :call-during-reference-phase nil)
  (defrestore +simple-specialized-array-code+ (restore-simple-specialized-array storage))
  
  ;; COMPLEX VECTORS AND ARRAYS
  (defstore array (store-array obj storage eq-refs store-object))
  (defrestore +array-code+ (restore-array storage restore-object))
  
  (defstore slot-info (store-slot-info obj storage eq-refs store-object))
  (defrestore +slot-info-code+ (restore-slot-info storage restore-object))
  
  ;; UNBOUND MARKER
  (defrestore +unbound-code+ (restore-unbound))
  
  ;; PATHNAMES
  (defstore pathname (store-pathname obj store-object)
    :check-for-ref-in eq-refs :write-phase-code +pathname-code+)
  (defrestore +pathname-code+ (restore-pathname restore-object))
  
  ;; HASH-TABLE
  (defstore hash-table (store-hash-table obj storage store-object) :check-for-ref-in eq-refs)
  (defrestore +hash-table-code+ (restore-hash-table storage restore-object))
  
  ;; STRINGS
  ;; I made a mess of internal dispatch around this so have to clean it up to take advantage of check-for-ref-in.
  ;; TODO clean this up
  (defstore simple-base-string (store-simple-base-string obj storage) :check-for-ref-in eq-refs
    :call-during-reference-phase nil)
  (defrestore +simple-base-string-code+ (restore-simple-base-string storage))
  
  (defstore simple-string (store-simple-string obj storage) :check-for-ref-in eq-refs
    :call-during-reference-phase nil)
  (defrestore +simple-string-code+ (restore-simple-string storage))
  
  (defstore action (store-action& obj storage store-object))
  (defrestore +action-code+ (restore-action& storage references restore-object))
  
  ;; On sbcl a condition is neither a structure-object nor a standard-object, but has slots and all
  #+sbcl
  (defstore condition (store-standard-object obj storage eq-refs store-object)))

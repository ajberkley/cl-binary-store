(in-package :cl-binary-store)

#+allegro
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1)
		     (space 0) (debug 0) (compilation-speed 0))))

(defvar *eq-refs-table-size* 7
  "A hint for the starting size of the object tracking hash table used for most objects")
(defvar *double-float-refs-table-size* 7
  "A hint for the starting size of the double float tracking reference table")
(defvar *num-eq-refs-table-size* 7
  "A hint for the starting size of the hash table tracking misc number types
 (complex, ratios, bignums)")

(defconstant +basic-codespace+ #x0001
  "This is the basic codespace of cl-binary-store.")

(define-codespace ("basic codespace" +basic-codespace+)
  (register-references num-eq-refs (make-hash-table :test #'eq :size *num-eq-refs-table-size*))
  (register-references
   double-float-refs (make-hash-table :test #+sbcl #'double-float-= #-sbcl #'eql
				      :size *double-float-refs-table-size*))
  (register-references eq-refs (make-hash-table :test #'eq :size *eq-refs-table-size*))
  (register-store-state list-lengths (make-hash-table :test #'eq))
  (register-store-state support-shared-list-structures
			(progn
			  (when *support-shared-list-structures*
			    (assert *track-references* nil
				    "To use *support-shared-list-structures* you must have ~
                                    *track-references* t"))
			  *support-shared-list-structures*))

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
  
  (defstore double-float
      (store-double-float obj storage double-float-refs assign-new-reference-id))
  (defrestore +double-float-code+ (restore-double-float storage))
  (defrestore +double-float-zero-code+ (restore-double-float-zero))
  
  (defstore ratio (store-ratio obj storage num-eq-refs assign-new-reference-id))
  (defrestore +ratio-code+ (restore-ratio restore-object))
  
  (defstore complex (store-complex obj storage store-object) :check-for-ref-in num-eq-refs)
  (defrestore +complex-code+ (restore-complex restore-object))
  (defrestore +complex-double-float-code+ (restore-complex-double-float storage))
  (defrestore +complex-single-float-code+ (restore-complex-single-float storage))

  ;; CONS
  
  (defstore cons (store-cons obj storage eq-refs store-object assign-new-reference-id
			     list-lengths support-shared-list-structures)
    :call-during-reference-phase (search-cons obj eq-refs store-object list-lengths
					      support-shared-list-structures))
  
  (defrestore +cons-code+ (restore-cons/indefinite storage restore-object))
  (defrestore +finite-length-list-code+ (restore-list/known-length storage restore-object))
  
  ;; T and NIL (STORED DISJOINT FROM SYMBOLS)
  
  (defstore null (store-nil storage) :call-during-reference-phase nil)
  (defrestore +nil-code+ (restore-nil))
  (defstore (eql t) (store-t storage) :call-during-reference-phase nil)
  (defrestore +t-code+ (restore-t))

  ;; INTERNED SYMBOLS / KEYWORDS / UNINTERNED SYMBOLS
  (defstore (and symbol (not null) (not (eql t)))
      (store-symbol obj storage eq-refs store-object assign-new-reference-id))
  (defrestore +symbol-code+ (restore-symbol storage restore-object))
  (defrestore +uninterned-symbol-code+ (restore-uninterned-symbol storage))
  
  ;; STRUCTURE-OBJECTS (defstruct) and STANDARD-CLASS (defclass)
  ;; We use two defstore lines to help with the typecase dispatch
  
  (defstore structure-object (store-standard/structure-object
			      obj storage eq-refs store-object assign-new-reference-id nil))
  (defstore standard-object (store-standard/structure-object
			     obj storage eq-refs store-object assign-new-reference-id t))
  ;; On sbcl a condition is neither a structure-object nor a standard-object
  #+sbcl
  (defstore condition (store-standard/structure-object
		       obj storage eq-refs store-object assign-new-reference-id t))
  
  (defrestore +standard/structure-object-code+
      (restore-standard/structure-object storage restore-object))

  ;; REFERENCES
  ;; direct integer encoding [-16 16] in the tag byte
  (defrestore (<= +first-small-integer-code+ code +last-small-integer-code+)
      (- code +small-integer-zero-code+))
  ;; small refs in the tag byte from [1 30]
  (defrestore (<= +first-direct-reference-id-code+ code +last-direct-reference-id-code+)
      (restore-reference (decode-reference-direct code) references))
  ;; 64 - 127 14 bit references (tag byte plus another byte)
  (defrestore (<= +first-one-byte-reference-id-code+ code +last-one-byte-reference-id-code+)
      (restore-reference (decode-reference-one-byte code (restore-ub8 storage)) references))
  ;; 128 - 191 22 bit references (tag byte plus another two bytes)
  (defrestore (<= +first-two-byte-reference-id-code+ code +last-two-byte-reference-id-code+)
      (restore-reference (decode-reference-two-bytes code (restore-ub16 storage)) references))
  ;; Anything more than that uses a fully tagged integer code subtracting off
  (defrestore +tagged-reference-code+
      (restore-reference (decode-reference-tagged (funcall restore-object)) references))
  (defrestore +new-reference-indicator-code+
      (restore-new-reference-indicator references restore-object))

  ;; SIMPLE VECTORS
  #+sbcl
  (defstore (simple-array * (*)) (store-simple-specialized-vector obj storage) :check-for-ref-in eq-refs)
  #+sbcl
  (defrestore +simple-specialized-vector-code+ (restore-simple-specialized-vector storage))
  
  (defstore simple-vector (store-simple-vector obj storage store-object) :check-for-ref-in eq-refs)
  (defrestore +simple-vector-code+ (restore-simple-vector storage restore-object))
  
  ;; SIMPLE ARRAYS
  #+sbcl
  (defstore (and (simple-array * *) (not (simple-array t *)))
      (store-simple-specialized-array obj storage) :check-for-ref-in eq-refs :call-during-reference-phase nil)
  #+sbcl
  (defrestore +simple-specialized-array-code+ (restore-simple-specialized-array storage))
  
  ;; COMPLEX VECTORS AND ARRAYS
  (defstore array (store-array obj storage eq-refs store-object assign-new-reference-id))
  (defrestore +array-code+ (restore-array storage restore-object))
  
  (defstore object-info (store-object-info obj storage eq-refs store-object assign-new-reference-id))
  (defrestore +object-info-code+ (restore-object-info storage restore-object))
  
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
  
  (defstore simple-string (store-simple-string obj storage)
    :check-for-ref-in eq-refs
    :call-during-reference-phase nil)
  (defrestore +simple-string-code+ (restore-simple-string storage))
  
  (defstore action (store-action& obj storage store-object))
  (defrestore +action-code+ (restore-action& storage references restore-object)))

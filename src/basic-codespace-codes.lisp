(in-package :cl-binary-store)

;; USER CODES START AT 225 and end at 255
;; [0 33] for various objects
;; [34 63] for small reference ids coded in the tag byte
;; [64 127] for 14 bit reference codes stored in the tag byte and one additional byte
;; [128 191] for 22 bit reference codes stored in the tag byte and two additional bytes
;; [192 224] for small integers [-16 16]
;; [225 255] for user codes

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
(defconstant +standard/structure-object-code+ 19)
(defconstant +t-code+ 20)
(defconstant +simple-specialized-vector-code+ 21)
(defconstant +simple-vector-code+ 22)
(defconstant +simple-specialized-array-code+ 23)
(defconstant +array-code+ 24)
(defconstant +object-info-code+ 25)
(defconstant +unbound-code+ 26)
(defconstant +pathname-code+ 27)
(defconstant +hash-table-code+ 28)
(defconstant +simple-base-string-code+ 29)
(defconstant +simple-string-code+ 30)
(defconstant +action-code+ 31
  "A request to perform an action.  Used for checking codespace versions and for
 updating reference vector size and for marking the end of data")
(defconstant +tagged-reference-code+ 32
  "A reference to an object")
(defconstant +new-reference-indicator-code+ 33
  "Note that the next object to be read should be assigned the next consecutive reference id")
(defconstant +first-direct-reference-id-code+ 34
  "[34 63] used for direct reference codes (tag only) ref id [1 30]")
(defconstant +last-direct-reference-id-code+ 63)
(defconstant +first-one-byte-reference-id-code+ 64
  "[64 127] used for the tag + one byte reference codes (6 bits)")
(defconstant +last-one-byte-reference-id-code+ 127)
(defconstant +first-two-byte-reference-id-code+ 128
  "[128 191] used for the two byte codes (6 bits)")
(defconstant +last-two-byte-reference-id-code+ 191)
(defconstant +first-small-integer-code+ 192
  "[192 224] is used for small signed integers [-16 16]")
(defconstant +small-integer-zero-code+ 208)
(defconstant +last-small-integer-code+ 224)
(defconstant +first-user-code+ (+ 1 +last-small-integer-code+))
(defconstant +last-user-code+ 255)

(defconstant +maximum-untagged-unsigned-integer+ (- +last-small-integer-code+
						    +small-integer-zero-code+))
(defconstant +minimum-untagged-signed-integer+ (- +first-small-integer-code+
						  +small-integer-zero-code+))

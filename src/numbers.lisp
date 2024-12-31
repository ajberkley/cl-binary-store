(in-package :cl-binary-store)

;; See also unsigned-bytes.lisp for smaller unsigned numbers

(declaim (inline store-sb8))
(defun store-sb8 (sb8 storage &optional (tag +sb8-code+))
  "Store an (integer -255 0) value SB8 to STORAGE.  Set TAG to NIL if your
 deserializer will know this is a SB8 value and the value will be written
 without a tag byte.  It's a bit odd to have (integer -255 0) but the dispatch
 cost is negligible.... the tag byte is the sign bit."
  (declare (optimize speed safety) (type (integer -255 0) sb8))
  (store-ub8 (- sb8) storage tag))

(declaim (notinline store-sb16))
(defun store-sb16 (sb16 storage &optional (tag +sb16-code+))
  (declare (optimize speed safety) (type (integer -65535 0) sb16))
  (store-ub16 (- sb16) storage tag))

(declaim (notinline store-sb32))
(defun store-sb32 (sb32 storage &optional (tag +sb32-code+))
  (declare (optimize speed safety) (type (integer -4294967295 0) sb32))
  (store-ub32 (- sb32) storage tag))

(declaim (inline restore-sb8))
(defun restore-sb8 (storage)
  "Despite the name, restore an (integer -255 0) value from storage
 that has previously been stored by STORE-SB8"
  (- (restore-ub8 storage)))

(declaim (notinline restore-sb16))
(defun restore-sb16 (storage)
  "Restore an (integer -65535 0) value from storage that has previously
 been stored by STORE-SB16."
  (- (restore-ub16 storage)))

(declaim (notinline restore-sb32))
(defun restore-sb32 (storage)
  "Restore an (integer -4294967295 0) value from storage that has previously
 been stored by STORE-UB32."
  (- (restore-ub32 storage)))

(declaim (notinline restore-fixnum))
(defun restore-fixnum (storage)
  (declare (optimize (speed 3) (safety 0)))
  (the (values fixnum &optional) (storage-read-sb64 storage)))

(declaim (inline store-only-fixnum))
(defun store-only-fixnum (fixnum storage &optional (tag +fixnum-code+))
  (declare #-debug-cbs (optimize (speed 3) (safety 0)) (type fixnum fixnum)
	   (type (or null storage) storage))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 9 8) :sap sap)
    (when tag
      (storage-write-byte! storage tag :offset offset :sap sap)
      (incf offset))
    (storage-write-sb64! storage fixnum :offset offset :sap sap)))

;; Bignum code is based on code from the CL-STORE package which is
;; Copyright (c) 2004 Sean Ross
;; and follows the MIT license
;; and can be found at https://cl-store.common-lisp.dev/
(defun num->bits (num)
  (loop for val = (abs num) then (ash val -32)
        for count from 0
        until (zerop val)
        collect (logand val #XFFFFFFFF) into bits
        finally (return (values bits count))))

(defun bits->num (bits)
  (loop with sum = 0
        for pos from 0 by 32
        for bit in bits
        finally (return sum)
        :do (incf sum (* bit (expt 2 pos)))))

(defun restore-bignum (storage)
  (declare (optimize speed safety))
  (let ((count (restore-tagged-fixnum storage)))
    (declare (type fixnum count))
    (ensure-enough-data storage (the fixnum (* 4 (abs count))))
    (* (if (< count 0) -1 1)
       (bits->num
	(loop repeat (the fixnum (abs count))
	      collect (restore-ub32 storage))))))

(defun store-bignum (bignum storage)
  (with-write-storage (storage)
    (storage-write-byte storage +bignum-code+)
    (multiple-value-bind (ub32s count)
	(num->bits bignum)
      (declare (type fixnum count))
      (store-fixnum (if (minusp bignum) (- count) count) storage)
      (dolist (ub32 ub32s) (store-ub32 ub32 storage nil)))))

(declaim (inline restore-single-float))
(defun restore-single-float (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 4)
  (let ((offset (storage-offset storage))
        (sap (storage-sap storage)))
    (setf (storage-offset storage) (+ 4 offset))
    (sap-ref-single sap offset)))

(declaim (inline store-single-float))
(defun store-single-float (single-float storage &optional (tag t))
  (declare (optimize speed safety) (type single-float single-float))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 5 4) :sap sap)
    (when tag
      (storage-write-byte! storage +single-float-code+ :offset offset :sap sap)
      (incf offset))
    (setf (sap-ref-single sap offset) single-float)))

(declaim (inline restore-double-float))
(defun restore-double-float (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 8)
  (let ((offset (storage-offset storage))
        (sap (storage-sap storage)))
    (setf (storage-offset storage) (+ 8 offset))
    (sap-ref-double sap offset)))

(declaim (inline restore-double-float-zero))
(defun restore-double-float-zero ()
  0d0)

(defmacro restore-double-float-to (slot storage)
  "Restore a double-float to an object that is not
 constructed yet."
  (assert (atom storage))
  `(progn
     (ensure-enough-data ,storage 8)
     (let ((offset (storage-offset ,storage))
           (sap (storage-sap ,storage)))
       (setf (storage-offset ,storage) (+ 8 offset))
       (setf ,slot (sap-ref-double sap offset)))))

(declaim (inline store-double-float))
(defun store-double-float (double-float storage double-float-refs assign-new-reference-id
			   &optional (tag t))
  (declare (optimize speed safety) (type double-float double-float))
  ;; We de-duplicate double-floats as there is no visible way to
  ;; determine this from common lisp, and it saves space in the image
  ;; and on disk if there are repeated numbers (like 0d0).
  (if (= double-float 0d0)
      (when storage
        (storage-write-byte storage +double-float-zero-code+))
      (maybe-store-reference-instead (double-float storage double-float-refs
				      assign-new-reference-id)
        (with-write-storage (storage :offset offset :reserve-bytes (if tag 9 8) :sap sap)
          (when tag
	    (storage-write-byte! storage +double-float-code+ :offset offset :sap sap)
	    (incf offset))
	  (setf (sap-ref-double sap offset) double-float)))))

(defun restore-ratio (restore-object)
  (declare (optimize speed safety) (type function restore-object))
  (/ (the integer (funcall restore-object))
     (the integer (funcall restore-object))))

(defun store-ratio (ratio storage num-eq-refs assign-new-reference-id)
  "Nominally we don't need to do references here, but if someone has two bignums and takes
 a ratio of them, we don't want to store the bignums twice."
  (declare (optimize speed safety))
  (maybe-store-reference-instead (ratio storage num-eq-refs assign-new-reference-id)
    (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
      (storage-write-byte! storage +ratio-code+ :offset offset :sap sap))
    (labels ((store-integer (integer)
               (if (typep integer 'fixnum)
                   (store-fixnum integer storage)
                   (store-bignum integer storage))))
      (store-integer (numerator ratio))
      (store-integer (denominator ratio)))))

(defun restore-complex (restore-object)
  (declare (type function restore-object))
  (complex (funcall restore-object)
	   (funcall restore-object)))

(defun store-complex (complex storage store-object)
  (declare (type complex complex) (type (or null storage) storage))
  (typecase complex
    ;; We do not try to match double-floats in complex numbers to others... (except 0d0)
    ((complex double-float) (store-complex-double-float complex storage))
    ((complex single-float) (store-complex-single-float complex storage))
    (t
     (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
       (storage-write-byte! storage +complex-code+ :offset offset :sap sap))
     (locally (declare (type function store-object))
       ;; We know it's a number, but overhead is small
       (funcall store-object (realpart complex))
       (funcall store-object (imagpart complex))))))

(declaim (inline restore-complex-double-float))
(defun restore-complex-double-float (storage)
  (declare (optimize speed safety))
  (complex (restore-double-float storage)
	   (restore-double-float storage)))

(declaim (inline store-complex-double-float))
(defun store-complex-double-float (complex-double-float storage)
  (declare (optimize speed safety) (type (complex double-float) complex-double-float))
  (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
    (storage-write-byte! storage +complex-double-float-code+ :offset offset :sap sap))
  (store-double-float (realpart complex-double-float) storage nil nil nil)
  (store-double-float (imagpart complex-double-float) storage nil nil nil))

(declaim (inline restore-complex-single-float))
(defun restore-complex-single-float (storage)
  (declare (optimize speed safety))
  (complex (restore-single-float storage)
	   (restore-single-float storage)))

(declaim (inline store-complex-single-float))
(defun store-complex-single-float (complex-single-float storage)
  (declare (optimize speed safety) (type (complex single-float) complex-single-float))
  (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
    (storage-write-byte! storage +complex-single-float-code+ :sap sap :offset offset))
  (store-single-float (realpart complex-single-float) storage nil)
  (store-single-float (imagpart complex-single-float) storage nil))

(defconstant +maximum-untagged-unsigned-integer+ (- +last-small-integer-code+
						    +small-integer-zero-code+))
(defconstant +minimum-untagged-signed-integer+ (- +first-small-integer-code+
						  +small-integer-zero-code+))

(declaim (inline store-tagged-unsigned-fixnum))
(defun store-tagged-unsigned-fixnum (fixnum storage)
  "Store and tag a number from 0 to max-positive-fixnum.
 You can call `restore-tagged-unsigned-fixnum' to restore it (or restore-object)"
  (declare (type fixnum fixnum) (optimize speed safety) (type (or null storage) storage))
  (when storage
    (if (<= fixnum +maximum-untagged-unsigned-integer+)
	(store-ub8 (+ fixnum +small-integer-zero-code+) storage nil)
	(if (< fixnum 256)
            (store-ub8 fixnum storage)
            (if (< fixnum 65536)
		(store-ub16 fixnum storage)
		(if (< fixnum #.(expt 2 32))
	            (store-ub32 fixnum storage)
	            (store-only-fixnum fixnum storage)))))))

(declaim (ftype (function (storage) (values fixnum &optional)) restore-tagged-unsigned-fixnum))
(defun restore-tagged-unsigned-fixnum (storage)
  "Read back a number written by `store-tagged-unsigned-fixnum'."
  (let ((tag (restore-ub8 storage)))
    (if (<= +small-integer-zero-code+ tag +last-small-integer-code+)
	(- tag +small-integer-zero-code+)
	(ecase tag
	  (#.+ub8-code+ (restore-ub8 storage))
	  (#.+ub16-code+ (restore-ub16 storage))
	  (#.+ub32-code+ (restore-ub32 storage))
	  (#.+fixnum-code+ (restore-fixnum storage))))))

(declaim (ftype (function (storage) (values fixnum &optional)) restore-tagged-fixnum))
(defun restore-tagged-fixnum (storage)
  "Read back a number written by `store-tagged-unsigned-fixnum'."
  (let ((tag (restore-ub8 storage)))
    (if (<= +first-small-integer-code+ tag +last-small-integer-code+)
	(- tag +small-integer-zero-code+)
	(ecase tag
	  (#.+ub8-code+ (restore-ub8 storage))
	  (#.+ub16-code+ (restore-ub16 storage))
	  (#.+ub32-code+ (restore-ub32 storage))
	  (#.+fixnum-code+ (restore-fixnum storage))
	  (#.+sb8-code+ (restore-sb8 storage))
	  (#.+sb16-code+ (restore-sb16 storage))
	  (#.+sb32-code+ (restore-sb32 storage))))))

(declaim (inline store-fixnum))
(defun store-fixnum (fixnum storage)
  "Store and tag a fixnum; "
  (declare (type fixnum fixnum) (optimize speed safety) (type (or null storage) storage))
  (when storage
    (if (>= fixnum 0)
        (store-tagged-unsigned-fixnum fixnum storage)
	(if (>= fixnum +minimum-untagged-signed-integer+)
	    (store-ub8 (+ fixnum +small-integer-zero-code+) storage nil)
            (if (> fixnum -256)
		(store-sb8 fixnum storage)
		(if (> fixnum -65536)
                    (store-sb16 fixnum storage)
                    (if (> fixnum #.(- (expt 2 32)))
			(store-sb32 fixnum storage)
			(store-only-fixnum fixnum storage))))))))

(declaim (inline store-tagged-unsigned-integer))
(defun store-tagged-unsigned-integer (integer storage)
  "Store and tag any unsigned integer.  For restoring this, call restore-object as this may
 be a bignum.  Prefer `store-tagged-unsigned-fixnum' if you are sure this isn't a bignum"
  (if (typep integer 'fixnum)
      (when storage
	(store-tagged-unsigned-fixnum integer storage))
      (store-bignum integer storage)))

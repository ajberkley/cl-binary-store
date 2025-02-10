(in-package :cl-binary-store)


#+allegro
(eval-when (:compile-toplevel)
  (setf declared-fixnums-remain-fixnums-switch t)
  (declaim (optimize (speed 3) (safety 0)
		     (space 0) (debug 0) (compilation-speed 0))))

;; See also unsigned-bytes.lisp for smaller unsigned numbers

(declaim (inline store-sb8))
(defun store-sb8 (sb8 storage &optional (tag +sb8-code+))
    "Store an (integer -255 0) value SB8 to STORAGE.  Set TAG to NIL if your
 deserializer will know this is a SB8 value and the value will be written
 without a tag byte.  It's a bit odd to have (integer -255 0) but the dispatch
 cost is negligible.... the tag byte is the sign bit."
  (declare (optimize (speed 3) (safety 1)) (type (integer -255 0) sb8)
	   (type write-storage storage))
  (let ((ub8 (- sb8)))
    (if tag
	(with-write-storage (storage :offset offset :reserve-bytes 2 :sap sap)
	  (set-sap-ref-16 sap offset (+ tag (ash ub8 8))))
	(with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
	  (set-sap-ref-8 sap offset ub8)))))

(declaim (notinline store-sb16))
(defun store-sb16 (sb16 storage &optional (tag +sb16-code+))
  (declare (optimize (speed 3) (safety 1)) (type (integer -65535 0) sb16))
  (store-ub16 (- sb16) storage tag))

(declaim (notinline store-sb32))
(defun store-sb32 (sb32 storage &optional (tag +sb32-code+))
  (declare (optimize (speed 3) (safety 1)) (type (integer -4294967295 0) sb32))
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

(declaim (inline store-only-fixnum))
(defun store-only-fixnum (fixnum storage &optional (tag +fixnum-code+))
  (declare #-debug-cbs (optimize (speed 3) (safety 0)) (type fixnum fixnum)
	   (type (or null write-storage) storage))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 9 8) :sap sap)
    (when tag
      (set-sap-ref-8 sap offset tag)
      (incf offset))
    (set-signed-sap-ref-64 sap offset fixnum)))

(declaim (inline store-tagged-unsigned-fixnum))
(defun store-tagged-unsigned-fixnum (fixnum storage)
  "Store and tag a number from 0 to max-positive-fixnum.
 You can call `restore-tagged-unsigned-fixnum' to restore it (or restore-object).  Do
 not call this except during the actual storage phase."
  (declare (optimize (speed 3) (safety 1))
	   (type fixnum fixnum) (type write-storage storage))
  (if (<= fixnum +maximum-untagged-unsigned-integer+)
      (store-ub8/no-tag (truly-the fixnum (+ fixnum +small-integer-zero-code+)) storage)
      (if (< fixnum 256)
          (store-ub8/tag (truly-the fixnum fixnum) storage)
          (if (< fixnum 65536)
	      (store-ub16 fixnum storage)
	      (if (< fixnum #.(expt 2 32))
	          (store-ub32 fixnum storage)
	          (store-only-fixnum fixnum storage))))))

(declaim (inline store-tagged-unsigned-fixnum/interior))
(defun store-tagged-unsigned-fixnum/interior (fixnum storage)
  "Use this paired with restore-tagged-unsigned-fixnum if you are inside another tagged
 region for storing unsigned numbers.  Somewhat more dense as we only need tags 0-3 for
 tagging unsigned numbers."
  (declare (type fixnum fixnum) (type write-storage storage))
  (if (<= fixnum +interior-coded-max-integer+)
      (store-ub8/no-tag (+ +first-direct-unsigned-integer-interior-code+ fixnum)
			storage) ;; direct coded
      (let ((fixnum (- fixnum +interior-coded-max-integer+ 1))) ;; code shifted
	(if (< fixnum 256)
            (store-ub8/tag (truly-the fixnum fixnum) storage)
            (if (< fixnum 65536)
		(store-ub16 fixnum storage)
		(if (< fixnum #.(expt 2 32))
	            (store-ub32 fixnum storage)
	            (store-only-fixnum fixnum storage)))))))

(declaim (inline restore-fixnum))
(defun restore-fixnum (storage)
  (declare (optimize (speed 3) (safety 0)))
  (ensure-enough-data storage 8)
  (let* ((offset (read-storage-offset storage))
	 (fixnum (signed-sap-ref-64 (read-storage-sap storage) offset)))
    (unless (typep fixnum 'fixnum)
      (unexpected-data "expected fixnum" fixnum))
    (setf (read-storage-offset storage) (truly-the fixnum (+ offset 8)))
    (truly-the fixnum fixnum)))

(declaim (inline store-fixnum))
(defun store-fixnum (fixnum storage)
  "Store and tag a fixnum; if inside another tag"
  (declare (type fixnum fixnum) (optimize (speed 3) (safety 1)) (type write-storage storage))
  (if (>= fixnum 0)
      (store-tagged-unsigned-fixnum fixnum storage)
      (if (>= fixnum +minimum-untagged-signed-integer+)
	  (store-ub8/no-tag (+ fixnum +small-integer-zero-code+) storage)
          (if (> fixnum -256)
	      (store-sb8 fixnum storage)
	      (if (> fixnum -65536)
                  (store-sb16 fixnum storage)
                  (if (> fixnum #.(- (expt 2 32)))
		      (store-sb32 fixnum storage)
		      (store-only-fixnum fixnum storage)))))))

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
  (declare (optimize (speed 3) (safety 1)))
  (let* ((count (restore-tagged-fixnum storage))
         (num-words (abs count)))
    (declare (type fixnum count))
    (unless (<= num-words (ash most-positive-fixnum -2))
      (unexpected-data "number of words in bignum" num-words))
    (check-if-too-much-data (read-storage-max-to-read storage)
                            (truly-the fixnum (* 4 num-words)))
    (let ((sum 0))
      (loop
        repeat num-words
        for pos from 0 by 32
        do
           (ensure-enough-data storage 4)
           (incf sum (* (restore-ub32 storage) (expt 2 pos))))
      (* (if (< count 0) -1 1) sum))))

(defun store-bignum (bignum storage)
  (when storage
    (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
      (set-sap-ref-8 sap offset +bignum-code+))
    (multiple-value-bind (ub32s count)
	(num->bits bignum)
      (declare (type fixnum count))
      (store-fixnum (if (minusp bignum) (- count) count) storage)
      (dolist (ub32 ub32s) (store-ub32 ub32 storage nil)))))

(declaim (inline restore-single-float))
(defun restore-single-float (storage)
  (declare (optimize (speed 3) (safety 1)))
  (ensure-enough-data storage 4)
  (let ((offset (read-storage-offset storage))
        (sap (read-storage-sap storage)))
    (setf (read-storage-offset storage) (truly-the fixnum (+ 4 offset)))
    (sap-ref-single sap offset)))

(declaim (inline store-single-float))
(defun store-single-float (single-float storage &optional (tag t))
  (declare (optimize (speed 3) (safety 1)) (type single-float single-float))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 5 4) :sap sap)
    (when tag
      (set-sap-ref-8 sap offset +single-float-code+)
      (incf offset))
    (set-sap-ref-single sap offset single-float)))

(declaim (inline restore-double-float))
(defun restore-double-float (storage)
  (declare (optimize (speed 3) (safety 1)))
  (ensure-enough-data storage 8)
  (let ((offset (read-storage-offset storage))
        (sap (read-storage-sap storage)))
    (setf (read-storage-offset storage) (truly-the fixnum (+ 8 offset)))
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
     (let ((offset (read-storage-offset ,storage))
           (sap (read-storage-sap ,storage)))
       (setf (read-storage-offset ,storage) (truly-the fixnum (+ 8 offset)))
       (setf ,slot (sap-ref-double sap offset)))))

(declaim (inline store-double-float))
(defun store-double-float (double-float storage double-float-refs assign-new-reference-id
			   &optional (tag t))
  (declare (optimize (speed 3) (safety 1)) (type double-float double-float))
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
	    (set-sap-ref-8 sap offset +double-float-code+)
	    (incf offset))
	  (set-sap-ref-double sap offset double-float)))))

(declaim (inline ensure-integer))
(defun ensure-integer (x)
  (if (integerp x)
      x
      (progn (unexpected-data "expected an integer") 0)))

(defun restore-ratio (restore-object)
  (declare (optimize (speed 3) (safety 1)) (type function restore-object))
  (let ((a (ensure-integer (funcall restore-object)))
        (b (ensure-integer (funcall restore-object))))
    (declare (type integer a b))
    (when (= b 0)
      (unexpected-data "ratio denominator is 0"))
    (/ (the integer a) (the integer b))))

(defun store-ratio (ratio storage num-eq-refs assign-new-reference-id)
  "Nominally we don't need to do references here, but if someone has two bignums and takes
 a ratio of them, we don't want to store the bignums twice."
  (declare (optimize (speed 3) (safety 1)))
  (maybe-store-reference-instead (ratio storage num-eq-refs assign-new-reference-id)
    (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
      (set-sap-ref-8 sap offset +ratio-code+))
    (labels ((store-integer (integer)
               (if (typep integer 'fixnum)
                   (when storage (store-fixnum integer storage))
                   (store-bignum integer storage))))
      (store-integer (numerator ratio))
      (store-integer (denominator ratio)))))

(declaim (inline ensure-real))
(defun ensure-real (x)
  (if (typep x 'real)
      x
      (progn (unexpected-data "real") 0)))

(defun restore-complex (restore-object)
  (declare (type function restore-object))
  (complex (ensure-real (funcall restore-object))
           (ensure-real (funcall restore-object))))

(declaim (inline restore-complex-double-float))
(defun restore-complex-double-float (storage)
  (declare (optimize (speed 3) (safety 1)))
  (complex (restore-double-float storage)
	   (restore-double-float storage)))

(declaim (inline store-complex-double-float))
(defun store-complex-double-float (complex-double-float storage)
  (declare (optimize (speed 3) (safety 1)) (type (complex double-float) complex-double-float))
  (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
    (set-sap-ref-8 sap offset +complex-double-float-code+))
  (store-double-float (realpart complex-double-float) storage nil nil nil)
  (store-double-float (imagpart complex-double-float) storage nil nil nil))

(declaim (inline restore-complex-single-float))
(defun restore-complex-single-float (storage)
  (declare (optimize (speed 3) (safety 1)))
  (complex (restore-single-float storage)
	   (restore-single-float storage)))

(declaim (inline store-complex-single-float))
(defun store-complex-single-float (complex-single-float storage)
  (declare (optimize (speed 3) (safety 1)) (type (complex single-float) complex-single-float))
  (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
    (set-sap-ref-8 sap offset +complex-single-float-code+))
  (store-single-float (realpart complex-single-float) storage nil)
  (store-single-float (imagpart complex-single-float) storage nil))

(defun store-complex (complex storage store-object)
  (declare (type complex complex) (type (or null write-storage) storage))
  (typecase complex
    ;; We do not try to match double-floats in complex numbers to others... (except 0d0)
    ((complex double-float) (store-complex-double-float complex storage))
    ((complex single-float) (store-complex-single-float complex storage))
    (t
     (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
       (set-sap-ref-8 sap offset +complex-code+))
     (locally (declare (type function store-object))
       ;; We know it's a number, but overhead is small
       (funcall store-object (realpart complex))
       (funcall store-object (imagpart complex))))))

(declaim (inline restore-tagged-unsigned-fixnum/interior))
(defun restore-tagged-unsigned-fixnum/interior (storage)
  "Use this if you know that this is an unsigned number (so after
 another tag bit).  This opens up the direct coding space for up to
 +interior-coded-max-integer+."
  (declare (type read-storage storage) (optimize (speed 3) (safety 1)))
  (let ((tag (restore-ub8 storage)))
    (if (>= tag +first-direct-unsigned-integer-interior-code+)
	(- tag +first-direct-unsigned-integer-interior-code+)
	(truly-the fixnum
	  (+ (case tag
	       (#.+ub8-code+ (restore-ub8 storage))
	       (#.+ub16-code+ (restore-ub16 storage))
	       (#.+ub32-code+ (restore-ub32 storage))
	       (#.+fixnum-code+
                (let ((fixnum (restore-fixnum storage)))
                  (unless (<= 0 fixnum (- most-positive-fixnum +interior-coded-max-integer+ 1))
                    (unexpected-data "unsigned fixnum/interior" fixnum))
                  (truly-the fixnum fixnum)))
               (otherwise (unexpected-data "tag for unsigned fixnum" tag)))
	     +interior-coded-max-integer+ 1)))))

(declaim (ftype (function (read-storage)
			  #+sbcl (values fixnum &optional)
			  #-sbcl fixnum)
		restore-tagged-unsigned-fixnum))
(defun restore-tagged-unsigned-fixnum (storage)
  "Read back a number written by `store-tagged-unsigned-fixnum'."
  (declare (optimize (speed 3) (safety 1)))
  (let ((tag (restore-ub8 storage)))
    (if (<= +small-integer-zero-code+ tag +last-small-integer-code+)
	(- tag +small-integer-zero-code+)
	(case tag
	  (#.+ub8-code+ (restore-ub8 storage))
	  (#.+ub16-code+ (restore-ub16 storage))
	  (#.+ub32-code+ (restore-ub32 storage))
	  (#.+fixnum-code+ (restore-fixnum storage))
          (otherwise (unexpected-data "tag for unsigned fixnum" tag))))))

(declaim (ftype (function (read-storage)
			  #+sbcl (values fixnum &optional)
			  #-sbcl fixnum) restore-tagged-fixnum))
(defun restore-tagged-fixnum (storage)
  "Read back a number written by `store-tagged-unsigned-fixnum'."
  (let ((tag (restore-ub8 storage)))
    (if (<= +first-small-integer-code+ tag +last-small-integer-code+)
	(- tag +small-integer-zero-code+)
	(case tag
	  (#.+ub8-code+ (restore-ub8 storage))
	  (#.+ub16-code+ (restore-ub16 storage))
	  (#.+ub32-code+ (restore-ub32 storage))
	  (#.+fixnum-code+ (restore-fixnum storage))
	  (#.+sb8-code+ (restore-sb8 storage))
	  (#.+sb16-code+ (restore-sb16 storage))
	  (#.+sb32-code+ (restore-sb32 storage))
          (otherwise (unexpected-data "tag for fixnum" tag))))))

(declaim (inline store-tagged-unsigned-integer))

(defun store-tagged-unsigned-integer (integer storage)
  "Store and tag any unsigned integer.  For restoring this, call restore-object as this may
 be a bignum.  Prefer `store-tagged-unsigned-fixnum' if you are sure this isn't a bignum"
  (if (typep integer 'fixnum)
      (when storage
	(store-tagged-unsigned-fixnum integer storage))
      (store-bignum integer storage)))

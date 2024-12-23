(in-package :cl-store-faster)

;; See also unsigned-bytes.lisp for smaller unsigned numbers

(declaim (inline store-sb8))
(defun store-sb8 (sb8 storage &optional (tag +sb8-code+))
  "Store an (integer -255 0) value SB8 to STORAGE.  If TAG is true,
 then will store a tag +SB8-CODE+ to storage first.  Omit TAG if your
 deserializer will know this is a SB8 value.  It's a bit odd to have these,
 but the dispatch cost is negligible... we use the tag bit as the sign bit."
  (declare (optimize speed safety) (type (integer -255 0) sb8))
  (store-ub8 (- sb8) storage tag))

(declaim (inline store-sb16))
(defun store-sb16 (sb16 storage &optional (tag +sb16-code+))
  (declare (optimize speed safety) (type (integer -65535 0) sb16))
  (store-ub16 (- sb16) storage tag))

(declaim (inline store-sb32))
(defun store-sb32 (sb32 storage &optional (tag +sb32-code+))
  (declare (optimize speed safety) (type (integer -4294967295 0) sb32))
  (store-ub32 (- sb32) storage tag))

(declaim (notinline restore-sb8))
(defun restore-sb8 (storage)
  "Despite the name, restore an (integer -255 0) value from storage
 that has previously been stored by STORE-SB8"
  (- (restore-ub8 storage)))

(declaim (inline restore-sb16))
(defun restore-sb16 (storage)
  "Restore an (integer -65535 0) value from storage that has previously
 been stored by STORE-SB16."
  (- (restore-ub16 storage)))

(declaim (inline restore-sb32))
(defun restore-sb32 (storage)
  "Restore an (integer -4294967295 0) value from storage that has previously
 been stored by STORE-UB32."
  (- (restore-ub32 storage)))

(declaim (inline restore-fixnum))
(defun restore-fixnum (storage)
  (declare (optimize speed safety))
  (the (values fixnum &optional) (storage-read-sb64 storage)))

(declaim (notinline store-fixnum))
(defun store-fixnum (fixnum storage &optional (tag +fixnum-code+))
  (declare (optimize speed safety) (type fixnum fixnum))
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
  (let ((count (restore-fixnum storage)))
    (ensure-enough-data storage (* 4 count))
    (* (if (< count 0) -1 1)
       (bits->num
	(loop repeat (abs count)
	      collect (restore-ub32 storage))))))

(defun store-bignum (bignum storage references)
  (maybe-store-reference-instead (bignum storage references)
    (with-write-storage (storage)
      (storage-write-byte storage +bignum-code+)
      (multiple-value-bind (ub32s count)
	  (num->bits bignum)
	(declare (type fixnum count))
	(store-fixnum (if (minusp bignum) (- count) count) storage nil)
	(dolist (ub32 ub32s) (store-ub32 ub32 storage nil))))))

(declaim (inline restore-single-float))
(defun restore-single-float (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 4)
  (let ((offset (storage-offset storage))
        (array (storage-store storage)))
    (declare (type (unsigned-byte 60) offset))
    (setf (storage-offset storage) (+ 4 offset))
    (let ((a (make-array 1 :element-type 'single-float)))
      (declare (dynamic-extent a))
      (copy-n-bytes a 0 array offset 4)
      (aref a 0))))

(declaim (inline store-single-float))
(defun store-single-float (single-float storage &optional (tag t))
  (declare (optimize speed safety) (type single-float single-float))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 5 4) :sap sap)
    (let ((temp (make-array 1 :element-type 'single-float :initial-element single-float)))
      (declare (dynamic-extent temp))
      (when tag
	(storage-write-byte! storage +single-float-code+ :offset offset :sap sap)
	(incf offset))
      (sb-sys:with-pinned-objects (temp)
	(copy-sap sap offset (sb-sys:vector-sap temp) 0 8)))))

(declaim (inline restore-double-float))
(defun restore-double-float (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 8)
  (let ((offset (storage-offset storage))
        (array (storage-store storage)))
    (declare (type (unsigned-byte 60) offset))
    (setf (storage-offset storage) (+ 8 offset))
    (let ((a (make-array 1 :element-type 'double-float)))
      (declare (dynamic-extent a))
      (copy-n-bytes a 0 array offset 8)
      (aref a 0))))

(defmacro restore-double-float-to (slot storage)
  (assert (atom storage))
  `(progn
     (ensure-enough-data ,storage 8)
     (let ((offset (storage-offset ,storage))
           (array (storage-store ,storage)))
       (setf (storage-offset ,storage) (+ 8 offset))
       (let ((a (make-array 1 :element-type 'double-float)))
         (declare (dynamic-extent a))
         (copy-n-bytes a 0 array offset 8)
         (setf ,slot (aref a 0))))))

(declaim (inline store-double-float))
(defun store-double-float (double-float storage references &optional (tag t))
  (declare (optimize speed safety) (type double-float double-float))
  ;; We de-duplicate double-floats as there is no visible way to
  ;; determine this from common lisp, and it saves space in the image
  ;; and on disk if there are repeated numbers (like 0d0).
  (maybe-store-reference-instead (double-float storage references)
    (with-write-storage (storage :offset offset :reserve-bytes (if tag 9 8) :sap sap)
      (when tag
	(storage-write-byte! storage +double-float-code+ :offset offset :sap sap)
	(incf offset))
      (let ((temp (make-array 1 :element-type 'double-float :initial-element double-float)))
	(declare (dynamic-extent temp))
	(sb-sys:with-pinned-objects (temp)
	  (copy-sap sap offset (sb-sys:vector-sap temp) 0 8))))))

(defun restore-ratio (storage references)
  (declare (optimize speed safety))
  (/ (the integer (restore-object storage references))
     (the integer (restore-object storage references))))

(defun store-ratio (ratio storage references)
  (declare (optimize speed safety))
  (maybe-store-reference-instead (ratio storage references)
    (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
      (storage-write-byte! storage +ratio-code+ :offset offset :sap sap))
    (store-object (numerator ratio) storage references)
    (store-object (denominator ratio) storage references)))

(defun restore-complex (storage references)
  (complex (restore-object storage references)
	   (restore-object storage references)))

(defun store-complex (complex storage references)
  (maybe-store-reference-instead (complex storage references)
    (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
      (storage-write-byte! storage +complex-code+ :offset offset :sap sap))
    (store-object (realpart complex) storage references)
    (store-object (imagpart complex) storage references)))

(declaim (inline restore-complex-double-float))
(defun restore-complex-double-float (storage)
  (declare (optimize speed safety))
  (complex (restore-double-float storage)
	   (restore-double-float storage)))

(declaim (inline store-complex-double-float))
(defun store-complex-double-float (complex-double-float storage references)
  (declare (optimize speed safety) (type (complex double-float) complex-double-float))
  (maybe-store-reference-instead (complex-double-float storage references)
    (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
      (storage-write-byte! storage +complex-double-float-code+ :offset offset :sap sap))
    (store-double-float (realpart complex-double-float) storage nil nil)
    (store-double-float (imagpart complex-double-float) storage nil nil)))

(declaim (inline restore-complex-single-float))
(defun restore-complex-single-float (storage)
  (declare (optimize speed safety))
  (complex (restore-single-float storage)
	   (restore-single-float storage)))

(declaim (inline store-complex-single-float))
(defun store-complex-single-float (complex-single-float storage references)
  (declare (optimize speed safety) (type (complex single-float) complex-single-float))
  (maybe-store-reference-instead (complex-single-float storage references)
    (with-write-storage (storage :offset offset :sap sap :reserve-bytes 1)
      (storage-write-byte! storage +complex-single-float-code+ :sap sap :offset offset))
    (store-single-float (realpart complex-single-float) storage nil)
    (store-single-float (imagpart complex-single-float) storage nil)))

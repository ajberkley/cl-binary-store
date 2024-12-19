(in-package :cl-store-faster)

;; See also unsigned-bytes.lisp for smaller numbers

(declaim (inline restore-fixnum))
(defun restore-fixnum (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 8)
  (let ((offset (storage-offset storage))
        (array (storage-store storage)))
    ;; May be faster here to blit the data
    ;; instead of reading bytes.
    (setf (storage-offset storage) (+ 8 offset))
    (the fixnum
	 (+ (ash (aref array offset) 56)
	    (ash (aref array (incf offset)) 48)
	    (ash (aref array (incf offset)) 40)
	    (ash (aref array (incf offset)) 32)
	    (ash (aref array (incf offset)) 24)
	    (ash (aref array (incf offset)) 16)
	    (ash (aref array (incf offset)) 8)
	    (aref array (incf offset))))))

(declaim (inline store-fixnum))
(defun store-fixnum (fixnum storage &optional (tag t))
  (declare (optimize speed safety) (type fixnum fixnum))
  (with-write-storage (storage)
    (let ((offset (ensure-enough-room-to-write storage 9)))
      (when tag
	(storage-write-byte! storage +fixnum-code+ offset)
	(incf offset))
      (storage-write-ub64! storage fixnum offset)
      (setf (storage-offset storage) (+ offset 8)))))

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

(defun store-bignum (bignum storage)
  (maybe-store-reference-instead (bignum storage)
    (with-write-storage (storage)
      (storage-write-byte storage +bignum-code+)
      (multiple-value-bind (bits count)
	  (num->bits bignum)
	(declare (type fixnum count))
	(store-fixnum (if (minusp bignum) (- count) count) storage nil)
	(dolist (x bits) (store-ub32 x storage nil))))))

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
(defun store-single-float (single-float storage)
  (declare (optimize speed safety) (type single-float single-float))
  (with-write-storage (storage)
    (let ((offset (ensure-enough-room-to-write storage 5))
	  (temp (make-array 1 :element-type 'single-float :initial-element single-float)))
      (declare (dynamic-extent temp))
      (storage-write-byte! storage +single-float-code+ offset)
      (incf offset)
      (with-storage-sap (sap storage)
	(sb-sys:with-pinned-objects (temp)
	  (copy-sap sap offset (sb-sys:vector-sap temp) 0 8))
	(setf (storage-offset storage) (+ offset 4))))))

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

(declaim (inline store-double-float))
(defun store-double-float (double-float storage &optional (tag nil))
  (declare (optimize speed safety) (type double-float double-float))
  ;; We de-duplicate double-floats as there is no visible way to
  ;; determine this from common lisp, and it saves space in the image
  ;; and on disk if there are repeated numbers (like 0d0).
  (maybe-store-reference-instead (double-float storage)
    (with-write-storage (storage offset 9)
      (when tag
	(storage-write-byte! storage +double-float-code+ offset)
	(incf offset))
      (let ((temp (make-array 1 :element-type 'double-float)))
	(declare (dynamic-extent temp))
	(with-storage-sap (sap storage)
	  (sb-sys:with-pinned-objects (temp)
	    (copy-sap sap offset (sb-sys:vector-sap temp) 0 8))
	(setf (storage-offset storage) (+ offset 8)))))))

(defun restore-ratio (storage)
  (/ (the integer (restore-object storage))
     (the integer (restore-object storage))))

(defun store-ratio (ratio storage)
  (maybe-store-reference-instead (ratio storage)
    (with-write-storage (storage)
      (storage-write-byte storage +ratio-code+))
    (store-object (numerator ratio) storage)
    (store-object (denominator ratio) storage)))

(defun restore-complex (storage)
  (complex (restore-object storage)
	   (restore-object storage)))

(defun store-complex (complex storage)
  (maybe-store-reference-instead (complex storage)
    (with-write-storage (storage)
      (storage-write-byte storage +complex-code+))
    (store-object (realpart complex) storage)
    (store-object (imagpart complex) storage)))

(declaim (inline restore-complex-double-float))
(defun restore-complex-double-float (storage)
  (complex (restore-double-float storage)
	   (restore-double-float storage)))

(declaim (inline store-complex-double-float))
(defun store-complex-double-float (complex-double-float storage)
  (maybe-store-reference-instead (complex-double-float storage)
    (with-write-storage (storage)
      (storage-write-byte storage +complex-double-float-code+)
      (store-double-float (realpart complex-double-float) storage nil)
      (store-double-float (imagpart complex-double-float) storage nil))))

(declaim (inline restore-complex-single-float))
(defun restore-complex-single-float (storage)
  (complex (restore-single-float storage)
	   (restore-single-float storage)))

(declaim (inline store-complex-single-float))
(defun store-complex-single-float (complex-single-float storage)
  (maybe-store-reference-instead (complex-single-float storage)
    (when storage
      (storage-write-byte storage +complex-single-float-code+)
      (store-single-float (realpart complex-single-float) storage)
      (store-single-float (imagpart complex-single-float) storage))))

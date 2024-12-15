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
  (ensure-enough-room storage 9)
  (let ((offset (storage-offset storage))
        (array (storage-store storage)))
    (declare (type (unsigned-byte 60) offset))
    (when tag
      (setf (aref array offset) +fixnum-code+)
      (incf offset))
    (setf (aref array offset) (logand (ash fixnum -56) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -48) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -40) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -32) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -24) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -16) 255))
    (setf (aref array (incf offset)) (logand (ash fixnum -8) 255))
    (setf (aref array (incf offset)) (logand fixnum 255))
    (setf (storage-offset storage) (incf offset))))


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
    (record-reference 
     (* (if (< count 0) -1 1)
	(bits->num
	 (loop repeat (abs count)
	       collect (restore-ub32 storage)))))))

(defun store-bignum (bignum storage)
  (maybe-store-reference-instead (bignum storage)
    (store-ub8 +bignum-code+ storage nil)
    (multiple-value-bind (bits count)
	(num->bits bignum)
      (declare (type fixnum count))
      (store-fixnum (if (minusp bignum) (- count) count) storage nil)
      (dolist (x bits) (store-ub32 x storage nil)))))

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
  (ensure-enough-room storage 5)
  (let ((offset (storage-offset storage))
        (array (storage-store storage))
        (temp (make-array 1 :element-type 'single-float)))
    (declare (type (unsigned-byte 60) offset) (dynamic-extent temp))
    (setf (aref array offset) +single-float-code+)
    (incf offset)
    (setf (aref temp 0) single-float)
    (copy-n-bytes array offset temp 0 4)
     (setf (storage-offset storage) (incf offset 4))))

(declaim (inline restore-double-float))
(defun restore-double-float (storage)
  (declare (optimize speed safety))
  (ensure-enough-data storage 8)
  (record-reference
   (let ((offset (storage-offset storage))
         (array (storage-store storage)))
     (declare (type (unsigned-byte 60) offset))
     (setf (storage-offset storage) (+ 8 offset))
     (let ((a (make-array 1 :element-type 'double-float)))
       (declare (dynamic-extent a))
       (copy-n-bytes a 0 array offset 8)
       (aref a 0)))))

(declaim (inline store-double-float))
(defun store-double-float (double-float storage)
  (declare (optimize speed safety) (type double-float double-float))
  ;; We de-duplicate double-floats as there is no visible way to
  ;; determine this from common lisp, and it saves space in the image
  ;; and on disk if there are repeated numbers (like 0d0).
  (maybe-store-reference-instead (double-float storage)
    (ensure-enough-room storage 9)
    (let ((offset (storage-offset storage))
          (array (storage-store storage))
          (temp (make-array 1 :element-type 'double-float)))
      (declare (type (unsigned-byte 60) offset) (dynamic-extent temp))
      (setf (aref array offset) +double-float-code+)
      (incf offset)
      (setf (aref temp 0) double-float)
      (copy-n-bytes array offset temp 0 8)
      (setf (storage-offset storage) (incf offset 8)))))

(defun restore-ratio (storage)
  (record-reference
   (/ (the integer (restore-object storage))
      (the integer (restore-object storage)))))

(defun store-ratio (ratio storage)
  (maybe-store-reference-instead (ratio storage)
    (ensure-enough-room storage 1)
    (let ((offset (storage-offset storage)))
      (setf (aref (storage-store storage) offset) +ratio-code+)
      (setf (storage-offset storage) (+ 1 offset)))
    (store-object (numerator ratio) storage)
    (store-object (denominator ratio) storage)))

(defun restore-complex (storage)
  (record-reference
   (complex (restore-object storage)
	    (restore-object storage))))

(defun store-complex (complex storage)
  (maybe-store-reference-instead (complex storage)
    (ensure-enough-room storage 1)
    (let ((offset (storage-offset storage)))
      (setf (aref (storage-store storage) offset) +complex-code+)
      (setf (storage-offset storage) (+ 1 offset)))
    (store-object (realpart complex) storage)
    (store-object (imagpart complex) storage)))

(declaim (inline restore-complex-double-float))
(defun restore-complex-double-float (storage)
  (record-reference
   (complex (restore-double-float storage)
	    (restore-double-float storage))))

(declaim (inline store-complex-double-float))
(defun store-complex-double-float (complex-double-float storage)
  (maybe-store-reference-instead (complex-double-float storage)
    (ensure-enough-room storage 16)
    (store-double-float (realpart complex-double-float) storage)
    (store-double-float (imagpart complex-double-float) storage)))

(declaim (inline restore-complex-single-float))
(defun restore-complex-single-float (storage)
  (record-reference
   (complex (restore-single-float storage)
	    (restore-single-float storage))))

(declaim (inline store-complex-single-float))
(defun store-complex-single-float (complex-single-float storage)
  (maybe-store-reference-instead (complex-single-float storage)
    (ensure-enough-room storage 8)
    (store-single-float (realpart complex-single-float) storage)
    (store-single-float (imagpart complex-single-float) storage)))

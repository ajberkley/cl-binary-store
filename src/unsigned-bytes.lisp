(in-package :cl-store-faster)

(declaim (inline restore-ub8))
(defun restore-ub8 (storage &optional (ignore-end-of-data nil))
  "Restore an (unsigned-byte 8) value from storage that has previously
 been stored by STORE-UB8.  If IGNORE-END-OF-DATA will return NIL if
 there is no more data available (this feature is used while reading
 all data from a stream to see if there is more data available)"
  (declare (optimize speed safety))
  (and (ensure-enough-data storage 1 ignore-end-of-data)
       (let ((offset (storage-offset storage)))
	 (setf (storage-offset storage) (1+ offset))
	 (aref (storage-store storage) offset))))

(declaim (inline store-ub8))
(defun store-ub8 (ub8 storage &optional (tag t))
  "Store an (unsigned-byte 8) value UB8 to STORAGE.  If TAG is true,
 then will store a tag +UB8-CODE+ to storage first.  Omit TAG if your
 deserializer will know this is a UB8 value."
  (declare (optimize speed safety))
  (when storage
    (ensure-enough-room storage 2)
    (let ((array (storage-store storage))
	  (offset (storage-offset storage)))
      (when tag
	(setf (aref array offset) +ub8-code+)
	(incf offset))
      (setf (aref array offset) ub8)
      (setf (storage-offset storage) (incf offset)))))

(declaim (inline restore-ub16))
(defun restore-ub16 (storage)
  "Restore a (unsigned-byte 16) from STORAGE which has previously been stored
 by STORE-UB16."
  (declare (optimize speed safety))
  (ensure-enough-data storage 2)
  (let ((offset (storage-offset storage))
        (array (storage-store storage)))
    (setf (storage-offset storage) (+ 2 offset))
    (+ (aref array offset) (ash (aref array (incf offset)) 8))))

(declaim (inline store-ub16))
(defun store-ub16 (ub16 storage &optional (tag t))
  "Store an (unsigned-byte 16) value UB16 to STORAGE.  If TAG is true will
 emit +UB16-CODE+ to STORAGE first.  Set TAG NIL if the deserializer will
 know from the context that the value is a UB16 to save a byte."
  (declare (optimize speed safety) (type (unsigned-byte 16) ub16))
  (when storage
    (ensure-enough-room storage 3)
    (let ((offset (storage-offset storage))
          (array (storage-store storage)))
      (declare (type (unsigned-byte 60) offset))
      (when tag
	(setf (aref array offset) +ub16-code+)
	(incf offset))
      (setf (aref array offset) (logand ub16 255))
      (setf (aref array (incf offset)) (ash ub16 -8))
      (setf (storage-offset storage) (incf offset)))))

(declaim (inline restore-ub32))
(defun restore-ub32 (storage)
  "Restore a (unsigned-byte 32) from STORAGE which has previously been stored
 by STORE-UB32."
  (declare (optimize speed safety))
  (when storage
    (ensure-enough-data storage 4)
    (let ((offset (storage-offset storage))
          (array (storage-store storage)))
      (declare (type (unsigned-byte 60) offset))
      (setf (storage-offset storage) (+ 4 offset))
      (+ (aref array offset)
	 (ash (aref array (incf offset)) 8)
	 (ash (aref array (incf offset)) 16)
	 (ash (aref array (incf offset)) 24)))))

(declaim (inline store-ub32))
(defun store-ub32 (ub32 storage &optional (tag t))
  "Store an (unsigned-byte 32) value UB32 to STORAGE.  If TAG is true will
 emit +UB32-CODE+ to STORAGE first.  Set TAG NIL if the deserializer will
 know from the context that the value is a UB32 to save a byte."
  (declare (optimize speed safety) (type (unsigned-byte 32) ub32))
  (when storage
    (ensure-enough-room storage 5)
    (let ((offset (storage-offset storage))
          (array (storage-store storage)))
      (declare (type (unsigned-byte 60) offset))
      (when tag
	(setf (aref array offset) +ub32-code+)
	(incf offset))
      (setf (aref array offset) (logand ub32 255))
      (setf (aref array (incf offset)) (logand (ash ub32 -8) 255))
      (setf (aref array (incf offset)) (logand (ash ub32 -16) 255))
      (setf (aref array (incf offset)) (ash ub32 -24))
      (setf (storage-offset storage) (incf offset)))))

(declaim (inline store-tagged-unsigned-integer))
(defun store-tagged-unsigned-integer (integer storage)
  "Because this is stored tagged, you can restore it using
 RESTORE-OBJECT."
  (when storage
    (if (< integer 256)
	(store-ub8 integer storage t)
	(if (< integer 65536)
	    (store-ub16 integer storage t)
	    (if (< integer (expt 2 32))
		(store-ub32 integer storage t)
		(if (typep integer 'fixnum)
		    (store-fixnum integer storage t)
		    (store-bignum integer storage)))))))

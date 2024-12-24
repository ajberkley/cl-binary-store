(in-package :cl-store-faster)

(declaim (inline maybe-restore-ub8))
(defun maybe-restore-ub8 (storage)
  "Maybe restore an (unsigned-byte 8) value from storage that has previously
 been stored by STORE-UB8.  If there is no more data available will return NIL."
  (declare (optimize (speed 3) (safety 0) (debug 0))) ;; this is called all the time!
  (and (ensure-enough-data storage 1 t)
       (let ((offset (storage-offset storage)))
	 (setf (storage-offset storage) (1+ offset))
         (sap-ref-8 (storage-sap storage) offset))))

(declaim (inline restore-ub8))
(defun restore-ub8 (storage)
  "Restore an (unsigned-byte 8) value from storage that has previously
 been stored by STORE-UB8."
  (declare (optimize (speed 3) (safety 0) (debug 0))) ;; called all the time!
  (ensure-enough-data storage 1)
  (let ((offset (storage-offset storage)))
    (prog1
	(sap-ref-8 (storage-sap storage) offset)
      (setf (storage-offset storage) (+ 1 offset)))))

(declaim (inline restore-ub16))
(defun restore-ub16 (storage)
  "Restore a (unsigned-byte 16) from STORAGE which has previously been stored
 by STORE-UB16."
  (declare (optimize speed safety))
  (ensure-enough-data storage 2)
  (let ((offset (storage-offset storage))
        (sap (storage-sap storage)))
    (setf (storage-offset storage) (+ 2 offset))
    (sap-ref-16 sap offset)))

(declaim (inline restore-ub32))
(defun restore-ub32 (storage)
  "Restore a (unsigned-byte 32) from STORAGE which has previously been stored
 by STORE-UB32."
  (declare (optimize speed safety))
  (ensure-enough-data storage 4)
  (let ((offset (storage-offset storage))
        (sap (storage-sap storage)))
    (setf (storage-offset storage) (+ 4 offset))
    (sap-ref-32 sap offset)))

(declaim (inline store-ub8))
(defun store-ub8 (ub8 storage &optional (tag +ub8-code+))
  "Store an (unsigned-byte 8) value UB8 to STORAGE.  If TAG is true,
 then will store a tag +UB8-CODE+ to storage first.  Omit TAG if your
 deserializer will know this is a UB8 value."
  (declare (optimize speed safety) (type (unsigned-byte 8) ub8)
	   (type (or null (unsigned-byte 8)) tag)
	   (type (or null storage) storage))
  (if tag
      (with-write-storage (storage :offset offset :reserve-bytes 2)
	(storage-write-ub16! storage (+ tag (ash ub8 8)) :offset offset))
      (with-write-storage (storage :offset offset :reserve-bytes 1)
	(storage-write-byte! storage ub8 :offset offset))))

(declaim (inline store-ub16))
(defun store-ub16 (ub16 storage &optional (tag +ub16-code+))
  "Store an (unsigned-byte 16) value UB16 to STORAGE.  If TAG is true will
 emit +UB16-CODE+ to STORAGE first.  Set TAG NIL if the deserializer will
 know from the context that the value is a UB16 to save a byte."
  (declare (optimize speed safety) (type (unsigned-byte 16) ub16))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 3 2) :sap sap)
    (when tag
      (storage-write-byte! storage tag :offset offset :sap sap)
      (incf offset))
    (storage-write-ub16! storage ub16 :offset offset :sap sap)))

(declaim (inline store-ub32))
(defun store-ub32 (ub32 storage &optional (tag +ub32-code+))
  "Store an (unsigned-byte 32) value UB32 to STORAGE.  If TAG is true will
 emit +UB32-CODE+ to STORAGE first.  Set TAG NIL if the deserializer will
 know from the context that the value is a UB32 to save a byte."
  (declare (optimize speed safety) (type (unsigned-byte 32) ub32)
	   (type (or null (unsigned-byte 8)) tag))
  (with-write-storage (storage :offset offset :reserve-bytes (if tag 5 4) :sap sap)
    (when tag
      (storage-write-byte! storage tag :offset offset :sap sap)
      (incf offset))
    (storage-write-ub32! storage ub32 :offset offset :sap sap)))

(declaim (notinline store-tagged-unsigned-fixnum))
(defun store-tagged-unsigned-fixnum (integer storage)
  (declare (type (and fixnum (integer 0)) integer) (optimize speed safety)
	   (type storage storage))
  (when storage
    (if (< integer 256)
	(store-ub8 integer storage)
	(if (< integer 65536)
	    (store-ub16 integer storage)
	    (if (< integer (expt 2 32))
		(store-ub32 integer storage)
		(store-fixnum integer storage))))))

(declaim (inline store-tagged-unsigned-integer))
(defun store-tagged-unsigned-integer (integer storage references)
  "Because this is stored tagged, you can restore it using
 RESTORE-OBJECT."
  (when storage
    (if (typep integer 'fixnum)
	(store-tagged-unsigned-fixnum integer storage)
	(store-bignum integer storage references))))

(defun restore-tagged-unsigned-fixnum (storage)
  (let ((tag (restore-ub8 storage)))
    (ecase tag
      (#.+ub8-code+ (restore-ub8 storage))
      (#.+ub16-code+ (restore-ub16 storage))
      (#.+ub32-code+ (restore-ub32 storage))
      (#.+fixnum-code+ (restore-fixnum storage)))))

;; TODO write restore-tagged-unsigned-fixnum to bypass some
;; dispatch?

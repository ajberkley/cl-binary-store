(in-package :cl-binary-store)

(declaim (#-debug-cbs inline #+debug-cbs notinline maybe-restore-ub8))
(defun maybe-restore-ub8 (storage)
  "Maybe restore an (unsigned-byte 8) value from storage that has previously
 been stored by STORE-UB8.  If there is no more data available will return NIL."
  (declare #-debug-cbs(optimize (speed 3) (safety 0) (debug 0))) ;; this is called all the time!
  (and (ensure-enough-data storage 1 t)
       (let ((offset (read-storage-offset storage)))
	 (setf (read-storage-offset storage) (1+ offset))
         (sap-ref-8 (read-storage-sap storage) offset))))

(declaim (#-debug-cbs inline #+debug-cbs notinline restore-ub8))
(defun restore-ub8 (storage)
  "Restore an (unsigned-byte 8) value from storage that has previously
 been stored by STORE-UB8."
  (declare #-debug-cbs(optimize (speed 3) (safety 0) (debug 0))) ;; called all the time!
  (ensure-enough-data storage 1)
  (let ((offset (read-storage-offset storage)))
    (prog1
	(sap-ref-8 (read-storage-sap storage) offset)
      (setf (read-storage-offset storage) (+ 1 offset)))))

(declaim (inline restore-ub16))
(defun restore-ub16 (storage)
  "Restore a (unsigned-byte 16) from STORAGE which has previously been stored
 by STORE-UB16."
  (declare (optimize speed safety))
  (ensure-enough-data storage 2)
  (let ((offset (read-storage-offset storage))
        (sap (read-storage-sap storage)))
    (setf (read-storage-offset storage) (+ 2 offset))
    (sap-ref-16 sap offset)))

(declaim (inline restore-ub32))
(defun restore-ub32 (storage)
  "Restore a (unsigned-byte 32) from STORAGE which has previously been stored
 by STORE-UB32."
  (declare (optimize speed safety))
  (ensure-enough-data storage 4)
  (let ((offset (read-storage-offset storage))
        (sap (read-storage-sap storage)))
    (setf (read-storage-offset storage) (+ 4 offset))
    (sap-ref-32 sap offset)))

(declaim (inline store-ub8))
(defun store-ub8 (ub8 storage &optional (tag +ub8-code+))
  "Store an (unsigned-byte 8) value UB8 to STORAGE.  If TAG is nil then
 we will skip writing a tag byte; use if your deserializer will know that
 the next byte is a UB8"
  (declare (optimize speed safety) (type (unsigned-byte 8) ub8)
	   (type (or null (unsigned-byte 8)) tag)
	   (type (or null write-storage) storage))
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

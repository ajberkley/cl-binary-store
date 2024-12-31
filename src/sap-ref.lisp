(in-package :cl-binary-store)

;; Functions to access static-vectors memory

(declaim (inline sap-ref-8 (setf sap-ref-8) sap-ref-16 (setf sap-ref-16)
		 sap-ref-32 (setf sap-ref-32) signed-sap-ref-32 (setf signed-sap-ref-32)
		 sap-ref-64 (setf sap-ref-64) signed-sap-ref-64 (setf signed-sap-ref-64)
		 sap-ref-double (setf sap-ref-double)
		 sap-ref-single (setf sap-ref-single)))

(defun (setf sap-ref-8) (ub8 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-8 sap offset) ub8)
  #-sbcl (setf (cffi:mem-ref sap :uint8 offset) ub8))

(defun (setf sap-ref-16) (ub16 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-16 sap offset) ub16)
  #-sbcl (setf (cffi:mem-ref sap :uint16 offset) ub16))

(defun (setf sap-ref-32) (ub32 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-32 sap offset) ub32)
  #-sbcl (setf (cffi:mem-ref sap :uint32 offset) ub32))

(defun (setf sap-ref-64) (ub64 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-64 sap offset) ub64)
  #-sbcl (setf (cffi:mem-ref sap :uint64 offset) ub64))

(defun (setf signed-sap-ref-64) (sb64 sap offset)
  #+sbcl (setf (sb-sys:signed-sap-ref-64 sap offset) sb64)
  #-sbcl (setf (cffi:mem-ref sap :int64 offset) sb64))

(defun sap-ref-8 (sap offset)
  #+sbcl (sb-sys:sap-ref-8 sap offset)
  #-sbcl (cffi:mem-ref sap :uint8 offset))

(defun sap-ref-16 (sap offset)
  #+sbcl (sb-sys:sap-ref-16 sap offset)
  #-sbcl (cffi:mem-ref sap :uint16 offset))

(defun sap-ref-32 (sap offset)
  #+sbcl (sb-sys:sap-ref-32 sap offset)
  #-sbcl (cffi:mem-ref sap :uint32 offset))

(defun sap-ref-64 (sap offset)
  #+sbcl (sb-sys:sap-ref-64 sap offset)
  #-sbcl (cffi:mem-ref sap :uint64 offset))

(defun sap-ref-double (sap offset)
  #+sbcl (sb-sys:sap-ref-double sap offset)
  #-sbcl (cffi:mem-ref sap :double offset))

(defun (setf sap-ref-double) (double sap offset)
  #+sbcl (setf (sb-sys:sap-ref-double sap offset) double)
  #-sbcl (setf (cffi:mem-ref sap :double offset) double))

(defun sap-ref-single (sap offset)
  #+sbcl (sb-sys:sap-ref-single sap offset)
  #-sbcl (cffi:mem-ref sap :float offset))

(defun (setf sap-ref-single) (single-float sap offset)
  #+sbcl (setf (sb-sys:sap-ref-single sap offset) single-float)
  #-sbcl (setf (cffi:mem-ref sap :float offset) single-float))

(defun signed-sap-ref-32 (sap offset)
  #+sbcl (sb-sys:signed-sap-ref-32 sap offset)
  #-sbcl (cffi:mem-ref sap :int32 offset))

(defun (setf signed-sap-ref-32) (value sap offset)
  #+sbcl (setf (sb-sys:signed-sap-ref-32 sap offset) value)
  #-sbcl (setf (cffi:mem-ref sap :int32 offset) value))

(defun signed-sap-ref-64 (sap offset)
  #+sbcl (sb-sys:signed-sap-ref-64 sap offset)
  #-sbcl (cffi:mem-ref sap :uint64 offset))

(defmacro array-sap (array)
  "Return a SAP referring to the backing store of array-sap (on sbcl) otherwise the
 1D vector backing-store of the vector."
  #+sbcl
  (let ((g (gensym)))
    `(sb-kernel:with-array-data ((,g ,array) (start) (end))
       (declare (ignore end))
       (assert (zerop start))
       (with-pinned-objects (,g)
         (vector-sap ,g))))
  #-sbcl
  (error "unimplemented"))

(defmacro with-pinned-objects ((&rest objects) &body body)
  #+sbcl
  `(sb-sys:with-pinned-objects ,objects
     ,@body)
  #-sbcl `(progn ,@body))

(defmacro vector-sap (vector)
  "On sbcl, return a SAP referring to the backing store of vector, otherwise the
 vector itself"
  #+sbcl `(sb-sys:vector-sap ,vector)
  #-sbcl vector)


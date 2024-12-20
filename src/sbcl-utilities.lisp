(in-package :cl-store-faster)

(sb-alien:define-alien-routine "memcpy" sb-alien:void
  (dest sb-alien:system-area-pointer :in)
  (src sb-alien:system-area-pointer :in)
  (n sb-alien:int :in))

(declaim (inline copy-sap))
(defun copy-sap (target-sap target-offset source-sap source-offset n)
  (case n
    (0)
    (1
     (setf (sb-sys:sap-ref-8 target-sap target-offset)
	   (sb-sys:sap-ref-8 source-sap source-offset)))
    (2
     (setf (sb-sys:sap-ref-16 target-sap target-offset)
	   (sb-sys:sap-ref-16 source-sap source-offset)))
    (4
     (setf (sb-sys:sap-ref-32 target-sap target-offset)
	   (sb-sys:sap-ref-32 source-sap source-offset)))
    (8
     (setf (sb-sys:sap-ref-64 target-sap target-offset)
	   (sb-sys:sap-ref-64 source-sap source-offset)))
    (t
     (memcpy (sb-sys:sap+ target-sap target-offset)
	     (sb-sys:sap+ source-sap source-offset)
	     n))))

  
(declaim (inline copy-n-bytes/array))
(defun copy-n-bytes/array (target target-offset source source-offset n)
  (sb-kernel:with-array-data ((target-array target) (start) (end))
    (assert (zerop start))
    (sb-kernel:with-array-data ((source-array source) (start) (end))
      (assert (zerop start))
      (let ((target-sap (sb-sys:vector-sap target-array))
	    (source-sap (sb-sys:vector-sap source-array)))
	(copy-sap target-sap target-offset source-sap source-offset n))))
  (values))

(declaim (inline copy-n-bytes))
(defun copy-n-bytes (target target-offset source source-offset n)
  (declare (optimize speed safety))
  (sb-sys:with-pinned-objects (source target)
    (let ((source-sap (sb-sys:vector-sap source))
	  (target-sap (sb-sys:vector-sap target)))
      (copy-sap target-sap target-offset source-sap source-offset n)))
  (values))

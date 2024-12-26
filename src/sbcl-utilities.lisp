(in-package :cl-binary-store)

(sb-alien:define-alien-routine "memcpy" sb-alien:void
  (dest sb-alien:system-area-pointer :in)
  (src sb-alien:system-area-pointer :in)
  (n sb-alien:int :in))

(declaim (inline copy-sap))
(defun copy-sap (target-sap target-offset source-sap source-offset n)
  (case n
    (0)
    (1
     (setf (sap-ref-8 target-sap target-offset)
	   (sap-ref-8 source-sap source-offset)))
    (2
     (setf (sap-ref-16 target-sap target-offset)
	   (sap-ref-16 source-sap source-offset)))
    (4
     (setf (sap-ref-32 target-sap target-offset)
	   (sap-ref-32 source-sap source-offset)))
    (8
     (setf (sap-ref-64 target-sap target-offset)
	   (sap-ref-64 source-sap source-offset)))
    (t
     #+sbcl
     (memcpy (sb-sys:sap+ target-sap target-offset)
	     (sb-sys:sap+ source-sap source-offset)
	     n)
     #-sbcl
     (error "not implemented"))))

(declaim (inline copy-n-bytes))
(defun copy-n-bytes (target target-offset source source-offset n)
  (declare (optimize speed safety))
  (with-pinned-objects (source target)
    (let ((source-sap (vector-sap source))
	  (target-sap (vector-sap target)))
      (copy-sap target-sap target-offset source-sap source-offset n)))
  (values))

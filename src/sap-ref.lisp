(in-package :cl-binary-store)

;; Functions to access foreign memory and set it from lisp values or
;; read into lisp values.  Uses SBCL stuff for SBCL, CFFI for other
;; impls, and some other work around.  Here we provide an interface
;; for accessing unaligned 8, 16, 32, 64 unsigned bits and 32, 64
;; signed bits, and unaligned single-floats and double-floats.

(declaim (inline sap-ref-8 (setf sap-ref-8) sap-ref-16 (setf sap-ref-16)
		 sap-ref-32 (setf sap-ref-32) sap-ref-64 (setf sap-ref-64)
		 signed-sap-ref-64 (setf signed-sap-ref-64)
		 sap-ref-double (setf sap-ref-double)
		 sap-ref-single (setf sap-ref-single)))

(defun (setf sap-ref-8) (ub8 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-8 sap offset) ub8)
  #-sbcl (setf (cffi:mem-ref sap :uint8 offset) ub8))

(defun (setf sap-ref-16) (ub16 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-16 sap offset) ub16)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :uint16 offset) ub16)
  #+allegro
  (progn (setf (cffi:mem-ref sap :uint8 offset) (logand ub16 #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 1)) (ash ub16 -8))))

(defun (setf sap-ref-32) (ub32 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-32 sap offset) ub32)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :uint32 offset) ub32)
  #+allegro
  (progn (setf (cffi:mem-ref sap :uint8 (+ offset 0)) (logand (ash ub32 0) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 1)) (logand (ash ub32 -8) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 2)) (logand (ash ub32 -16) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 3)) (logand (ash ub32 -24) #xFF))))

(defun (setf sap-ref-64) (ub64 sap offset)
  #+sbcl (setf (sb-sys:sap-ref-64 sap offset) ub64)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :uint64 offset) ub64)
  #+allegro
  (progn (setf (cffi:mem-ref sap :uint8 (+ offset 0)) (logand (ash ub64 0)   #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 1)) (logand (ash ub64 -8)  #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 2)) (logand (ash ub64 -16) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 3)) (logand (ash ub64 -24) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 4)) (logand (ash ub64 -32) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 5)) (logand (ash ub64 -40) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 6)) (logand (ash ub64 -48) #xFF))
	 (setf (cffi:mem-ref sap :uint8 (+ offset 7)) (logand (ash ub64 -56) #xFF))))

(defun sap-ref-8 (sap offset)
  #+sbcl (sb-sys:sap-ref-8 sap offset)
  #-sbcl (cffi:mem-ref sap :uint8 offset))

(defun sap-ref-16 (sap offset)
  #+sbcl (sb-sys:sap-ref-16 sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :uint16 offset)
  #+allegro (+ (ash (cffi:mem-ref sap :uint8 offset) 0)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 1)) 8)))

(defun sap-ref-32 (sap offset)
  #+sbcl (sb-sys:sap-ref-32 sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :uint32 offset)
  #+allegro (+ (ash (cffi:mem-ref sap :uint8 offset) 0)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 1)) 8)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 2)) 16)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 3)) 24)))

(defun sap-ref-64 (sap offset)
  #+sbcl (sb-sys:sap-ref-64 sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :uint64 offset)
  #+allegro (+ (ash (cffi:mem-ref sap :uint8 offset) 0)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 1)) 8)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 2)) 16)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 3)) 24)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 4)) 32)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 5)) 40)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 6)) 48)
	       (ash (cffi:mem-ref sap :uint8 (+ offset 7)) 56)))

(defun sap-ref-double (sap offset)
  #+sbcl (sb-sys:sap-ref-double sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :double offset)
  #+allegro
  (excl:shorts-to-double-float
   (sap-ref-16 sap (+ offset 6))
   (sap-ref-16 sap (+ offset 4))
   (sap-ref-16 sap (+ offset 2))
   (sap-ref-16 sap (+ offset 0))))

(defun (setf sap-ref-double) (double sap offset)
  #+sbcl (setf (sb-sys:sap-ref-double sap offset) double)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :double offset) double)
  #+allegro
  (multiple-value-bind (s3 s2 s1 s0)
      (excl:double-float-to-shorts double)
    (setf (sap-ref-16 sap offset) s0)
    (setf (sap-ref-16 sap (+ offset 2)) s1)
    (setf (sap-ref-16 sap (+ offset 4)) s2)
    (setf (sap-ref-16 sap (+ offset 6)) s3)))

(defun sap-ref-single (sap offset)
  #+sbcl (sb-sys:sap-ref-single sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :float offset)
  #+allegro
  (excl:shorts-to-single-float
   (sap-ref-16 sap (+ offset 2))
   (sap-ref-16 sap (+ offset 0))))

(defun (setf sap-ref-single) (single-float sap offset)
  #+sbcl (setf (sb-sys:sap-ref-single sap offset) single-float)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :float offset) single-float)
  #+allegro
  (multiple-value-bind (s1 s0)
      (excl:single-float-to-shorts single-float)
    (setf (sap-ref-16 sap offset) s0)
    (setf (sap-ref-16 sap (+ offset 2)) s1)))

(declaim (inline mask-signed negative-to-twos-complement/64))
(defun mask-signed (x size)
  "Re-interpret a SIZE bit lisp number as if it were a signed twos complement number"
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun negative-to-twos-complement/64 (x)
  (assert (< x 0))
  (logand (+ 1 (logxor (- x) #xFFFFFFFFFFFFFFFF)) #xFFFFFFFFFFFFFFFF))

(defun signed-sap-ref-64 (sap offset)
  #+sbcl (sb-sys:signed-sap-ref-64 sap offset)
  #-(or sbcl allegro) (cffi:mem-ref sap :int64 offset)
  #+allegro (mask-signed (sap-ref-64 sap offset) 64))

(defun (setf signed-sap-ref-64) (value sap offset)
  #+sbcl (setf (sb-sys:signed-sap-ref-64 sap offset) value)
  #-(or sbcl allegro) (setf (cffi:mem-ref sap :int64 offset) value)
  #+allegro
  (setf (sap-ref-64 sap offset)
	(if (< value 0)
	    (negative-to-twos-complement/64 value)
	    value)))

(defmacro array-sap (array)
  "Return a pointer referring to the backing store of an array (on sbcl)"
  (declare (ignorable array))
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
  (declare (ignorable objects))
  #+sbcl
  `(sb-sys:with-pinned-objects ,objects
     ,@body)
  #-sbcl `(progn ,@body))

(defmacro vector-sap (vector)
  "On sbcl, return a SAP referring to the backing store of vector"
  #+sbcl `(sb-sys:vector-sap ,vector)
  #-sbcl vector)


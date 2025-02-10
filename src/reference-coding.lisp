(in-package :cl-binary-store)

(defconstant +reference-direct-min-ref-id+ 1)
(defconstant +reference-direct-max-ref-id+
  (+ +reference-direct-min-ref-id+
     (- +last-direct-reference-id-code+ +first-direct-reference-id-code+)))
(defconstant +reference-one-byte-min-ref-id+ (+ +reference-direct-max-ref-id+ 1))
(defconstant +reference-one-byte-max-ref-id+ (+ (ash 255 6) +reference-one-byte-min-ref-id+))
(defconstant +reference-two-byte-min-ref-id+ (+ +reference-one-byte-max-ref-id+ 1))
(defconstant +reference-two-byte-max-ref-id+ (+ (ash 65535 6) +reference-two-byte-min-ref-id+))

(declaim (inline decode-reference-direct))
(defun decode-reference-direct (raw-8-bit)
  "Result will be between [1 30].  This uses just the tag byte"
  (declare (optimize (speed 3) (safety 1)) (type (unsigned-byte 8) raw-8-bit))
  (assert (<= +first-direct-reference-id-code+ raw-8-bit +last-direct-reference-id-code+))
  (+ 1 (- raw-8-bit +first-direct-reference-id-code+)))

(declaim (inline decode-reference-one-byte))
(defun decode-reference-one-byte (tag-byte next-byte)
  "Result will be between [31 16414].  This uses the tag byte plus another byte"
  (declare (type (unsigned-byte 8) tag-byte next-byte) (optimize (speed 3) (safety 1)))
  ;;(format t "Tag byte is ~A, next-byte is ~A~%" tag-byte next-byte)
  (assert (<= +first-one-byte-reference-id-code+ tag-byte +last-one-byte-reference-id-code+))
  (+ +reference-one-byte-min-ref-id+ (logxor tag-byte #x40) (ash next-byte 6)))

(declaim (inline decode-reference-two-bytes))
(defun decode-reference-two-bytes (tag-byte next-16-bits)
  "Result will be between [16415 4210718].  This uses the tag byte plus 2 additional bytes"
  (declare (type (unsigned-byte 8) tag-byte) (type (unsigned-byte 16) next-16-bits)
	   (optimize (speed 3) (safety 1)))
  ;;(format t "Tag byte is ~A, next-16-bits is ~A~%" tag-byte next-16-bits)
  (assert (<= +first-two-byte-reference-id-code+ tag-byte +last-two-byte-reference-id-code+))
  (+ +reference-two-byte-min-ref-id+ (logxor tag-byte #x80) (ash next-16-bits 6)))

(declaim (inline decode-reference-tagged))
(defun decode-reference-tagged (number)
  "Number ranges from -16 to wherever.  This uses the reference-tag byte plus the tagged integer
 which can be anywhere from 1 byte direct tagged to arbitrarily large."
  (declare (optimize (speed 3) (safety 1)))
  (if (and (typep number 'fixnum) (>= number +minimum-untagged-signed-integer+)
           (<= number #.(expt 2 54))) ;; arbitrary 1 TB limit on number of references!
      (truly-the fixnum
           (+ (- +minimum-untagged-signed-integer+)
              (truly-the fixnum number)
              1 +reference-two-byte-max-ref-id+))
      (unexpected-data "reference tag not valid")))

(declaim (inline encode-reference-direct))
(defun encode-reference-direct (ref-index)
  "reference indicies start a 1, so we subtract one here."
  (+ (- +first-direct-reference-id-code+ 1) ref-index))

;; Little endian, least significant byte first
(declaim (inline encode-reference-one-byte))
(defun encode-reference-one-byte (ref-index)
  "Returns a 16 bit value"
  (let* ((shifted-ref-index
	   (- ref-index +reference-one-byte-min-ref-id+))
	 (tag-byte (+ #x40 (logand shifted-ref-index #x3F))))
    (+ tag-byte (ash (logand shifted-ref-index #xFFC0) 2))))

(declaim (inline encode-reference-two-bytes))
(defun encode-reference-two-bytes (ref-index)
  (let ((shifted-ref-index
	  (- ref-index +reference-two-byte-min-ref-id+)))
    (values (+ #x80 (logand shifted-ref-index #x3F))
	    (ash shifted-ref-index -6))))

(declaim (inline encode-reference-tagged))
(defun encode-reference-tagged (ref-index)
  (+ (- ref-index (+ 1 +reference-two-byte-max-ref-id+)) +minimum-untagged-signed-integer+))

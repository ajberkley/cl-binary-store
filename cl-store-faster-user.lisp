(defpackage #:cl-store-faster-user
  (:use #:common-lisp #:cl-store-faster)
  (:documentation "A serialization/deserialization library for Common
 lisp optimized for speed and flexibility.

 The goal is about 10x faster than cl-store and reasonably compact files.

 Out of the box we support reading/writing from/to streams, reading/writing from/to
 in-memory vectors (directly and via flexi-streams).

 Extending this with your own serializer / deserializer is straightforward as it is
 expected that the default standard-object and structure-object serialization may
 not meet everyones needs.

 All number types are supported, but we provide specialized compact writers for:
  ub8, ub16, ub32, ub64, fixnum, single-float, double-float
  (complex double-float) (complex single-float)
 
 All array types are supported.

 On SBCL we provide fast and compact serialization of vectors and simple-arrays of:
  bit        (simple-bit-vector in 1D or multi-dimensional)
  base-char  (simple-base-string in 1D or multi-dimensional)
  character  (simple-string in 1D or multi-dimensional)
  single-float
  double-float
  fixnum
  signed-byte: 8 16 32 64
  unsigned-byte: 2 4 7 8 15 16 31 32 62 64")
  (:export
   ;; User interface
   #:restore-from-stream
   #:store-to-stream
   #:restore-from-vector
   #:store-to-vector
   #:end-of-data
   #:store-to-file
   #:restore-from-file
   ;; Support complex circular lists
   #:*support-shared-list-structures*))

(in-package #:cl-store-faster-user)
  

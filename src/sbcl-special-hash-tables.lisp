(in-package :cl-binary-store)

(declaim (inline double-float-=))
(defun double-float-= (dfa dfb)
  (declare (type double-float dfa dfb))
  (= dfa dfb))

(declaim (inline double-float-hash))
(defun double-float-hash (df)
  (declare (type double-float df))
  (sxhash df))

(declaim (inline string-hash))
(defun string-hash (simple-string)
  (declare (type simple-string simple-string))
  (sxhash simple-string))

(declaim (inline string-and-type-=))
(defun string-and-type-= (stringa stringb)
  "These are not displaced strings, etc.  Just simple strings."
  (declare (type simple-string stringa stringb) (optimize (speed 3) (safety 0)))
  (let ((is-simple-base-string (typep stringa 'simple-base-string)))
    (if is-simple-base-string
        (and (typep stringb 'simple-base-string) (string= stringa stringb))
        (and (not (typep stringb 'simple-base-string)) (string= stringa stringb)))))

(sb-ext:define-hash-table-test double-float-= double-float-hash)
(sb-ext:define-hash-table-test string-and-type-= string-hash)

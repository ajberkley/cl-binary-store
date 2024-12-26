(defpackage :example-extension
  (:use :common-lisp :cl-binary-store))

(in-package :example-extension)

(defclass blarg ()
  ((a-not-serializable :initform (lambda () "I was initialized!"))
   (b-serializable :initarg :b-serializable)))

;; Here we specialize this method to tell cl-binary-store to only
;; serialize one slot and to call initialize-instance on the object
;; after restoring it (instead of the default which assumes all slots
;; will be populated on loading)
(defmethod serializable-slot-info (object (type (eql 'blarg)))
  (make-slot-info
   :class (find-class 'blarg)
   :slot-names #(b-serializable)
   :call-initialize-instance t ;; if nil the "I was initialized!" test below fails
   :type 'blarg))

(defun test-serializable-slot-info ()
  (let* ((b (make-instance 'blarg :b-serializable "asdf"))
	 (b-restored (restore (store nil b))))
    (assert (string= (funcall (slot-value b-restored 'a-not-serializable))
		     "I was initialized!"))
    (assert (string= (slot-value b-restored 'b-serializable) "asdf"))))

;; Here is another way to do this

(defconstant +test-code+ 99)

(defclass something-else ()
  ((information :initform "Hi" :accessor information)))

(defun store-something-else (obj storage store-object)
  (store-ub8 +test-code+ storage)
  (store-ub16 12345 storage)
  (funcall store-object (format nil "Hi, I decided to write this instead of a 'something-else"))
  (funcall store-object (format nil "But actually, it told me to tell you:"))
  (funcall store-object (information obj)))

(defun restore-something-else (restore-object)
  (assert (= (funcall restore-object) 12345))
  (format t (funcall restore-object))
  (format t (funcall restore-object))
  (format t (funcall restore-object)))

(defun test-it ()
  (restore (store nil (make-instance 'something-else))))

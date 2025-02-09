(in-package :cl-binary-store)

;; Pathname code is pretty much identical to cl-store

(declaim (inline store-pathname))
(defun store-pathname (obj store-object)
  (declare (optimize speed safety) (type pathname obj) (type function store-object))
  #-sbcl (funcall store-object (pathname-host obj))
  #+sbcl (funcall store-object (host-namestring obj))
  (funcall store-object (pathname-device obj))
  (funcall store-object (pathname-directory obj))
  (funcall store-object (pathname-name obj))
  (funcall store-object (pathname-type obj))
  (funcall store-object (pathname-version obj)))

(defun restore-pathname (restore-object)
  (declare (type function restore-object))
  (handler-case
      (make-pathname
       :host #+sbcl (funcall restore-object)
       #-sbcl (funcall restore-object)
       :device (funcall restore-object)
       :directory (funcall restore-object)
       :name (funcall restore-object)
       :type (funcall restore-object)
       :version (funcall restore-object))
    (error (e) (unexpected-data "pathname" e))))

(in-package :cl-store-faster)

;; Pathname code is pretty much identical to cl-store

(defun store-pathname (obj storage)
  (declare (optimize speed safety) (type pathname obj))
  (maybe-store-reference-instead (obj storage)
    (store-ub8 +pathname-code+ storage nil)
    (store-object #-sbcl (pathname-host obj)
                  #+sbcl (host-namestring obj) storage)
    (store-object (pathname-device obj) storage)
    (store-object (pathname-directory obj) storage)
    (store-object (pathname-name obj) storage)
    (store-object (pathname-type obj) storage)
    (store-object (pathname-version obj) storage)))

(defun restore-pathname (storage)
  (with-delayed-reference
   (make-pathname
    :host (restore-object storage)
    :device (restore-object storage)
    :directory (restore-object storage)
    :name (restore-object storage)
    :type (restore-object storage)
    :version (restore-object storage))))

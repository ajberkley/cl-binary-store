(in-package :cl-store-faster)

;; Pathname code is pretty much identical to cl-store

(defun store-pathname (obj storage references)
  (declare (optimize speed safety) (type pathname obj))
  (maybe-store-reference-instead (obj storage references)
    (store-ub8 +pathname-code+ storage nil)
    #-sbcl (store-object (pathname-host obj) storage references)
    #+sbcl (store-object (host-namestring obj) storage references) ;; store-string
    (store-object (pathname-device obj) storage references)
    (store-object (pathname-directory obj) storage references)
    (store-object (pathname-name obj) storage references)
    (store-object (pathname-type obj) storage references)
    (store-object (pathname-version obj) storage references)))

(defun restore-pathname (storage references)
  (assert references)
  (make-pathname
   :host (restore-object storage references)
   :device (restore-object storage references)
   :directory (restore-object storage references)
   :name (restore-object storage references)
   :type (restore-object storage references)
   :version (restore-object storage references)))

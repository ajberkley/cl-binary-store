(defpackage :example-extension
  (:use :common-lisp :cl-binary-store)
  (:export #:test-special-serializer/deserializer
	   #:test-serializable-object-info
	   #:test-unable-to-restore-double-floats))

(in-package :example-extension)

(defclass blarg ()
  ((a-not-serializable :initform (lambda () "I was initialized!"))
   (b-serializable :initarg :b-serializable)))

;; Here we specialize this method to tell cl-binary-store to only
;; serialize one slot and to call initialize-instance on the object
;; after restoring it (instead of the default which assumes all slots
;; will be populated on loading)
(defmethod serializable-object-info ((type (eql 'blarg)))
  (values (list 'b-serializable)))

(defmethod specialized-object-constructor ((type (eql 'blarg)))
  (lambda (object-info slot-values)
    (assert (= (length slot-values) 1))
    (assert (= (length (object-info-slot-names object-info)) 1))
    (assert (eq (svref (object-info-slot-names object-info) 0) 'b-serializable))
    (make-instance 'blarg :b-serializable (nth 0 slot-values))))

(defun test-serializable-object-info ()
  (let* ((b (make-instance 'blarg :b-serializable "asdf"))
	 (b-restored (restore (store nil b))))
    (assert (string= (funcall (slot-value b-restored 'a-not-serializable))
		     "I was initialized!"))
    (assert (string= (slot-value b-restored 'b-serializable) "asdf"))
    (format t "Success!~%")))

;; Here is another way to do this

(defconstant +extension-codespace+ #x9999
  "This is our magic number / version number")
(defconstant +test-code+ 225) ;; must be in the user space land of [225 255] see basic-codespace-codes.lisp

(defclass something-else ()
  ((information :initform (format nil "Hi from slot information!") :accessor information)))

(defun store-something-else (obj storage store-object)
  (when storage
    (store-ub8/no-tag +test-code+ storage)
    (store-ub16 12345 storage))
  (funcall store-object (format nil "Hi, I decided to write this instead of a 'something-else~%"))
  (funcall store-object (format nil "But actually, it told me to tell you:~%"))
  (funcall store-object (information obj)))

(defun restore-something-else (restore-object)
  (assert (= *version-being-read* +extension-codespace+))
  (assert (= (funcall restore-object) 12345))
  (format t (funcall restore-object))
  (format t (funcall restore-object))
  (format t (funcall restore-object))
  "And here is a bonus thing returned to you")

(defun test-special-serializer/deserializer ()
  ;; Option one write the version number into the stream
  (format t "Example of writing something completely different for a 'something-else object:~%~%")
  (format t "First example writing a version number into the stream to switch codespaces~%")
  (let ((*write-version* +extension-codespace+)
	(*write-magic-number* t))
    (print (restore (store nil (make-instance 'something-else)))))
  (format t "~%~%Second example forcing the right codespace~%")
  ;; Option two just keep track of it yourself
  (let ((*write-version* +extension-codespace+)
	(*read-version* +extension-codespace+)
	(*write-magic-number* nil))
    (restore (store nil (make-instance 'something-else)))))

;; Note that in extension-codespace we have explicitly deleted support for double-floats
;; let's verify that.

(defun test-unable-to-restore-double-floats ()
  (let ((bad-output
	  (let ((*write-version* +basic-codespace+)
		(*write-magic-number* t))
	    (store nil 1.23d0))))
    (let ((*read-version* +extension-codespace+)
	  (*allow-codespace-switching* nil))
      (handler-case
	  (restore bad-output)
	(error (e)
	  (format t "Successfully denied codespace switching!~%Error was: ~A~%" e))))
    (let ((output-with-double-float
	    (let ((*write-version* +basic-codespace+)
		  (*write-magic-number* nil))
	      (store nil 1.23d0))))
      (handler-case
	  (let ((*read-version* +extension-codespace+))
	    (restore output-with-double-float))
	(error (e)
	  (format t "Interpreting of double-float not supported in our codespace!~%Error was: ~A~%" e)))
      (let ((*read-version* +basic-codespace+))
	(let ((restored (restore output-with-double-float)))
	  (if (= restored 1.23d0)
	      (format t "Successfully read double-float when we were allowed to!~%")
	      (format t "COULD NOT READ DOUBLE FLOAT BUG BUG BUG!~%")))))))
    

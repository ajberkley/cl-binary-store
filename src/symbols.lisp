(in-package :cl-binary-store)

(declaim (inline store-nil))
(defun store-nil (storage)
  (store-ub8 +nil-code+ storage nil))

(declaim (inline store-t))
(defun store-t (storage)
  (store-ub8 +t-code+ storage nil))

(declaim (inline store-boolean))
(defun store-boolean (boolean storage)
  (if boolean (store-t storage) (store-nil storage)))

(declaim (inline restore-boolean))
(defun restore-boolean (storage)
  (= (restore-ub8 storage) +t-code+))

(declaim (inline restore-nil))
(defun restore-nil ()
  nil)

(declaim (inline restore-t))
(defun restore-t ()
  t)

(declaim (notinline store-symbol))
(defun store-symbol (symbol storage eq-refs store-object)
  (declare (notinline store-simple-specialized-vector))
  (let ((symbol-package (symbol-package symbol)))
    (cond
      (symbol-package
       (maybe-store-reference-instead (symbol storage eq-refs)
	 #+debug-cbs
	 (format t "Storing symbol ~S from package ~S~%"
		 (symbol-name symbol) (package-name (symbol-package symbol)))
	 (when storage
	   (storage-write-byte storage +symbol-code+)
	   (store-string/no-refs (symbol-name symbol) storage))
         ;; Nominally we can use the eq-refs table but we don't
	 (funcall (the function store-object) (package-name symbol-package))))
      (t ;; uninterned symbols.  We don't bother de-duplicating the string representations
       #+debug-cbs (format t "Storing symbol without a package ~S~%" symbol)
       (when storage
	 (storage-write-byte storage +uninterned-symbol-code+)
	 (store-string/no-refs (symbol-name symbol) storage))))))

(define-condition missing-package (error)
  ((symbol-string :initarg :symbol-string :reader missing-package-symbol-string)
   (package-string :initarg :package-string :reader missing-package-package-string)))

(defmethod print-object ((obj missing-package) stream)
  (format stream "~S says it is from package ~S, but no such package!"
	  (missing-package-symbol-string obj)
	  (missing-package-package-string obj)))

(defun signal-missing-package (symbol-string package-string)
  (restart-case ;; TODO ADD SOME FEATURES HERE
      (error 'missing-package :symbol-string symbol-string
			      :package-string package-string)))

(declaim (inline restore-symbol))
(defun restore-symbol (storage restore-object)
  "Do not call me directly because if you called store-symbol you may have
 ended up writing a reference to the symbol object instead of the symbol object."
  (let* ((symbol-string (restore-string storage))
	 (package-string (funcall (the function restore-object)))) ;; might be a reference
      (if (find-package package-string)
	  (values (intern symbol-string package-string))
	  (signal-missing-package symbol-string package-string))))

(defun restore-uninterned-symbol (storage)
  "You can call this directly since we never store references"
  (values (make-symbol (restore-string storage))))

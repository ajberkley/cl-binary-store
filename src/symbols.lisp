(in-package :cl-store-faster)

(declaim (inline store-nil))
(defun store-nil (storage)
  (store-ub8 +nil-code+ storage nil))

(declaim (inline store-t))
(defun store-t (storage)
  (store-ub8 +t-code+ storage nil))

(declaim (inline store-boolean))
(defun store-boolean (boolean storage)
  (if boolean (store-t storage) (store-nil storage)))

(declaim (inline constantly-nil))
(defun restore-nil (storage)
  (declare (ignorable storage))
  nil)

(declaim (inline constantly-t))
(defun restore-t (storage)
  (declare (ignorable storage))
  t)

(declaim (inline store-symbol))
(defun store-symbol (symbol storage references)
  (let ((symbol-package (symbol-package symbol)))
    (cond
      (symbol-package
       (maybe-store-reference-instead (symbol storage references)
	 (store-ub8 +symbol-code+ storage nil)
	 #+debug-csf
	 (format t "Storing symbol ~S from package ~S~%"
		 (symbol-name symbol) (package-name (symbol-package symbol)))
	 (store-object (symbol-name symbol) storage references)
	 (store-object (package-name symbol-package) storage references)))
      (t ;; symbols without a package, (symbol-package (gensym)) -> nil
       #+debug-csf (format t "Storing symbol without a package ~S~%" symbol)
       ;;these can never be the same
       (store-ub8 +gensym-code+ storage nil)
       (store-object (symbol-name symbol) storage references)))))

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
(defun restore-symbol (storage references)
  (let* ((symbol-string (restore-object storage references))
	 (package-string (restore-object storage references)))
    (if (find-package package-string)
	(intern symbol-string package-string)
	(signal-missing-package symbol-string package-string))))

(defun restore-gensym (storage references)
  ;; Can never be referred to by anything
  (let ((symbol-string (restore-object storage references)))
    (make-symbol symbol-string)))

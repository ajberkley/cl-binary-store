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
(defun store-symbol (symbol storage)
  (cond
    ((symbol-package symbol)
     (maybe-store-reference-instead (symbol storage)
       (store-ub8 +symbol-code+ storage nil)
       ;; (format t "Storing ~S package ~S~%"
       ;; 	   (symbol-name symbol) (package-name (symbol-package symbol)))
       ;; TODO maybe we can skip storing reference ids for these strings?
       (store-object (symbol-name symbol) storage)
       (store-object (package-name (symbol-package symbol)) storage)))
    (t ;; symbols without a package, (symbol-package (gensym)) -> nil
     ;;(format t "Symbol without a package ~S~%" symbol)
     ;;these can never be the same
     (store-ub8 +gensym-code+ storage nil)
     (store-object (symbol-name symbol) storage))))

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
(defun restore-symbol (storage)
  (with-delayed-reference
    (let* ((symbol-string (restore-object storage))
	   (package-string (restore-object storage)))
      (if (find-package package-string)
	  (intern symbol-string package-string)
	  (signal-missing-package symbol-string package-string)))))

(defun restore-gensym (storage)
  ;; Can never be referred to by anything
  (let ((symbol-string (restore-object storage)))
    (make-symbol symbol-string)))

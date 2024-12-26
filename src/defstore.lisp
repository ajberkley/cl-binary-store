(in-package :cl-binary-store)

(defvar *ref-tables* (make-hash-table :test 'eql) "Maps name -> ref-table")
(defvar *store-info* (make-hash-table :test 'equal) "Maps type -> `store-info'")
(defvar *restore-info* (make-hash-table :test 'eql) "Maps code -> `restore-info'")

(defstruct restore-info
  (restore-function-code nil))
  
(defstruct store-info
  (type nil)
  (reference-phase-code nil)
  (storage-phase-code nil))

(defstruct ref-table
  (name nil)
  (construction-code nil))

(defun register-references& (table-name construction-code)
  (let* ((new-ref-table (make-ref-table :name table-name :construction-code construction-code))
         (pre-existing (gethash table-name *ref-tables*)))
    (when (and pre-existing (not (equalp pre-existing new-ref-table)))
      (cerror "REPLACE IT" (format nil "Already extant reference table ~A" table-name))
      (remhash pre-existing *ref-tables*))
    (setf (gethash table-name *ref-tables*) new-ref-table))
  (values))
  
(defmacro register-references (table-name construction-code)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-references& ',table-name ',construction-code)))

(defmacro with-reference-tables ((track-references) &body body)
  (let ((let-bindings nil))
    (maphash (lambda (table-name ref-table)
               (push (list table-name `(when ,track-references
                                         ,(ref-table-construction-code ref-table)))
                     let-bindings))
             *ref-tables*)
    `(let (,@let-bindings)
       (declare (dynamic-extent ,@(mapcar #'first let-bindings)))
       ,@body)))
  
(defmacro map-reference-tables (func)
  (let ((code nil))
    (maphash (lambda (table-name ref-table)
               (declare (ignorable ref-table))
               (push `(funcall ,func ',table-name ,table-name) code))
             *ref-tables*)
    `(progn ,@code)))

(defun update-store-info
    (type store-function-signature
     &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
       check-for-ref-in write-phase-code)
  (labels ((maybe-wrap-code-with-ref-check-for-store-phase (code)
             (if check-for-ref-in
                 `(unless (referenced-already obj storage ,check-for-ref-in)
                    ,@(when write-phase-code
                        `((store-ub8 ,write-phase-code storage nil)))
                    ,code)
                 code))
           (maybe-wrap-code-with-ref-check-for-ref-phase (code)
             (if check-for-ref-in
                 `(unless (check-reference obj ,check-for-ref-in t)
                    ,code)
                 code)))
    (loop for param in (cdr store-function-signature)
          do (unless (or (member param '(obj storage store-object)) (gethash param *ref-tables*))
               (error (format nil "While parsing DEFSTORE for ~A, ~A is an unknown parameter of DEFSTORE~%~
                                   it must be one of OBJ, STORAGE, STORE-OBJECT or a reference table name from~%~
                                   REGISTER-REFERENCES" type param))))
    (let* ((write-phase-code store-function-signature)
           (reference-phase-code (if call-during-reference-phase-provided-p
                                     call-during-reference-phase
                                     write-phase-code))
           (si (make-store-info
                :type type
                :reference-phase-code (maybe-wrap-code-with-ref-check-for-ref-phase reference-phase-code)
                :storage-phase-code (maybe-wrap-code-with-ref-check-for-store-phase write-phase-code))))
      (unless (or (null (gethash type *store-info*))
                  (equalp (gethash type *store-info*) si))
        (cerror "REPLACE IT" (format nil "Replacing already existing store code for type ~A" type)))
      (setf (gethash type *store-info*) si))))

(defmacro defstore (type store-function-signature &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
						    check-for-ref-in write-phase-code)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-store-info ',type ',store-function-signature
			,@(if call-during-reference-phase-provided-p
			      `(:call-during-reference-phase ',call-during-reference-phase))
			:check-for-ref-in ',check-for-ref-in
			:write-phase-code ',write-phase-code)))

(defun update-restore-info (code restore-function-signature)
  (let ((code (eval code)))
    (loop for param in (cdr restore-function-signature)
          do (unless (member param '(storage references restore-object))
               (error (format nil "While parsing DEFRESTORE for code ~A, found unknown param ~A, it must be one~%~
                                 of STORAGE, REFERENCES, or RESTORE-OBJECT" code param))))
    (let ((ri (make-restore-info :restore-function-code restore-function-signature)))
      (unless (or (null (gethash code *restore-info*))
                  (equalp (gethash code *restore-info*) ri))
        (cerror "REPLACE IT" (format nil "Replacing already existing restore code for code ~A" code)))
      (setf (gethash code *restore-info*) ri))))

(defmacro defrestore (code restore-function-signature)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-restore-info ,code ',restore-function-signature)))

(defun store-object/phase (obj store-info-accessor)
  ;; This assumes that the caller has defined OBJ, STORAGE, STORE-OBJECT, and the various
  ;; tables in *ref-tables*.  I don't have the energy to make this all hygenic.
  `(etypecase ,obj
     ,@(strict-subtype-ordering
	(let ((type-dispatch-table nil))
	  (maphash (lambda (type store-info)
                     (push (list type
                                 (funcall store-info-accessor store-info))
                           type-dispatch-table))
		   *store-info*)
	  type-dispatch-table)
	:key #'first)))
  
(defmacro store-object/storage-phase (obj)
  (declare (ignorable obj))
  (assert (string= (symbol-name obj) "OBJ"))
  (store-object/phase obj 'store-info-storage-phase-code))

(defmacro store-object/reference-phase (obj)
  (declare (ignorable obj))
  (assert (string= (symbol-name obj) "OBJ"))
  (store-object/phase obj 'store-info-reference-phase-code))

(defun make-read-dispatch-table (code-to-dispatch-on)
  ;; Assumes this is in a context where STORAGE, REFERENCES, and RESTORE-OBJECT are defined
  (let ((code nil))
    (maphash (lambda (dispatch-code restore-info)
               (push (list dispatch-code (restore-info-restore-function-code restore-info)) code))
             *restore-info*)
    (setf code (sort code #'< :key #'first))
    `(case ,code-to-dispatch-on
       ,@code
       (otherwise
        (error 'simple-error :format-control "Unknown code ~A found in stream"
                             :format-arguments (list ,code-to-dispatch-on))))))

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

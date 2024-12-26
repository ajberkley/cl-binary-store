(in-package :cl-binary-store)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  
  (defmacro defstore
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
          (cerror "REPLACE IT" (format nil "Replacing already existing store code for type ~A" 123)))
        (setf (gethash type *store-info*) si))))

  (defmacro defrestore (code restore-function-signature)
    (let ((code (eval code)))
      (loop for param in (cdr restore-function-signature)
            do (unless (member param '(storage references restore-object))
                 (error (format nil "While parsing DEFRESTORE for code ~A, found unknown param ~A, it must be one~%~
                                 of STORAGE, REFERENCES, or RESTORE-OBJECT" code param))))
      (let ((ri (make-restore-info :restore-function-code restore-function-signature)))
        (unless (or (null (gethash code *restore-info*))
                    (equalp (gethash code *restore-info*) ri))
          (cerror "REPLACE IT" (format nil "Replacing already existing restore code for code ~A" 123)))
        (setf (gethash code *restore-info*) ri))))

  (defmacro store-object/storage-phase (obj)
    ;; This assumes that the caller has defined OBJ, STORAGE, STORE-OBJECT, and the various tables in *ref-tables*
    ;; TODO CHANGE TO DUAL LAYER DISPATCH TO HELP WITH CODE GEN
    ;; FIXNUM -> , structure-object ->, standard-object ->, vector ->, array ->, symbol ->
    (assert (string= (symbol-name obj) "OBJ"))
    `(etypecase obj
       ,@(strict-subtype-ordering
	  (let ((type-dispatch-table nil))
	    (maphash (lambda (type store-info)
                       (push (list type
                                   (store-info-storage-phase-code store-info))
                             type-dispatch-table))
		     *store-info*)
	    type-dispatch-table)
	  :key #'first)))

  (defmacro store-object/reference-phase (obj)
  ;; This assumes that the caller has defined OBJ, STORAGE, STORE-OBJECT, and the various tables in *ref-tables*
  ;; TODO CHANGE TO DUAL LAYER DISPATCH TO HELP WITH CODE GEN
  ;; FIXNUM -> , structure-object ->, standard-object ->, vector ->, array ->, symbol ->
  (assert (string= (symbol-name obj) "OBJ"))
  `(etypecase obj
     ,@(strict-subtype-ordering
	(let ((type-dispatch-table nil))
	  (maphash (lambda (type store-info)
                     (push (list type
                                 (store-info-reference-phase-code store-info))
                           type-dispatch-table))
		   *store-info*)
	  type-dispatch-table)
	:key #'first)))

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
                               :format-arguments (list ,code-to-dispatch-on)))))))

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

(register-references num-eq-refs (make-hash-table :test #'eq))
(register-references double-float-refs (make-hash-table :test #'double-float-=))
;;(register-references string-refs (make-hash-table :test #'string-and-type-=))
(register-references eq-refs (make-hash-table :test #'eq))

(defconstant +ub8-code+ 0)
(defconstant +ub16-code+ 1)
(defconstant +ub32-code+ 2)
(defconstant +fixnum-code+ 3)
(defconstant +cons-code+ 4)
(defconstant +nil-code+ 5)
(defconstant +sb8-code+ 6)
(defconstant +sb16-code+ 7)
(defconstant +sb32-code+ 8)
(defconstant +bignum-code+ 9)
(defconstant +single-float-code+ 10)
(defconstant +double-float-code+ 11)
(defconstant +double-float-zero-code+ 12)
(defconstant +ratio-code+ 13)
(defconstant +complex-code+ 14)
(defconstant +complex-double-float-code+ 15)
(defconstant +complex-single-float-code+ 16)
(defconstant +symbol-code+ 17)
(defconstant +uninterned-symbol-code+ 18)
(defconstant +structure-object-code+ 19)
(defconstant +standard-object-code+ 20)
(defconstant +t-code+ 21)
(defconstant +referrer-ub8-code+ 22)
(defconstant +referrer-ub16-code+ 23)
(defconstant +referrer-ub32-code+ 24)
(defconstant +referrer-code+ 25)
(defconstant +record-reference-ub8-code+ 26)
(defconstant +record-reference-ub16-code+ 27)
(defconstant +record-reference-ub32-code+ 28)
(defconstant +record-reference-code+ 29)
(defconstant +simple-specialized-vector-code+ 30)
(defconstant +simple-vector-code+ 31)
(defconstant +simple-specialized-array-code+ 32)
(defconstant +array-code+ 33)
(defconstant +slot-info-code+ 34)
(defconstant +unbound-code+ 35)
(defconstant +pathname-code+ 36)
(defconstant +hash-table-code+ 37)
(defconstant +simple-base-string-code+ 38)
(defconstant +simple-string-code+ 39)
(defconstant +action-code+ 40)

(eval-when (:compile-toplevel :load-toplevel :execute) ;; so the tables are available for debugging
  ;; NUMBERS
  (defstore fixnum (store-fixnum obj storage) :call-during-reference-phase nil)

  (defrestore +ub8-code+ (restore-ub8 storage))
  (defrestore +ub16-code+ (restore-ub16 storage))
  (defrestore +ub32-code+ (restore-ub32 storage))
  (defrestore +fixnum-code+ (restore-fixnum storage))
  (defrestore +sb8-code+ (restore-sb8 storage))
  (defrestore +sb16-code+ (restore-sb16 storage))
  (defrestore +sb32-code+ (restore-sb32 storage))

  (defstore bignum (store-bignum obj storage) :check-for-ref-in num-eq-refs
    :call-during-reference-phase nil)
  (defrestore +bignum-code+ (restore-bignum storage))

  (defstore single-float (store-single-float obj storage) :call-during-reference-phase nil)
  (defrestore +single-float-code+ (restore-single-float storage))

  (defstore double-float (store-double-float obj storage double-float-refs))
  (defrestore +double-float-code+ (restore-double-float storage))
  (defrestore +double-float-zero-code+ (restore-double-float-zero))

  (defstore ratio (store-ratio obj storage num-eq-refs))
  (defrestore +ratio-code+ (restore-ratio restore-object))

  (defstore complex (store-complex obj storage store-object) :check-for-ref-in num-eq-refs)
  (defrestore +complex-code+ (restore-complex restore-object))
  (defrestore +complex-double-float-code+ (restore-complex-double-float storage))
  (defrestore +complex-single-float-code+ (restore-complex-single-float storage))

;;; CONS

  (defstore cons
      (store-cons obj storage eq-refs store-object)
    :call-during-reference-phase (search-cons obj eq-refs store-object))

  (defrestore +cons-code+ (restore-cons storage restore-object))

;;; T and NIL (STORED DISJOINT FROM SYMBOLS)

  (defstore null (store-nil storage) :call-during-reference-phase nil)
  (defrestore +nil-code+ (restore-nil))
  (defstore (eql t) (store-t storage) :call-during-reference-phase nil)
  (defrestore +t-code+ (restore-t))

;;; INTERNED SYMBOLS / KEYWORDS / UNINTERNED SYMBOLS
  (defstore (and symbol (not null) (not (eql t))) (store-symbol obj storage eq-refs store-object))
  (defrestore +symbol-code+ (restore-symbol storage restore-object))
  (defrestore +uninterned-symbol-code+ (restore-uninterned-symbol storage))

  ;; STRUCTURE-OBJECTS (defstruct) and STANDARD-CLASS (defclass)
  (defstore structure-object (store-struct obj storage eq-refs store-object))
  (defstore standard-object (store-standard-object obj storage eq-refs store-object))
  (defrestore +structure-object-code+ (restore-struct restore-object))
  (defrestore +standard-object-code+ (restore-standard-object restore-object))

  ;; REFERENCES
  (defrestore +referrer-ub8-code+ (restore-referrer-ub8 storage references))
  (defrestore +referrer-ub16-code+ (restore-referrer-ub16 storage references))
  (defrestore +referrer-ub32-code+ (restore-referrer-ub32 storage references))
  (defrestore +referrer-code+ (restore-referrer storage references))

  (defrestore +record-reference-ub8-code+ (restore-reference-id-ub8 storage references restore-object))
  (defrestore +record-reference-ub16-code+ (restore-reference-id-ub16 storage references restore-object))
  (defrestore +record-reference-ub32-code+ (restore-reference-id-ub32 storage references restore-object))
  (defrestore +record-reference-code+ (restore-reference-id storage references restore-object))

  ;; SIMPLE VECTORS
  ;;  Here we are trying to move the maybe-store-reference-instead to the caller to avoid the notinline dispatch?
  ;;  dunno if its easier for the extension writer to put in the check where they want or to magically put it
  ;;  here.  It helps with dispatch a bit.  HOW MANY TIMES IS IT *JUST* A WRAPPER USE?
  #+sbcl
  (defstore (simple-array * (*)) (store-simple-specialized-vector obj storage) :check-for-ref-in eq-refs)
  (defrestore +simple-specialized-vector-code+ (restore-simple-specialized-vector storage))

  (defstore simple-vector (store-simple-vector obj storage store-object) :check-for-ref-in eq-refs)
  (defrestore +simple-vector-code+ (restore-simple-vector storage restore-object))

  ;; SIMPLE ARRAYS
  #+sbcl
  (defstore (and (simple-array * *) (not (simple-array t *)))
      (store-simple-specialized-array obj storage) :check-for-ref-in eq-refs :call-during-reference-phase nil)
  (defrestore +simple-specialized-array-code+ (restore-simple-specialized-array storage))

  ;; COMPLEX VECTORS AND ARRAYS
  (defstore array (store-array obj storage eq-refs store-object))
  (defrestore +array-code+ (restore-array storage restore-object))

  (defstore slot-info (store-slot-info obj storage eq-refs store-object))
  (defrestore +slot-info-code+ (restore-slot-info storage restore-object))

  ;; UNBOUND MARKER
  (defrestore +unbound-code+ (restore-unbound))

  ;; PATHNAMES
  (defstore pathname (store-pathname obj store-object)
    :check-for-ref-in eq-refs :write-phase-code +pathname-code+)
  (defrestore +pathname-code+ (restore-pathname restore-object))

  ;; HASH-TABLE
  (defstore hash-table (store-hash-table obj storage store-object) :check-for-ref-in eq-refs)
  (defrestore +hash-table-code+ (restore-hash-table storage restore-object))
  
  ;; STRINGS
  ;; I made a mess of internal dispatch around this so have to clean it up to take advantage of check-for-ref-in.
  ;; TODO clean this up
  (defstore simple-base-string (store-simple-base-string obj storage) :check-for-ref-in eq-refs
    :call-during-reference-phase nil)
  (defrestore +simple-base-string-code+ (restore-simple-base-string storage))

  (defstore simple-string (store-simple-string obj storage) :check-for-ref-in eq-refs
    :call-during-reference-phase nil)
  (defrestore +simple-string-code+ (restore-simple-string storage))

  (defstore action (store-action& obj storage store-object))
  (defrestore +action-code+ (restore-action& storage references restore-object))

  ;; On sbcl a condition is neither a structure-object nor a standard-object, but has slots and all
  #+sbcl
  (defstore condition (store-standard-object obj storage eq-refs store-object)))

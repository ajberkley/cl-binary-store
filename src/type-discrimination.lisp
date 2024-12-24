;; This has to be the most hideous code I've written in awhile,
(in-package :cl-store-faster)

(defun binned-disjoint-types (type-specifiers)
  "Returns an alist with keys being a type and values being sub-types of the values.
 CL-USER> (binned-disjoint-types '(fixnum (unsigned-byte 8) standard-object)) ->
 ((STANDARD-OBJECT) (FIXNUM (UNSIGNED-BYTE 8)))"
  (loop with bins = nil
	while type-specifiers
	for new-type = (pop type-specifiers)
	do
	   (push (cons new-type nil) bins)
	   (let ((bins bins))
	     (setf type-specifiers
		   (loop
		     with remaining-types = type-specifiers
		     with bin-definition-changed = t
		     with bin = (pop bins)
		     while (and remaining-types bin-definition-changed)
		     for bin-type = (car bin)
		     do (setf bin-definition-changed nil)
			(setf remaining-types 
			      (loop for type in remaining-types
				    while type
				    for type-is-a-subtype-of-bin
				      = (subtypep type bin-type)
				    for type-is-a-supertype-of-bin
				      = (subtypep bin-type type)
				    when (and type-is-a-supertype-of-bin
					      type-is-a-subtype-of-bin)
				      do (error "~A is identical to ~A"
						bin-type type)
				    if type-is-a-supertype-of-bin
				      do (setf bin-definition-changed t)
					 (let ((old-super-type (car bin)))
					   (setf (car bin) type)
					   (setf bin-type type)
					   (push old-super-type (cdr bin)))
				    else
				      if type-is-a-subtype-of-bin
					do (push type (cdr bin))
				    else
				      collect type))
		     unless bin-definition-changed
		       do
			  (setf bin (pop bins))
		     finally (return remaining-types))))
	finally (return bins)))

(binned-disjoint-types '((unsigned-byte 8) (unsigned-byte 16)))

(defun satisfies-test (x) (< x 3))
(deftype satisfies-something () '(satisfies satisfies-test))
(defstruct another)
(defstruct blarg)
(defstruct (includes-blarg (:include blarg)))

(defclass class-a () ())
(defclass class-b (class-a) ())
(defclass class-c (class-a) ())

(defparameter *many-types*
  `(real complex ratio integer fixnum
    (complex double-float) (complex single-float)
    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
    (unsigned-byte 64)
    bignum
    standard-object
    standard-class
    blarg
    includes-blarg
    another
    satisfies-something
    vector
    simple-vector
    array
    simple-array
    null
    double-float
    (eql t)
    (simple-array (unsigned-byte 8) (*))
    (simple-array single-float (*))
    (simple-array double-float (*))
    (simple-array fixnum (*))
    (simple-array fixnum *)))

(defvar *good-top-levels* '(fixnum cons single-float nil simple-vector simple-array vector
			    array structure-object structure-class t))

(defun build-discriminator-tree
    (type-specifiers &optional (top-level-type-bins *good-top-levels*))
  (let* ((fixed-bins top-level-type-bins)
	 (trimmed-type-specifiers (remove-if
				   (lambda (type) (member type fixed-bins :test 'equal))
				   type-specifiers))
	 (bins (loop with remaining-types = trimmed-type-specifiers
		     for bin-type in fixed-bins
		     for sub-types = (remove-if-not
				      (lambda (type)
					(and (not (eq type bin-type)) (subtypep type bin-type)))
				      remaining-types)
		     do (setf remaining-types (remove-if
					       (lambda (type) (member type sub-types :test 'equal))
					       remaining-types))
		     collect (cons bin-type sub-types))))
    (labels ((walk (bins)
	       (loop for (parent-type . sub-types) in bins
		     collect (cons parent-type (walk (binned-disjoint-types sub-types))))))
      (walk bins))))

(defun analyze-discriminators
    (type-specifiers &optional (top-level-type-bins *good-top-levels*))
  "A nice discrimator tree:
 (simulate-discriminators *many-types*
   '(cons fixnum null (eql t) single-float array number structure-object standard-object t))"
  (let ((results (build-discriminator-tree type-specifiers top-level-type-bins)))
    (labels ((print-it (bins parent &optional (spacing "") (num-branches 0)
				      (num-instructions 0) (function-calls 0))
	       (loop for (type . sub-types) in bins
		     for discriminator = `(lambda (x)
					    (declare 
					     (optimize
					      (speed 3) (safety 0) (debug 0))
					     (type (and ,parent
							,@(loop for fail in failed-types
							      	collect `(not ,fail)))
						   x))
					    (typep x ',type))
		     for code
		       = (with-output-to-string (str)
			   (disassemble (compile nil discriminator) :stream str))
		     do
			(incf function-calls (cl-ppcre:count-matches "FDEF" code))
			(incf num-branches (+ (cl-ppcre:count-matches "JEQ" code)
					      (cl-ppcre:count-matches "JE" code)
					      (cl-ppcre:count-matches "JB" code)
					      (cl-ppcre:count-matches "JA" code)
					      (cl-ppcre:count-matches "JNE" code)
					      (cl-ppcre:count-matches "CMOV" code)))
			(incf num-instructions (count #\Newline code))
			(format t "~A~A ~A compares, ~A instructions, and ~A function-calls~%"
				spacing type num-branches num-instructions function-calls)
			(print-it sub-types type 
				  (concatenate 'string " " spacing)
				  num-branches
				  num-instructions
				  function-calls)
		     collect type into failed-types)))
      (print-it results t))))

(defun build-store-dispatch (code-info value storage references)
  "code-info is a `code-info', information about types and functions for
 serialization/deserialization.  This function builds etypecase dispatch
 code"
  (let ((dispatch-tree
	  (build-discriminator-tree
	   (print 
	    (loop
	      for key being the hash-keys of *code-info*
	      when (not (numberp key)) collect key)))))
    (print dispatch-tree)
    (labels ((get-code-info (type)
	       (gethash type code-info))
	     (walk (tree parent)
	       (loop for (type . sub-types) in tree
		     for code-info = (print (get-code-info type))
		     collect `(,type
			       ,@(if code-info
				     `((,(code-info-store-func-name code-info)
					,value
					,storage
					,@(when (code-info-store-references code-info)
					    `(,references))))
				     (list
				      `(declare (type (and ,parent 
							   ,@(loop for fail in failed-types
								   collect `(not ,fail))))
						,value)
				      `(etypecase ,value
					 ,(walk sub-types type)))))
		     collect type into failed-types)))
  `(etypecase ,value
     ,(walk dispatch-tree t)))))
     
;; Let's compare this to sbcl generated code

(defun trust-sbcl (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)
		     ;; (sb-c::jump-table 3)
		     ))
  (typecase x
    ((unsigned-byte 8) 0)
    ((unsigned-byte 16) 1)
    ((unsigned-byte 32) 2)
    ((unsigned-byte 64) 3)
    (fixnum 4)
    (blarg 5)
    (includes-blarg 6)
    ((simple-array double-float (*)) 7)
    (simple-array 8)
    (vector 9)
    (array 10)
    (ratio 11)
    (complex 12)))

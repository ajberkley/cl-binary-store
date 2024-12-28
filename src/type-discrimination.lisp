(in-package :cl-binary-store)

(defvar *preferred-dispatch-order*
  '(cons fixnum symbol single-float double-float vector array structure-object
    standard-object bignum t)
  "To change this, setf it and call (rebuild-dispatch).
 In benchmarking you will see factors of two speed differences by tweaking this
 (test-cl-binary-store-on-data (long-list-of-small-integers) :track-references nil)")

(defun strict-subtype-ordering (type-specs &key (key #'identity)
					     (type-groups *preferred-dispatch-order*))
  ;; This sort of works, but some weird issues I haven't debugged yet
  ;; with the type hierarchy.
  ;; We need to order these by subtypep, a simple sort won't work
  ;; because we have disjoint sets.  So first, we have to sort into
  ;; disjoint sets, then sort, then recombine.
  (let* ((groups (make-array (length type-groups) :initial-element nil)))
    (loop for item in type-specs
	  do (loop for count below (length type-groups)
		   for type-group in type-groups
		   until (subtypep (funcall key item) type-group)
		   finally
		      (push item (svref groups count))))
    (loop for g across groups
	  appending (stable-sort (reverse g) #'subtypep :key key))))


;; This has to be the most hideous code I've written in awhile,

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
(defstruct blarg-td)
(defstruct (includes-blarg-td (:include blarg-td)))

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
    blarg-td
    includes-blarg-td
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

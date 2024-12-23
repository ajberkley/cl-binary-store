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

(defun simulate-discriminators
    (type-specifiers &optional (top-level-type-bins '(t)))
  "A nice discrimator tree:
 (simulate-discriminators *many-types*
   '(cons fixnum null (eql t) single-float array number structure-object standard-object t))"
  ;; We want to prefer directly tagged elements at the high level, so we don't
  ;; actually try to build the type-tree, we just manually put them first
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
      (let ((results (walk bins)))
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
	  (print-it results t)
	  results)))))


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
;; This just generates a comparison tree.
;; (simulate-discriminators '((unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 64)
;; 			   fixnum blarg includes-blarg array simple-array vector
;; 			   (simple-array double-float (*)) ratio complex))
;; gives me

;; T 0 compares, 6 instructions, and 0 function-calls
;;  COMPLEX 2 compares, 21 instructions, and 0 function-calls
;;  RATIO 4 compares, 33 instructions, and 0 function-calls
;;  ARRAY 6 compares, 46 instructions, and 0 function-calls
;;   SIMPLE-ARRAY 7 compares, 57 instructions, and 0 function-calls
;;   VECTOR 8 compares, 66 instructions, and 0 function-calls
;;    (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 9 compares, 75 instructions, and 0 function-calls
;;  BLARG 8 compares, 60 instructions, and 0 function-calls
;;   INCLUDES-BLARG 9 compares, 71 instructions, and 0 function-calls
;;  FIXNUM 9 compares, 69 instructions, and 0 function-calls
;;  (UNSIGNED-BYTE 64) 15 compares, 91 instructions, and 0 function-calls
;;   (UNSIGNED-BYTE 16) 16 compares, 101 instructions, and 0 function-calls
;;    (UNSIGNED-BYTE 8) 17 compares, 111 instructions, and 0 function-calls

; disassembly for TRUST-SBCL
; Size: 146 bytes. Origin: #x55410426                         ; TRUST-SBCL
; 26:       48F7C201FEFFFF   TEST RDX, -511
; 2D:       7505             JNE L1
; 2F:       31D2             XOR EDX, EDX
; 31: L0:   C9               LEAVE
; 32:       F8               CLC
; 33:       C3               RET
; 34: L1:   48F7C20100FEFF   TEST RDX, -131071
; 3B:       7471             JEQ L9
; 3D:       488515C4FFFFFF   TEST RDX, [RIP-60]               ; [#x55410408] = #xFFFFFFFE00000001
; 44:       7461             JEQ L8
; 46:       F6C201           TEST DL, 1
; 49:       7455             JEQ L7
; 4B:       8D42FD           LEA EAX, [RDX-3]
; 4E:       A80F             TEST AL, 15
; 50:       7513             JNE L2
; 52:       8B4201           MOV EAX, [RDX+1]
; 55:       81784D5B010000   CMP DWORD PTR [RAX+77], 347
; 5C:       7507             JNE L2
; 5E:       BA0A000000       MOV EDX, 10
; 63:       EBCC             JMP L0
; 65: L2:   488D4AF1         LEA RCX, [RDX-15]
; 69:       F6C10F           TEST CL, 15
; 6C:       7516             JNE L3
; 6E:       8A09             MOV CL, [RCX]
; 70:       8BC1             MOV EAX, ECX
; 72:       3C81             CMP AL, -127
; 74:       7323             JAE L6
; 76:       8BC1             MOV EAX, ECX
; 78:       3C15             CMP AL, 21
; 7A:       7416             JEQ L5
; 7C:       8BC1             MOV EAX, ECX
; 7E:       2C21             SUB AL, 33
; 80:       3C08             CMP AL, 8
; 82:       7607             JBE L4
; 84: L3:   BA17010050       MOV EDX, #x50000117              ; NIL
; 89:       EBA6             JMP L0
; 8B: L4:   BA18000000       MOV EDX, 24
; 90:       EB9F             JMP L0
; 92: L5:   BA16000000       MOV EDX, 22
; 97:       EB98             JMP L0
; 99: L6:   BA0E000000       MOV EDX, 14
; 9E:       EB91             JMP L0
; A0: L7:   BA08000000       MOV EDX, 8
; A5:       EB8A             JMP L0
; A7: L8:   BA04000000       MOV EDX, 4
; AC:       EB83             JMP L0
; AE: L9:   BA02000000       MOV EDX, 2
; B3:       E979FFFFFF       JMP L0

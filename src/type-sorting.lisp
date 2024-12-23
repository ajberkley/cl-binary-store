;; This has to be the most hideous code I've written in awhile,

(defun build-type-tree (type-specifiers)
  (labels ((disjoint-types (type-specifiers)
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
					       when (and type-is-a-supertype-of-bin type-is-a-subtype-of-bin)
						    do (error "~A is identical to ~A" bin-type type)
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
		   finally (return bins))))
    ;; We want to prefer directly tagged elements at the high level, so we don't
    ;; actually try to build the type-tree, we just manually put them first
    (let* ((fixed-bins '(fixnum cons single-float t))
	   (trimmed-type-specifiers (remove-if (lambda (type) (member type fixed-bins)) type-specifiers))
	   (bins (loop with remaining-types = trimmed-type-specifiers
		       for bin-type in fixed-bins
		       for sub-types = (remove-if-not (lambda (type)
							(and (not (eq type bin-type)) (subtypep type bin-type)))
						      trimmed-type-specifiers)
		       do (setf remaining-types (remove-if (lambda (type) (member type sub-types)) remaining-types))
		       collect (cons bin-type sub-types))))
      (labels ((walk (bins)
		 (loop for (parent-type . sub-types) in bins
		       collect (cons parent-type (walk (disjoint-types sub-types))))))
	(let ((results (walk bins)))
	  (labels ((print-it (bins parent &optional (spacing "") (num-compares 0)
					    (num-instructions 0) (function-calls 0))
		     (loop for (type . sub-types) in bins
			   for discriminator = `(lambda (x)
						  (declare (optimize (speed 3)
								     (safety 0)
								     (debug 0))
							   (type ,parent x))
						  (typep x ',type))
			   for discriminator-code
			     = (with-output-to-string (str)
				  (disassemble (compile nil discriminator) :stream str))
			   do
			      (incf function-calls (cl-ppcre:count-matches "FDEF" discriminator-code))
			      (incf num-compares (+ (cl-ppcre:count-matches "CMOV" discriminator-code)
						    (cl-ppcre:count-matches "TEST" discriminator-code)
						    (cl-ppcre:count-matches "CMP" discriminator-code)
						    (cl-ppcre:count-matches "JEQ" discriminator-code)
						    (cl-ppcre:count-matches "JNEQ" discriminator-code)))
			      (incf num-instructions (count #\Newline discriminator-code))
			      (format t "~A~A ~A compares ~A instructions ~A function-calls~%"
				      spacing type num-compares num-instructions function-calls)
			      (print-it sub-types type 
					(concatenate 'string " " spacing)
					num-compares
					num-instructions
					function-calls))))
	    (print-it results t)
	    ;; results
	    (values)
	    ))))))
	
		     
  

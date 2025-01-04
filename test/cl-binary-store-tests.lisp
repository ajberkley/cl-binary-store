(defpackage #:cl-binary-store-tests
  (:use #:common-lisp #:parachute #:cl-binary-store))

(in-package #:cl-binary-store-tests)

(define-test error-if-weird-settings
  (let ((*track-references* t)
	(*support-shared-list-structures* nil))
    (is '= (restore (store nil 1)) 1))
  (let ((*track-references* nil)
	(*support-shared-list-structures* t))
    (fail (restore (store nil 1))))
  (let ((*track-references* nil)
	(*support-shared-list-structures* nil))
    (is '= (restore (store nil 1)) 1)))

(define-test test-very-basic-list-cdr-circularity
  (let ((a (list 123 456))
	(*track-references* t)
	(*support-shared-list-structures* t))
    (setf (cddr a) a)
    (let ((result (restore-from-vector (store-to-vector a))))
      ;; (let ((*print-circle* t))
      ;; 	(print result))
      (is '= (first result) 123)
      (is '= (second result) 456)
      (is 'eq (cddr result) result))
    (let ((a (cons 1234 nil)))
      (setf (cdr a) a)
      (let ((result (restore (store nil a))))
	(is '= 1234 (car result))
	(is 'eq (cdr result) result)))))

(define-test test-very-basic-list-car-circularity
  (let ((a (list nil "abcd"))
	(*support-shared-list-structures* t))
    (setf (first a) a)
    (let ((result (restore-from-vector (store-to-vector a))))
      (is 'eq (first result) result)
      (is 'equalp (second result) "abcd")
      (false (cddr result)))))

(define-test test-non-basic-circularity
  (let ((a (list 123 456))
	(*support-shared-list-structures* t))
    (setf (cdr (last a)) (nthcdr 1 a)) ;; loop back to second element
    (let ((result (restore-from-vector (store-to-vector a))))
      (is '= (first result) 123)
      (is '= (cadr result) 456)
      (is 'eq (cddr result) (cdr result)))))

(define-test cars-are-stored-in-reference-tables
  (let* ((a (make-list 3 :initial-element (list 1 2 3)))
	 (restored-a (restore (store-to-vector a))))
    (is 'equal (first a) (first restored-a))
    (loop for elt in restored-a
	  do (is 'eql elt (first restored-a)))))

(define-test dotted-lists
  (let ((a '(1 2 3 4 . 5)))
    (is 'equal a (restore (store nil a)))))

;; This test uses too much memory and is too slow to run regularly...
(define-test up-to-ub32-references-work
  (let* ((elts (loop for i fixnum from 0 below 50000 ;;5000000
		     collect (format nil "~A" i)))
	 (double-elts (append elts elts))
	 (stored-double-elts (store-to-vector double-elts))
	 (len-stored-double-elts (length stored-double-elts)))
    (true (every #'equal double-elts (restore-from-vector stored-double-elts)))
    (setf stored-double-elts nil)
    (true (< len-stored-double-elts
	     (length (store-to-vector (append elts (map 'list #'copy-seq elts))))))))

(define-test test-simple-displaced-array-circularity
  (let* ((a (make-array 1))
	 (b (make-array 1 :displaced-to a))
	 (*support-shared-list-structures* nil))
    (setf (aref a 0) b)
    (let ((c (restore-from-vector (store-to-vector b))))
      (let ((*print-circle* t))
	;; This depends on the circle printer to do the same thing :)
	(is 'equal
	    (format nil "~A~%" c)
	    (format nil "~A~%" b))))))

(define-test test-displaced-array-circularity
  (let* ((a (make-array 3))
	 (b (make-array 1 :displaced-to a))
	 (*support-shared-list-structures* nil))
    (setf (aref a 0) b)
    (setf (aref a 1) (list "blarg" a b))
    (setf (aref a 2) (make-array 3 :initial-element b))
    (let ((c (restore-from-vector (store-to-vector b))))
      (let ((*print-circle* t))
	;; This depends on the circle printer to do the same thing :)
	(is 'equal
	    (format nil "~A~%" c)
	    (format nil "~A~%" b))))))

(defun type-equal (t1 t2)
  (and (subtypep t1 t2)
       (subtypep t2 t1)))

(define-test test-simple-arrays
  (loop for input-type in '(single-float double-float (signed-byte 8) fixnum)
	for generator in (list (lambda () (random 1f0))
			       (lambda () (random 1d0))
			       (lambda () (- (random 256) 128))
			       (lambda () (- (random 12345) 123)))
	do
	   (let ((input (make-array (list (+ 1 (random 10))
					  (+ 1 (random 10))) :element-type input-type)))
	     (loop for idx below (array-total-size input)
		   do (setf (row-major-aref input idx) (funcall generator)))
	     (let ((restored (restore (store nil input))))
	       (true (equalp restored input))
	       (true (equal (type-of input) (type-of restored)))))))

(define-test test-simple-vectors
  (let* ((elt-types
	     '(bit
	       fixnum base-char character single-float
	       double-float (signed-byte 8) (signed-byte 16)
	       (signed-byte 32) (signed-byte 64)
	       (unsigned-byte 2) ;; lispworks and sbcl
	       (unsigned-byte 4) ;; lispworks and sbcl
	       (unsigned-byte 7)
	       (unsigned-byte 8)
	       (unsigned-byte 15)
	       (unsigned-byte 16)
	       (unsigned-byte 31)
	       (unsigned-byte 32)
	       (unsigned-byte 62)
	       (unsigned-byte 64)))
	   (sizes
	     (loop repeat (length elt-types)
		   collect (+ 1 (random 100))))
	   (fill-values
	     (loop for elt-type in elt-types
		   for size in sizes
		   collect
		   (loop repeat size collect
				     (case elt-type
				       (bit (random 1))
				       (fixnum #+(or ccl allegro)
					       (random (- (expt 2 59) (expt 2 58)))
					       #-ccl
					       (random (- (expt 2 62) (expt 2 61))))
				       (base-char #\a)
				       (character #\b)
				       (single-float (random 1f0))
				       (double-float (random 1d0))
				       (otherwise
					(if (eql (first elt-type) 'signed-byte)
					    (- (random 128))
					    (random (expt 2 (second elt-type))))))))))
      (assert (= (length elt-types) (length fill-values) (length sizes)))
    (let* ((input-data (loop for elt-type in elt-types
			     for fill in fill-values
			     for size in sizes
			     collect
			     (make-array size :element-type elt-type :initial-contents fill)))
	   (input (store-to-vector input-data)))
	(loop for elt-type in elt-types
	      for size in sizes
	      for input-array in input-data
	      for result in (restore-from-vector input)
	      do
		 ;; (format t "~A with ~A elements, (~A) result is a ~A and is~% ~A~%"
		 ;; 	 elt-type size input-array (type-of result) result)
		 (is 'type-equal (upgraded-array-element-type (array-element-type result))
		     (upgraded-array-element-type elt-type))
		 (is 'equalp input-array result)
		 (true (= (length result) size))))))
  
(define-test test-strings
  (let ((a-string "asdffdsa")
	(b-string "somethin"))
    (dolist (string (list (make-string 10 :element-type 'base-char :initial-element #\a)
			  a-string
			  (make-string 10 :element-type 'character :initial-element #\b)))
      (is 'equalp (restore (store nil string)) string))
    ;; Check that references work
    (true (apply #'eql (restore (store nil (list a-string a-string)))))
    (is 'equalp (restore (store nil (list a-string a-string b-string)))
	(list a-string a-string b-string))
    (true (< (length (store nil (list a-string a-string)))
	     (length (store nil (list a-string b-string)))))))

(define-test test-symbols
  (let ((symbols (list (intern "HI" "CL-BINARY-STORE-TESTS")
		       (intern "TEST-SYMBOL-HI" "CL-BINARY-STORE"))))
    (true (equalp
	   (restore-from-vector (store-to-vector symbols))
	   symbols))
    (let ((vec (store-to-vector symbols)))
      (unintern (find-symbol "HI"))
      (unintern (find-symbol "TEST-SYMBOL-HI" "CL-BINARY-STORE"))
      (restore-from-vector vec)
      (true (find-symbol "HI"))
      (true (find-symbol "TEST-SYMBOL-HI" "CL-BINARY-STORE")))
    (let ((g (gensym)))
      (let ((new-g (restore-from-vector (store-to-vector g))))
	(is 'equalp (symbol-name g) (symbol-name new-g))
	(true (null (symbol-package new-g)))))))

(define-test test-simple-vector
  (let ((vector #(1 2 3 "asdf" "ghijil" 1d0)))
    (is
     'equalp
     (restore-from-vector (store-to-vector vector))
     vector)))

(defstruct blarg
  (a)
  (b 0d0 :type double-float)
  (c 0f0 :type single-float)
  (d #() :type simple-vector)
  (f nil :type boolean)
  (g t :type (or simple-vector integer (eql t))))

(define-test test-struct-simple
  (let* ((one (make-blarg :a 1234 :b 1d0 :d (make-array 5 :initial-element "hi")))
	 (two (make-blarg :a 456 :b 3d0 :d (make-array 5 :initial-element "boo")))
	 (s (list one two one two)))
    (let ((result (restore-from-vector (store-to-vector s))))
      (is 'equalp s result)
      (is 'eql (first result) (third result))
      (is 'eql (second result) (fourth result)))))

(define-test test-struct-circular
  (let ((s (list (make-blarg :a 1234 :b 1d0 :d (make-array 5 :initial-element "hi"))
		 (make-blarg :a 456 :b 3d0 :d (make-array 5 :initial-element "boo")))))
    (setf (blarg-a (second s)) (first s))
    (setf (blarg-a (first s)) (second s))
    (let ((result (restore-from-vector (store-to-vector s)))
	  (*print-circle* t))
      (is '= (length result) 2)
      (is 'eql (blarg-a (first result)) (second result))
      (is 'eql (blarg-a (second result)) (first result))
      (setf (blarg-a (first s)) nil)
      (setf (blarg-a (second s)) nil)
      (setf (blarg-a (first result)) nil)
      (setf (blarg-a (second result)) nil)
      (is 'equalp result s))))

(defclass a-class ()
  ((a :initarg :a)
   (b :initform 1d0 :initarg :b)
   (c :initform "c" :initarg :c)))
  
(defclass b-class (a-class)
  ((d :initform "hihi" :initarg :d)))

(define-test test-standard-objects
  (let* ((b (list (make-instance 'b-class)
		  (make-instance 'b-class :a 1 :b 2 :c 3 :d 4)
		  (make-instance 'a-class))))
    ;; circularity tests
    (setf (slot-value (first b) 'b) (first b))
    (setf (slot-value (third b) 'c) (second b))
    (destructuring-bind (x y z)
	(restore-from-vector (store-to-vector b))
      (false (slot-boundp x 'a))
      (is 'eq (slot-value x 'b) x)
      (is 'equalp (slot-value x 'c) "c")
      (is 'equalp (slot-value x 'd) "hihi")
      
      (true (slot-boundp y 'a))
      (is 'eql (slot-value y 'b) 2)
      (is 'eql (slot-value y 'c) 3)
      (is 'eql (slot-value y 'd) 4)
      
      (false (slot-boundp z 'a))
      (is 'eql (slot-value z 'b) 1d0)
      (is 'eq (slot-value z 'c) y))
    ;; Reference tests
    (let* ((b0 (make-instance 'b-class))
	   (result (restore-from-vector (store-to-vector (list b0 b0)))))
      (is 'eql (first result) (second result)))))

(define-test test-pathname
  (let ((a (make-pathname :directory "tmp" :name "blarg")))
    (is 'equalp
	(restore-from-vector (store-to-vector a))
	a)))

(define-test test-hash-table
  (let* ((ht (make-hash-table :test 'equalp))
	 (kvs (list (cons 1234 t)
		    (cons "blarg" 5668d0)
		    (cons (list 7d0) (vector 1 2 3))
		    (cons (vector (/ 3 4)) 17d0))))
    (map nil (lambda (x)
	       (setf (gethash (car x) ht) (cdr x)))
	 kvs)
    (let ((ht-restore (restore-from-vector (store-to-vector ht))))
      (maphash (lambda (k v)
		 (equalp (gethash k ht-restore) v))
	       ht))))
	 
(define-test test-simple-array-t-multi-dim
  (let ((a (make-array '(1 1 1) :initial-element 3)))
    (is 'equalp
	a
	(restore-from-vector (store-to-vector a)))))

(define-test test-non-proper-list
  (let ((non-proper-list '(1 . 2)))
    (is 'equal
	(restore-from-vector
	 (store-to-vector
	  non-proper-list))
	non-proper-list)))

(define-test test-simple-double-float
  (let ((df 3.1415d0)
	(df-signed -1.23d-23))
    (is '= df
	(restore-from-vector
	 (store-to-vector df)))
    (is '= df-signed
	(restore-from-vector
	 (store-to-vector df-signed)))))

(define-test test-simple-single-float
  (let ((sf 3.1415f0))
    (is '= sf
	(restore-from-vector
	 (store-to-vector sf))))
  (let ((sf -3.1415f-12))
    (is '= sf
	(restore-from-vector
	 (store-to-vector sf)))))

(define-test test-complex
  (let ((complex-numbers
	  (list
	   (complex 1 2)
	   (complex 3f0 4f0)
	   (complex -5d0 3d0)
	   (complex (/ 1 2) 17)
	   (complex 1f0 2d0))))
    (is 'equal
	complex-numbers
	(restore-from-vector
	 (store-to-vector complex-numbers)))))

(define-test test-ratio
  (let ((ratios (list (/ 1 2) (/ 4 -5) (/ 1 (expt 2 93)))))
    (is 'equal
	ratios
	(restore-from-vector
	 (store-to-vector ratios)))))

(define-test test-sb8
  (is '= -127 (restore-from-vector (store-to-vector -127)))
  (is '= -255 (restore-from-vector (store-to-vector -255))))

(define-test test-sb16
  (is '= -32768 (restore-from-vector (store-to-vector -32768)))
  (is '= -65535 (restore-from-vector (store-to-vector -65535))))

(define-test test-sb32
  (let ((num (- (expt 2 31))))
    (is '= num (restore-from-vector (store-to-vector num))))
  (let ((num (- (1- (expt 2 32)))))
    (is '= num (restore-from-vector (store-to-vector num)))))

(define-test test-fixnum
  (let* ((num (expt 2 58))
	 (mnum (- num)))
    (is '= num (restore-from-vector (store-to-vector num)))
    (is '= mnum (restore-from-vector (store-to-vector mnum)))))

;; Do nothing

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun store-string-for-tests (obj storage)
    ;; Write nothing!
    (declare (ignore obj storage)))
  (define-codespace ("test-codespace" 999999 :inherits-from +basic-codespace+)
    (defstore simple-string (store-string-for-tests obj storage) :override t)
    (defstore simple-base-string (store-string-for-tests obj storage) :override t)))

(define-test test-versioning
  (true (null (restore
	       (let ((*output-magic-number* t)
		     (*write-version* 999999))
		 (store nil "check")))))
  (is 'equalp
      "check"
      (restore
       (let ((*output-magic-number* nil))
	 (store nil "check"))))
  (is 'equalp
      "check"
      (restore
       (let ((*output-magic-number* t))
	 (store nil "check")))))
  
#+sbcl(define-test test-condition-serialization
  ;; On sbcl a condition is neither a standard-object nor a structure-object
  (let* ((a (make-condition 'simple-error :format-control "hi ~A" :format-arguments (list 123)))
         (b (restore (store nil a))))
    (is 'eql (type-of a) (type-of b))
    (is 'eql (class-of a) (class-of b))
    (is 'equalp (simple-condition-format-control a) (simple-condition-format-control b))
    (is 'equal (simple-condition-format-arguments a) (simple-condition-format-arguments b))))

#+sbcl (define-test test-sap-write/read
  ;; Normal use
  (let ((a (make-array 24 :element-type '(unsigned-byte 8) :initial-element 0))
        (input (list 1 2)))
    (is 'equal
        (with-pinned-objects (a)
          (let ((len (store-to-sap (vector-sap a) (length a) input)))
            (restore-from-sap (vector-sap a) len)))
        input))
  ;; Not enough space
  (let ((a (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0))
        (input (list 1 2)))
    (with-pinned-objects (a)
      (fail (store-to-sap (vector-sap a) (length a) input))))
  ;; Dynamic sap replacement for mmap'ed files
  (let ((a (make-array 24 :element-type '(unsigned-byte 8) :initial-element 0))
        (data (list 1d0 2 3)))
    ;; Here we don't actually reallocate, we just lie and say we did
    (with-pinned-objects (a)
      (let* ((len
               (handler-bind ((out-of-space
                                (lambda (e)
                                  (replace-store-sap-buffer
                                   (vector-sap a)
                                   :sap-offset (out-of-space-current-offset e)
                                   :sap-size (length a)))))
                 (store-to-sap (sb-sys:vector-sap a) 1 data))))
        (is 'equal data (restore-from-sap (sb-sys:vector-sap a) len))))))

(define-test test-store/restore-to-file
  (let ((data1 (make-array 398423 :initial-element 3))
        (data2 (make-list 1234 :initial-element "hi")))
    (multiple-value-bind (d1 d2)
        (restore (store "/tmp/blarg-test-cl-store.bin" (list data1 data2) :data-is-list-of-separate-objects t))
      (is 'equalp data1 d1)
      (is 'equalp data2 d2))))

(define-test test-end-marker
  (is 'equal (multiple-value-list
	      (restore (concatenate 'vector
				    (let ((*output-end-marker* t))
				      (store nil '(1 2) :data-is-list-of-separate-objects t))
				    (store nil 3))))
      '(1 2))
  (is 'equal (multiple-value-list
	      (restore (concatenate 'vector
				    (let ((*output-end-marker* nil))
				      (store nil '(1 2) :data-is-list-of-separate-objects t))
				    (store nil 3))))
      '(1 2 3)))

(defparameter *a* 1d0)
(defparameter *b* 4d0)
(defparameter *c* 5d0)

(define-test test-double-float-references
  ;; Have to use globals so the compiler doesn't make our double floats eq
  (let ((len-just-a (length (store nil *a*))) ;; 9 bytes
	(len-two-as (length (store nil (list *a* *a*) :data-is-list-of-separate-objects t))) ;; 13 bytes
	(len-a-and-b/4 (length (store nil (list *a* (/ *b* 4d0)) :data-is-list-of-separate-objects t))) ;; 13 bytes
	(len-a-and-c (length (store nil (list *a* *c*) :data-is-list-of-separate-objects t)))) ;; 18 bytes
    (true (> len-two-as len-just-a))
    (true (= len-two-as len-a-and-b/4))
    (true (= len-a-and-b/4 len-two-as))
    (true (> len-a-and-c len-a-and-b/4))))

(defstruct only-serialize-a
  (a nil)
  (b nil))

(defmethod serializable-object-info ((type (eql 'only-serialize-a)))
  (list 'a))

(define-test test-specialized-struct-serializer
  (let ((obj
	  (restore (store nil (make-only-serialize-a :a "a" :b "censored")))))
    ;; Here the value of b is undefined behavior
    (is 'equalp (only-serialize-a-a obj) "a")))

(defclass only-serialize-a-class ()
  ((a :initarg :a)
   (b :initarg :b)))

(defmethod serializable-object-info ((type (eql 'only-serialize-a-class)))
  (list 'a))

(define-test test-specialized-object-serializer
  (let ((obj
	  (restore (store nil (make-instance 'only-serialize-a-class :a "a" :b "censored")))))
    (is 'equalp (slot-value obj 'a) "a")
    (true (null (slot-boundp obj 'b)))))

(defstruct specially-constructed
  (a)
  (b))

(defmethod specialized-object-constructor ((type (eql 'specially-constructed)))
  (lambda (object-info slot-values)
    (let ((slot-names (object-info-slot-names object-info)))
      (true (eq type 'specially-constructed))
      (true (= (length slot-names) (length slot-values) 2))
      (every (lambda (slot-name slot-value)
	       (true (member slot-name '(a b)))
	       (is 'equalp slot-value (if (eq slot-name 'a)
					  "a"
					  "censored")))
	     slot-names slot-values)
      (make-specially-constructed :a "hello"))))

(define-test test-specialized-object-constructor
  (let ((obj
	  (restore (store nil (make-specially-constructed :a "a" :b "censored")))))
    (is 'equalp (specially-constructed-a obj) "hello")
    (true (null (specially-constructed-b obj)))))

(defstruct going-away-struct a b)
(defstruct replacement-struct b a)

(define-test test-use-replacement-struct
  (let ((data (store nil (make-going-away-struct :a "a" :b "b"))))
    (unintern 'going-away-struct)
    (handler-bind ((object-type-not-found
		     (lambda (e)
		       (declare (ignore e))
		       (invoke-restart (find-restart 'use-different-class) 'replacement-struct))))
      (let ((result (restore data)))
	(true (typep result 'replacement-struct))
	(is 'string= (replacement-struct-a result) "a")
	(is 'string= (replacement-struct-b result) "b")))))

(define-test reference-id-encoding/decoding
  (loop for ref-id from
	cl-binary-store::+reference-direct-min-ref-id+ to
	cl-binary-store::+reference-direct-max-ref-id+
	do
	   (assert (= (cl-binary-store::decode-reference-direct
		       (cl-binary-store::encode-reference-direct ref-id))
		      ref-id)))
  
  (loop for ref-id from
	cl-binary-store::+reference-one-byte-min-ref-id+ to
	cl-binary-store::+reference-one-byte-max-ref-id+
	do (let ((result (cl-binary-store::encode-reference-one-byte ref-id)))
	     (let ((tag-byte (logand result #xFF))
		   (next-byte (ash result -8)))
	       (assert (<= 0 next-byte 255))
	       (assert (= (cl-binary-store::decode-reference-one-byte tag-byte next-byte)
			  ref-id)))))
  (loop for ref-id from
	cl-binary-store::+reference-two-byte-min-ref-id+ to
	cl-binary-store::+reference-two-byte-max-ref-id+
	do (multiple-value-bind (tag-byte next-16-bytes)
	       (cl-binary-store::encode-reference-two-bytes ref-id)
	     (assert (<= 0 next-16-bytes 65535))
	     (assert (= (cl-binary-store::decode-reference-two-bytes tag-byte next-16-bytes)
			ref-id))))

  (loop for ref-id from
		   (+ cl-binary-store::+reference-two-byte-max-ref-id+ 1) below
		   (+ cl-binary-store::+reference-two-byte-max-ref-id+ 123456)
	do (assert (= (cl-binary-store::decode-reference-tagged
		       (cl-binary-store::encode-reference-tagged ref-id)) ref-id))))


(defclass blarg-test-object ()
  ((a :initarg :a :reader blarg-test-object-a)
   (b :initarg :b :reader blarg-test-object-b)))
		   
(define-test test-standard-object
  (let* ((one (make-instance 'blarg-test-object :a 1234 :b 1d0))
	 (two (make-instance 'blarg-test-object :a 456 :b #(1 2 3)))
	 (s (list one two one two)))
    (let ((result (restore-from-vector (store-to-vector s))))
      (true (every (lambda (o r)
		     (and (equalp (blarg-test-object-a o)
				  (blarg-test-object-a r))
			  (equalp (blarg-test-object-b o)
				  (blarg-test-object-b r))))
		   s result))
      (is 'eql (first result) (third result))
      (is 'eql (second result) (fourth result)))))

(define-test test-bignum
  (is '= (expt 2 64) (restore (store nil (expt 2 64))))
  (is '= 12345678901234567890 (restore (store nil 12345678901234567890)))
  (is '= -12345678901234567890 (restore (store nil -12345678901234567890))))

(define-test test-write-into-extant-vector
  (loop for length in '(100 #-abcl 50000)
	do
	   (let* ((data (make-list length :initial-element 1))
		  (result (coerce (store nil data) '(simple-array (unsigned-byte 8) (*))))
		  (restored-result (restore result))
		  (restored-result-2 (progn
				       (fill result 0)
				       (store result data)
				       (restore result))))
	     (is '= (length restored-result) length)
	     (is '= (length restored-result-2) length)
	     (is 'equal data restored-result)
	     (is 'equal data restored-result-2))))

(define-test test-interior-unsigned-fixnum
  (dotimes (list-length 300)
    (is '= (length (restore (store nil (make-list list-length)))) list-length)))
	
(define-test test-strings-longer-than-buffer
  (dolist (element-type '(base-char character))
    (let* ((input (make-string 40000 :element-type element-type :initial-element (code-char 42)))
	   (restored (restore (store nil input))))
      (is 'equalp restored input)
      (is 'eql (array-element-type input) (array-element-type restored)))))

(define-test test-empty-strings-and-vectors
  (true (zerop (length (restore (store nil #())))))
  (true (zerop (length (restore (store nil (make-array 0 :element-type 'single-float))))))
  (true (zerop (length (restore (store nil (coerce "" 'simple-string))))))
  (true (zerop (length (restore (store nil (coerce "" 'simple-base-string))))))
  (true (zerop (length (restore (store nil nil))))))

(let ((stuff (list -123 -1234 -123456 -34247823946234923864 #*0101
		   -1f0 -2d0 -1.234d0 (expt 2 64) (/ (expt 2 128) (expt 2 12))
		   (complex 1d0 1d0)
		   (make-array 123 :element-type 'double-float :initial-element 1.23d0)
		   (make-array 123 :element-type '(signed-byte 32) :initial-element -123984))))
  (define-test test-interop-write
    ;; this writes a file with a bunch of stuff
    (store "/tmp/blarg.bin" stuff)
    (is 'equalp (restore "/tmp/blarg.bin") stuff))
  (define-test test-interop-read
    (is 'equalp (restore "/tmp/blarg.bin") stuff)))

(define-test test-max-to-read/write
  (let* ((n 100000)
	 (input-specialized-array (list (make-array n :element-type '(unsigned-byte 8))
					n n))
	 (input-simple-vector (list (make-array n) n (* 8 n)))
	 (input-list (list (make-list n) n (* 16 n)))
	 (input-ht (list (make-hash-table :size n) nil (* 16 n))))
    (loop for (input estimated-output-size estimated-input-size)
	    in (list input-ht input-list input-simple-vector input-specialized-array)
	  for lots-of-data = (store nil input)
	  do
	     (print (list (type-of input) estimated-output-size estimated-input-size))
	     (fail (restore lots-of-data :max-to-read (floor estimated-input-size 2)))
	     (fail (restore lots-of-data :max-to-read (- estimated-input-size 1)))
	     (is 'equalp (restore lots-of-data :max-to-read (+ 10000 estimated-input-size)) input)
	     (when estimated-output-size
	       (fail (store nil input :max-to-write (- estimated-output-size 1000))))
	     (finish (store nil input :max-to-write (round (* 1.1 (or estimated-output-size 1024))))))))

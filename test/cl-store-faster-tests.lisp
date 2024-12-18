(defpackage #:cl-store-faster-tests
  (:use #:common-lisp #:parachute #:cl-store-faster))

(in-package #:cl-store-faster-tests)

(define-test test-basic-circularity
  #+nil(let* ((a (make-array 3))
	 (b (make-array 3 :displaced-to a))
	 (cl-store-faster:*support-shared-list-structures* t))
    (setf (aref a 0) b)
    (setf (aref a 1) (list "blarg" a b))
    (setf (aref a 2) (make-array 3 :initial-element b))
    (let ((c (restore-from-vector (store-to-vector b))))
      (let ((*print-circle* t))
	;; This depends on the circle printer to do the same thing :)
	(is 'equal
	    (format nil "~A~%" c)
	    (format nil "~A~%" b))))))

(define-test test-simple-arrays
    (let* ((elt-types
	     '(bit fixnum base-char character single-float
	       double-float (signed-byte 8) (signed-byte 16)
	       (signed-byte 32) (signed-byte 64)
	       (unsigned-byte 2)
	       (unsigned-byte 4)
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
		   collect (+ 1 (random 10))))
	   (fill-values
	     (loop for elt-type in elt-types
		   for size in sizes
		   collect
		   (loop repeat size collect
				     (case elt-type
				       (bit (random 1))
				       (fixnum (random (- (expt 2 62) (expt 2 61))))
				       (base-char #\a)
				       (character #\b)
				       (single-float (random 1f0))
				       (double-float (random 1d0))
				       (otherwise
					(if (eql (first elt-type) 'signed-byte)
					    (- (random 128))
					    (random (expt 2 (second elt-type))))))))))
      (assert (= (length elt-types) (length fill-values) (length sizes)))
      (let ((input (store-to-vector 
		    (loop for elt-type in elt-types
			  for fill in fill-values
			  for size in sizes
			  collect
			  (make-array size :element-type elt-type :initial-contents fill)))))
	(loop for elt-type in elt-types
	      for fill in fill-values
	      for size in sizes
	      for result in (restore-from-vector input)
	      do
		 ;;(format t "~A with ~A elements~% ~A (vs ~A)~%" elt-type size fill result)
		 (true (equal (upgraded-array-element-type (array-element-type result))
			      elt-type))
		 (true (every (lambda (x fill-value) (eql x fill-value)) result fill))
		 (true (= (length result) size))))))
  
(define-test test-strings
  (dolist (string (list (make-string 10 :element-type 'base-char :initial-element #\a)
			"asdffdsa"
			(make-string 10 :element-type 'character :initial-element #\b)))
    (is 'equalp (restore-from-vector (store-to-vector string)) string)))

(define-test test-symbols
  (let ((symbols (list (intern "HI" "CL-STORE-FASTER-TESTS")
		       (intern "TEST-SYMBOL-HI" "CL-STORE-FASTER"))))
    (true (equalp
	   (restore-from-vector (store-to-vector symbols))
	   symbols))
    (let ((vec (store-to-vector symbols)))
      (unintern (find-symbol "HI"))
      (unintern (find-symbol "TEST-SYMBOL-HI" "CL-STORE-FASTER"))
      (restore-from-vector vec)
      (true (find-symbol "HI"))
      (true (find-symbol "TEST-SYMBOL-HI" "CL-STORE-FASTER")))
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
  (let ((s (list (make-blarg :a 1234 :b 1d0 :d (make-array 5 :initial-element "hi"))
		 (make-blarg :a 456 :b 3d0 :d (make-array 5 :initial-element "boo")))))
    (let ((result (restore-from-vector (store-to-vector s))))
      (is 'equalp result s))))

(define-test test-struct-circular
  (let ((s (list (make-blarg :a 1234 :b 1d0 :d (make-array 5 :initial-element "hi"))
		 (make-blarg :a 456 :b 3d0 :d (make-array 5 :initial-element "boo"))))
	(cl-store-faster:*support-shared-list-structures* t))
    (setf (blarg-a (second s)) (first s))
    (setf (blarg-a (first s)) (second s)) ;; <-- this is pointing at middle of s
    (let ((result (restore-from-vector (store-to-vector s))))
      (is 'eql (blarg-a (first result)) (second result))
      (is 'eql (blarg-a (second result)) (first result))
      (setf (blarg-a (first s)) nil)
      (setf (blarg-a (second s)) nil)
      (setf (blarg-a (first result)) nil)
      (setf (blarg-a (second result)) nil)
      (is 'equalp result s))))

(define-test test-struct-info
  (let ((b (cl-store-faster::compute-struct-info (make-instance 'blarg))))
    (is 'equalp
	(restore-from-vector (store-to-vector b))
	b)))

(defclass a-class ()
  ((a :initarg :a)
   (b :initform 1d0 :initarg :b)
   (c :initform "c" :initarg :c)))
  
(defclass b-class (a-class)
  ((d :initform "hihi" :initarg :d)))

(define-test test-standard-objects
  (let ((b (list (make-instance 'b-class)
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
      (is 'eq (slot-value z 'c) y))))

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

	 

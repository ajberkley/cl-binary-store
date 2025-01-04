;;(quicklisp:quickload "cl-store")
;;#-(or lispworks allegro)(quicklisp:quickload "hyperluminal-mem")
;;(quicklisp:quickload "cl-conspack")
;;#+sbcl (require 'sb-sprof)

(in-package :cl-binary-store)

(defmacro timed ((annotation &optional (repeats 1) output-size-MB) &body body)
  (let ((start (gensym))
        (end (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1
           ,@body
         (let* ((,end (get-internal-real-time)))
	   (cond
	     ((= ,end ,start)
	      (format t "~A too fast to resolve~%" ,annotation))
	     (t
	      (let ((ms-per (/ (- ,end ,start)
                               (* 0.001f0 internal-time-units-per-second)
                               ,repeats)))
		(format t "~A ~,2f ms ~A~%" ,annotation ms-per
			,(if output-size-MB
                             `(format nil "at ~d MB/sec" (round (/ ,output-size-MB ms-per 1f-3)))
                        ""))))))))))


#-(or allegro abcl lispworks) ;; crashes on abcl
(defun test-hlmem-on-data (data &key (repeats 20))
  (let* ((words (hyperluminal-mem:msize 0 data))
         (output-size (/ (* 8 words) 1e6)))
    (format t "HYPERLUMINAL-MEM~%")
    (format t " OUTPUT SIZE: ~,2f MB~%" output-size)
    (static-vectors:with-static-vector (a-store (* 8 words))
      (timed (" WRITE:" repeats output-size)
        (dotimes (x repeats)
	  (hyperluminal-mem::mwrite (static-vectors:static-vector-pointer a-store) 0 words data)))
      ;; returns words
      (timed (" READ :" repeats output-size)
        (dotimes (x repeats)
	  (hyperluminal-mem:mread (static-vectors:static-vector-pointer a-store) 0 words))))))

(defun test-cl-binary-store-on-data
    (data &key (track-references t) (support-shared-list-structures nil) (repeats 20)
            (read t) (write t) (file nil))
  (let* ((cl-binary-store:*support-shared-list-structures* support-shared-list-structures)
	 (cl-binary-store:*track-references* track-references)
	 (store (coerce
		 (cl-binary-store:store nil data)
		 '(simple-array (unsigned-byte 8) (*))))
	 (size (length store))
         (output-size-MB (/ size 1e6)))
    (format t "CL-BINARY-STORE~%")
    (format t " OUTPUT SIZE: ~,2f MB~%" output-size-MB)
    (when write
	(timed (" WRITE:" repeats output-size-MB)
	  (dotimes (x repeats) (cl-binary-store:store store data)))
	(when file
	  (timed (" FILE WRITE:" repeats output-size-MB)
	    (dotimes (x repeats)
	      (with-open-file (str "blarg.bin" :if-exists :supersede :if-does-not-exist :create
					       :direction :output :element-type '(unsigned-byte 8))
		(cl-binary-store:store str data))))))
    (when read
      (timed (" READ :" repeats output-size-MB)
        (dotimes (x repeats) (cl-binary-store:restore store)))
      (when file
	(timed (" FILE READ :" repeats output-size-MB)
	  (dotimes (x repeats)
	    (with-open-file (str "blarg.bin" :direction :input :element-type '(unsigned-byte 8))
	      (cl-binary-store:restore str))))))
    (values)))

(defun test-conspack-on-data (data &key (repeats 10) (read t) (write t) (to-file nil)
				     (track-references t))
  (format t "CL-CONSPACK~%")
  (let* ((encoded-data (if track-references
			   (conspack:tracking-refs ()
			     (conspack:encode data))
			   (conspack:encode data)))
	 (output-size-MB (/ (length encoded-data) 1e6)))
    (format t " OUTPUT SIZE: ~,2fMB~%" output-size-MB)
    (when write
      (when to-file
	(timed (" FILE WRITE:" repeats output-size-MB)
	  (dotimes (x repeats)
	    (with-open-file (str "blarg.bin" :if-exists :supersede :if-does-not-exist :create
					     :direction :output :element-type '(unsigned-byte 8))
	      (if track-references
		  (conspack:tracking-refs ()
		    (conspack:encode data :stream str))
		  (conspack:encode data :stream str))))))
      (timed (" WRITE:" repeats output-size-MB)
	(dotimes (x repeats)
	  (if track-references
	      (conspack:tracking-refs ()
		(conspack:encode data))
	      (conspack:encode data)))))
    (when read
      (when to-file
	(timed (" FILE READ :" repeats output-size-MB)
	  (dotimes (x repeats)
	    (with-open-file (str "blarg.bin" :element-type '(unsigned-byte 8))
	      (if track-references
		  (conspack:tracking-refs ()
		    (conspack:decode-stream str))
		  (conspack:decode-stream str))))))
      (timed (" READ :" repeats output-size-MB)
	(if track-references
	    (dotimes (x repeats)
	      (conspack:tracking-refs ()
		(conspack:decode encoded-data)))
	    (dotimes (x repeats)
	      (conspack:decode encoded-data)))))
    (values)))

(defun test-cl-store-on-data
    (data &key (check-for-circs nil) (repeats 10) (read t) (write t)
	    (precise-list-storage nil))
  ;; if you try and dump it to a flexi-streams sequence it's 4x slower than this!
  (let ((cl-store:*check-for-circs* check-for-circs)
	(cl-store:*precise-list-storage* precise-list-storage))
    (format t "CL-STORE~%")
    (cl-store:store data "blarg.bin")
    (let ((output-size-MB
            (with-open-file (str "blarg.bin")
              (/ (file-length str) 1e6))))
      (format t " OUTPUT SIZE: ~,2fMB~%" output-size-MB)
      (when write
        (timed (" WRITE:" repeats output-size-MB)
          (dotimes (x repeats) (cl-store:store data "blarg.bin"))))
      (when read
        (timed (" READ :" repeats output-size-MB)
          (dotimes (x repeats) (cl-store:restore "blarg.bin")))))))

(defun test-on-data (data &key (hlmem t) (cl-store t) (cl-binary-store t) (conspack t))
  #-(or allegro abcl lispworks)
  (when hlmem
    (test-hlmem-on-data data))
  (when cl-binary-store
    (test-cl-binary-store-on-data data :track-references (not hlmem)
                                       :support-shared-list-structures (and (not hlmem)
                                                                            (not cl-store))))
  (when conspack
    (test-conspack-on-data data :track-references (not hlmem)))
  (when cl-store
    (test-cl-store-on-data data :check-for-circs (not hlmem))))

;; Data to test on
(defun long-list-of-tiny-integers (&optional (n 1000000))
  (loop repeat n collect (- (random 33) 16)))

(defun long-simple-vector-of-tiny-integers (&optional (n 1000000))
  (coerce (long-list-of-tiny-integers n) 'simple-vector))

(defun long-list-of-not-tiny-integers (&optional (n 1000000))
  (make-list n :initial-element (random 256)))

(defun long-list-of-random-fixnums (&optional (n 1000000))
  (loop repeat n collect (random #-ccl(- (expt 2 61) (expt 2 60))
				 #+ccl (- (expt 2 59) (expt 2 58)))))

(defun long-list-of-random-double-floats (&optional (n 1000000))
  (loop repeat n collect (random 1d0)))

(defun long-list-of-random-single-floats (&optional (n 1000000))
  (loop repeat n collect (random 1f0)))

(defun long-list-of-random-complex-double-floats (&optional (n 1000000))
  (loop repeat n collect (complex (random 1d0) (random 1d0))))

(defun long-list-of-big-ub8-vectors (&optional (n 1000))
  (loop repeat n
	collect
	(coerce
	 (loop for i fixnum from 0 below 10000
	       collect 123)
         '(simple-array (unsigned-byte 8) (*)))))

(defun long-list-of-big-simple-bit-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 10000
	       collect (random 1))
         '(simple-array bit (*)))))

(defun long-list-of-big-simple-double-float-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 1000
	       collect (random 1d0))
         '(simple-array double-float (*)))))

(defun list-of-double-float-matrices ()
  (loop repeat 100
	collect
	(let ((m (make-array '(100 100) :element-type 'double-float)))
	  (dotimes (i 100)
	    (dotimes (j 100)
	      (setf (aref m i j) (random 1d0))))
	  m)))

(defun long-complex-list ()
  (loop repeat 1000000 collect (if (> (random 1000) 500)
				   3.1415d0
				   ;; (complex 1d0) ;; cl-store chokes
				   ;; (random 1d0) ;; cl-store chokes
				   (if (> (random 100) 50)
				       ;;(random 1f0) ;; <- makes cl-store take forever!
				       "hi" ;;(format nil "~A" (random 123))
				       (if (> (random 100) 50)
					   (cons (random 30) 2)
					   (if (= (random 2) 1)
					       (complex 1d0 1d0)
					       ;; (random 1f0) slows cl-store crazily
					       #()))))))

(defun lots-of-the-same-string ()
  (let ((string (coerce "asdf" 'simple-base-string)))
    (loop for i fixnum from 0 below 1000000
	  collect string)))

(defun lots-of-keywords (&optional (n 100000))
  "With some repeats"
  (loop for i fixnum from 0 below n
	collect (intern (format nil "~A" (random n)) 'keyword)))

(defun lots-of-symbols (&optional (N 100000))
  "With some repeats"
  (loop for i fixnum from 0 below n
	collect (intern (format nil "~A" (random n)) 'cl-user)))

(defun lots-of-uninterned-symbols (&optional (N 100000))
  "With some repeats"
  (let ((symbol-pool (loop for i fixnum below (floor N 5) collect (gensym "HELLO"))))
    (loop repeat 5 appending symbol-pool)))

(defstruct bench-blarg
  a
  b)

(conspack:defencoding bench-blarg
  a b)

#-allegro
(defmethod hyperluminal-mem:msize-object ((b bench-blarg) index)
  (hyperluminal-mem:msize* index (bench-blarg-a b) (bench-blarg-b b)))

#-allegro
(defmethod hyperluminal-mem:mwrite-object ((b bench-blarg) ptr index end-index)
  (hyperluminal-mem:mwrite* ptr index end-index (bench-blarg-a b) (bench-blarg-b b)))

#-allegro
(defmethod hyperluminal-mem:mread-object ((type (eql 'bench-blarg)) ptr index end-index &key)
  (hyperluminal-mem:with-mread* (a b new-index) (ptr index end-index)
    (values
     (make-bench-blarg :a a :b b)
     new-index)))

(defun lots-of-structure-objects (&optional (n 100000))
  (coerce 
   (loop for i below n
         collect (make-bench-blarg :a (random 1d0) :b (coerce (format nil "~A" (random 100)) 'simple-base-string)))
   'simple-vector))

(defclass c-blarg
    ()
  ((a :initarg :a)
   (b :initarg :b)))

(conspack:defencoding c-blarg
  a b)

(defun lots-of-standard-objects (&optional (n 100000))
  (coerce 
   (loop for i below n
	 collect (make-instance 'c-blarg :a (random 256) :b "hello"))
   'simple-vector))

(defun simple-base-strings ()
  (loop for i below 100000
        collect (coerce (format nil "~A" (random 1000000)) 'simple-base-string)))

(defun simple-strings ()
  (loop for i below 100000
        collect (format nil "~A~A" (random 1000000)
			#+(or abcl allegro) (code-char #x03b1)
			#-(or abcl allegro) #\U+03b1)))

(defun a-pile-of-tangled-conses (&optional (number 1000))
  (let ((a (make-array number)))
    (loop for n below number do (setf (svref a n) (cons nil nil)))
    (loop repeat (* 10 number)
	  do (setf (car (svref a (random number)))
		   (svref a (random number)))
	     (setf (cdr (svref a (random number)))
		  (svref a (random number))))
    a))

(defun a-bunch-of-specialized-arrays (&optional (n 10000))
  (loop for type in '((unsigned-byte 1)
		      (unsigned-byte 2)
		      (unsigned-byte 4)
		      (unsigned-byte 8)
		      (unsigned-byte 16)
		      (unsigned-byte 32)
		      fixnum
		      (unsigned-byte 64)
		      (signed-byte 8)
		      (signed-byte 16)
		      (signed-byte 32)
		      (signed-byte 64)
		      single-float
		      double-float)
	for elt in (list 1 2 15 255 65535 (1- (expt 2 32)) (expt 2 50) (1- (expt 2 64))
		     -1 -128 -32768 -100000 1f0 -1d0)
	collect
	(make-array n :element-type type :initial-element elt)))


(defstruct address
  (street "Ave Greene" :type simple-string)
  (state "QC" :type simple-string)
  (zip "H3Z1Z9" :type simple-base-string))

(defstruct person
  (first "Andrew" :type simple-string)
  (second "Berkley" :type simple-string)
  (age 49 :type (unsigned-byte 8)) ;; take that future people living to 256 years old!
  (addresses nil :type list)
  (telephone "" :type simple-base-string)
  (email "" :type simple-string))
  
(defun a-lot-of-people-and-addresses (&optional (n 10000))
  (let* ((addresses (coerce
		    (loop repeat n
			  collect (make-address :street (format nil "~A" (random 1000000))
						:state (format nil "~A" (random 100))
						:zip (format nil "~A" (random 100000))))
		    'simple-vector))
	 (people-with-addresses
	   (coerce
	    (loop repeat n
		  collect (make-person
			   :first (format nil "~A" (random 10000000))
			   :second (format nil "~A" (random 10000000))
			   :age (random 100)
			   :addresses (list (svref addresses (random n)))
			   :telephone (format nil "~A" (random 1000000))
			   :email (format nil "~A" (random 100000000))))
	    'simple-vector)))
    people-with-addresses))
    
    
			       

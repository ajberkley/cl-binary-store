(quicklisp:quickload "cl-store")
(quicklisp:quickload "hyperluminal-mem")
(require 'sb-sprof)

(in-package :cl-binary-store)

(defmacro timed ((annotation &optional (repeats 1) output-size-MB) &body body)
  (let ((start (gensym))
        (end (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1
           ,@body
         (let* ((,end (get-internal-real-time))
                (ms-per (/ (- ,end ,start)
                           (* 0.001f0 internal-time-units-per-second)
                           ,repeats)))
           (format t "~A ~,2f ms ~A~%" ,annotation ms-per
                   ,(if output-size-MB
                        `(format nil "at ~d MB/sec" (round (/ ,output-size-MB ms-per 1f-3)))
                        "")))))))


(defun test-hlmem-on-data (data &key (repeats 100))
  (let* ((words (hyperluminal-mem:msize 0 data))
	 (a-store (make-array words :element-type '(unsigned-byte 64)))
         (output-size (/ (* 8 words) 1e6)))
    (format t "HYPERLUMINAL-MEM~%")
    (format t " OUTPUT SIZE: ~,2f MB~%" output-size)
    (sb-sys:with-pinned-objects (a-store)
      (timed (" HLMEM WRITE:" repeats output-size)
        (dotimes (x repeats) (hyperluminal-mem::mwrite (sb-sys:vector-sap a-store) 0 words data)))
      ;; returns words
      (timed (" HLMEM READ :" repeats output-size)
        (dotimes (x repeats) (hyperluminal-mem:mread (sb-sys:vector-sap a-store) 0 words))))))

(defun test-cl-binary-store-on-data
    (data &key (track-references t) (support-shared-list-structures nil) (repeats 100)
            (read t) (write t))
  (let* ((cl-binary-store:*support-shared-list-structures* support-shared-list-structures)
	 (cl-binary-store:*track-references* track-references)
	 (size (length (cl-binary-store:store nil data)))
         (output-size-MB (/ size 1e6))
         (store (make-array size :element-type '(unsigned-byte 8))))
    (format t "CL-BINARY-STORE~%")
    (format t " OUTPUT SIZE: ~,2f MB~%" output-size-MB)
    (if write
	(timed (" CL-BINARY-STORE WRITE:" repeats output-size-MB)
	  (dotimes (x repeats) (cl-binary-store:store store data)))
	(cl-binary-store:store store data))
    (when read
      (timed (" CL-BINARY-STORE READ :" repeats output-size-MB)
        (dotimes (x repeats) (cl-binary-store:restore store))))
    (values)))

(defun test-cl-store-on-data
    (data &key (check-for-circs nil) (repeats 10) (read t) (write t))
  ;; if you try and dump it to a flexi-streams sequence it's 4x slower than this!
  (let ((cl-store:*check-for-circs* check-for-circs))
    (format t "CL-STORE~%")
    (cl-store:store data "blarg.bin")
    (let ((output-size-MB
            (with-open-file (str "blarg.bin")
              (/ (file-length str) 1e6))))
      (format t " OUTPUT SIZE: ~,2fMB~%" output-size-MB)
      (when write
        (timed (" CL-STORE WRITE:" repeats output-size-MB)
          (dotimes (x repeats) (cl-store:store data "blarg.bin"))))
      (when read
        (timed (" CL-STORE READ :" repeats output-size-MB)
          (dotimes (x repeats) (cl-store:restore "blarg.bin")))))))

(defun test-on-data (data &key (hlmem t) (cl-store t) (cl-binary-store t))
  (when hlmem
    (test-hlmem-on-data data))
  (when cl-binary-store
    (test-cl-binary-store-on-data data :track-references (not hlmem)
                                       :support-shared-list-structures (and (not hlmem)
                                                                            (not cl-store))))
  (when cl-store
    (test-cl-store-on-data data :check-for-circs (not hlmem))))

;; Data to test on
(defun long-list-of-tiny-integers (&optional (n 1000000))
  (loop repeat n collect (random 8)))

(defun long-list-of-not-tiny-integers (&optional (n 1000000))
  (make-list n :initial-element (random 256)))

(defun long-list-of-random-fixnums (&optional (n 1000000))
  (loop repeat n collect (random (- (expt 2 61) (expt 2 60)))))

(defun long-list-of-random-double-floats (&optional (n 1000000))
  (loop repeat n collect (random 1d0)))

(defun long-list-of-random-single-floats (&optional (n 1000000))
  (loop repeat n collect (random 1f0)))

(defun long-list-of-random-complex-double-floats (&optional (n 1000000))
  (loop repeat n collect (complex (random 1d0) (random 1d0))))

(defun long-list-of-big-ub8-vectors ()
  (loop repeat 1000
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
					       "hello"
					       ;; (random 1f0) slows cl-store crazily
					       #()))))))

(defun lots-of-keywords ()
  (loop for i fixnum from 0 below 100000
	collect (intern (format nil "~A" (random 250000)) 'keyword)))

(defun lots-of-symbols ()
  (loop for i fixnum from 0 below 100000
	collect (intern (format nil "~A" (random 250000)) 'cl-user)))

(defstruct blarg
  a
  b)

(defun lots-of-structure-objects ()
  (loop for i below 100000
        collect (make-blarg :a (random 1d0) :b (format nil "~A" (random 100)))))

(defclass c-blarg
    ()
  ((a :initarg :a)
   (b :initarg :b)))

(defun lots-of-standard-objects ()
  (loop for i below 100000
	collect (make-instance 'c-blarg :a (random 256) :b "hello")))

(defun simple-base-strings ()
  (loop for i below 100000
        collect (coerce (format nil "~A" (random 1000000)) 'simple-base-string)))

(defun simple-strings ()
  (loop for i below 100000
        collect (format nil "~A~A" (random 1000000) #\U+03b1)))

(defun a-pile-of-tangled-conses (&optional (number 20000))
  (let ((a (make-array number)))
    (loop for n below number do (setf (svref a n) (cons nil nil)))
    (loop repeat (* 10 number)
	  do (setf (car (svref a (random number)))
		   (svref a (random number)))
	     (setf (cdr (svref a (random number)))
		  (svref a (random number))))
    a))

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
    
    
			       

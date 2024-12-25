(quicklisp:quickload "cl-store")
(quicklisp:quickload "hyperluminal-mem")
(require 'sb-sprof)

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

(defun test-cl-store-faster-on-data
    (data &key (track-references t) (support-shared-list-structures nil) (repeats 100)
            (read t) (write t))
  (let* ((cl-store-faster:*support-shared-list-structures* support-shared-list-structures)
	 (cl-store-faster:*track-references* track-references)
	 (size (length (cl-store-faster:store nil data)))
         (output-size-MB (/ size 1e6))
         (store (make-array size :element-type '(unsigned-byte 8))))
    (format t "CL-STORE-FASTER~%")
    (format t " OUTPUT SIZE: ~,2f MB~%" output-size-MB)
    (when write
      (timed (" CL-STORE-FASTER WRITE:" repeats output-size-MB)
        (dotimes (x repeats) (cl-store-faster:store store data))))
    (when read
      (timed (" CL-STORE-FASTER READ :" repeats output-size-MB)
        (dotimes (x repeats) (cl-store-faster:restore store))))
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

(defun test-on-data (data &key (hlmem t) (cl-store t) (cl-store-faster t))
  (when hlmem
    (test-hlmem-on-data data))
  (when cl-store-faster
    (test-cl-store-faster-on-data data :track-references (not hlmem)
                                       :support-shared-list-structures (and (not hlmem)
                                                                            (not cl-store))))
  (when cl-store
    (test-cl-store-on-data data :check-for-circs (not hlmem))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 8.00 MB
;;  HLMEM WRITE: 2.28 ms at 3509 MB/sec
;;  HLMEM READ : 4.60 ms at 1739 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 3.00 MB
;;  CL-STORE-FASTER WRITE: 5.60 ms at 536 MB/sec
;;  CL-STORE-FASTER READ : 11.84 ms at 253 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 5.00MB
;;  CL-STORE WRITE: 59.19 ms at 84 MB/sec
;;  CL-STORE READ : 55.19 ms at 91 MB/sec
(defun long-list-of-small-integers (&optional (n 1000000))
  (make-list n :initial-element (random 256)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 16.00 MB
;;  HLMEM WRITE: 27.04 ms at 592 MB/sec
;;  HLMEM READ : 17.72 ms at 903 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 10.00 MB
;;  CL-STORE-FASTER WRITE: 9.48 ms at 1055 MB/sec
;;  CL-STORE-FASTER READ : 18.08 ms at 553 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 48.00MB
;;  CL-STORE WRITE: 598.34 ms at 80 MB/sec
;;  CL-STORE READ : 733.13 ms at 65 MB/sec
(defun long-list-of-random-double-floats (&optional (n 1000000))
  (make-list n :initial-element (random 1d0)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 8.00 MB
;;  HLMEM WRITE: 3.08 ms at 2598 MB/sec
;;  HLMEM READ : 6.12 ms at 1307 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 6.00 MB
;;  CL-STORE-FASTER WRITE: 8.64 ms at 695 MB/sec
;;  CL-STORE-FASTER READ : 13.40 ms at 448 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 22.00MB
;;  CL-STORE WRITE: 295.57 ms at 74 MB/sec
;;  CL-STORE READ : 426.76 ms at 52 MB/sec
(defun long-list-of-random-single-floats (&optional (n 1000000))
  (make-list n :initial-element (random 1f0)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 80.02 MB
;;  HLMEM WRITE: 31.60 ms at 2532 MB/sec
;;  HLMEM READ : 40.08 ms at 1997 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 10.01 MB
;;  CL-STORE-FASTER WRITE: 1.20 ms at 8339 MB/sec
;;  CL-STORE-FASTER READ : 2.60 ms at 3849 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 10.01MB
;;  CL-STORE WRITE: 80.79 ms at 124 MB/sec
;;  CL-STORE READ : 32.40 ms at 309 MB/sec
;; No surpise here since I just blit them out
(defun long-list-of-big-ub8-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 10000
	       collect 123)
         '(simple-array (unsigned-byte 8) (*)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 1.27 MB
;;  HLMEM WRITE: 7.96 ms at 160 MB/sec
;;  HLMEM READ : 14.48 ms at 88 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.26 MB
;;  CL-STORE-FASTER WRITE: 0.12 ms at 10468 MB/sec
;;  CL-STORE-FASTER READ : 0.16 ms at 7851 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 40.05MB
;;  CL-STORE WRITE: 549.55 ms at 73 MB/sec
;;  CL-STORE READ : 513.15 ms at 78 MB/sec
(defun long-list-of-big-simple-bit-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 10000
	       collect (random 1))
         '(simple-array bit (*)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 16.02 MB
;;  HLMEM WRITE: 34.48 ms at 465 MB/sec
;;  HLMEM READ : 13.96 ms at 1147 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 8.01 MB
;;  CL-STORE-FASTER WRITE: 0.76 ms at 10535 MB/sec
;;  CL-STORE-FASTER READ : 2.00 ms at 4003 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 47.06MB
;;  CL-STORE WRITE: 646.34 ms at 73 MB/sec
;;  CL-STORE READ : 885.11 ms at 53 MB/sec
(defun long-list-of-big-simple-double-float-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 1000
	       collect (random 1d0))
         '(simple-array double-float (*)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 20.49 MB
;;  HLMEM WRITE: 35.16 ms at 583 MB/sec
;;  HLMEM READ : 30.16 ms at 680 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 8.06 MB
;;  CL-STORE-FASTER WRITE: 24.88 ms at 324 MB/sec
;;  CL-STORE-FASTER READ : 91.19 ms at 88 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 27.78MB
;;  CL-STORE WRITE: 392.76 ms at 71 MB/sec
;;  CL-STORE READ : 417.56 ms at 67 MB/sec
;; and using referencing to deduplicate the double-floats
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 3.38 MB
;;  CL-STORE-FASTER WRITE: 91.27 ms at 37 MB/sec
;;  CL-STORE-FASTER READ : 16.92 ms at 200 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 4.88MB
;;  CL-STORE WRITE: 107.59 ms at 45 MB/sec
;;  CL-STORE READ : 112.79 ms at 43 MB/sec
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

;; With reference counting on everyone
;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 3.20 MB
;;  HLMEM WRITE: 18.52 ms at 173 MB/sec
;;  HLMEM READ : 33.32 ms at 96 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 0.99 MB
;;  CL-STORE-FASTER WRITE: 37.32 ms at 26 MB/sec
;;  CL-STORE-FASTER READ : 29.08 ms at 34 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 1.24MB
;;  CL-STORE WRITE: 59.99 ms at 21 MB/sec
;;  CL-STORE READ : 69.99 ms at 18 MB/sec
(defun lots-of-keywords ()
  (loop for i fixnum from 0 below 100000
	collect (intern (format nil "~A" (random 250000)) 'keyword)))

;; With referencing counting on everyone
;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 3.20 MB
;;  HLMEM WRITE: 8.56 ms at 374 MB/sec
;;  HLMEM READ : 18.92 ms at 169 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.15 MB
;;  CL-STORE-FASTER WRITE: 22.88 ms at 50 MB/sec
;;  CL-STORE-FASTER READ : 18.24 ms at 63 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 1.24MB
;;  CL-STORE WRITE: 35.20 ms at 35 MB/sec
;;  CL-STORE READ : 51.59 ms at 24 MB/sec
(defun lots-of-symbols ()
  (loop for i fixnum from 0 below 100000
	collect (intern (format nil "~A" (random 250000)) 'cl-user)))

(defstruct blarg
  a
  b)

;; hyperluminal mem needs an extension for this so skipping it for now
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.79 MB
;;  CL-STORE-FASTER WRITE: 56.11 ms at 32 MB/sec
;;  CL-STORE-FASTER READ : 11.32 ms at 158 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 6.69MB
;;  CL-STORE WRITE: 168.78 ms at 40 MB/sec
;;  CL-STORE READ : 184.78 ms at 36 MB/sec
(defun lots-of-structure-objects ()
  (loop for i below 100000
        collect (make-blarg :a (random 1d0) :b (format nil "~A" (random 100)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 2.40 MB
;;  HLMEM WRITE: 4.04 ms at 594 MB/sec
;;  HLMEM READ : 2.80 ms at 857 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 0.99 MB
;;  CL-STORE-FASTER WRITE: 2.68 ms at 369 MB/sec
;;  CL-STORE-FASTER READ : 2.92 ms at 339 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 0.99MB
;;  CL-STORE WRITE: 11.60 ms at 85 MB/sec
;;  CL-STORE READ : 8.00 ms at 124 MB/sec
(defun simple-base-strings ()
  (loop for i below 100000
        collect (coerce (format nil "~A" (random 1000000)) 'simple-base-string)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 2.40 MB
;;  HLMEM WRITE: 4.56 ms at 526 MB/sec
;;  HLMEM READ : 3.12 ms at 769 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.19 MB
;;  CL-STORE-FASTER WRITE: 3.92 ms at 303 MB/sec
;;  CL-STORE-FASTER READ : 9.08 ms at 131 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 2.08MB
;;  CL-STORE WRITE: 20.00 ms at 104 MB/sec
;;  CL-STORE READ : 12.00 ms at 173 MB/sec
(defun simple-strings ()
  (loop for i below 100000
        collect (format nil "~A~A" (random 1000000) #\U+03b1)))

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
;;  HLMEM WRITE: 2.32 ms at 3448 MB/sec
;;  HLMEM READ : 5.04 ms at 1587 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 3.00 MB
;;  CL-STORE-FASTER WRITE: 3.60 ms at 833 MB/sec ;; (/ 2e6 3.6e-3) = 550Mobjs/second
;;  CL-STORE-FASTER READ : 8.20 ms at 366 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 5.00MB
;;  CL-STORE WRITE: 59.20 ms at 84 MB/sec
;;  CL-STORE READ : 50.40 ms at 99 MB/sec
(defun long-list-of-small-integers (&optional (n 1000000))
  (make-list n :initial-element (random 256)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 8.00 MB ;; bit tagging, not byte tagging
;;  HLMEM WRITE: 2.24 ms at 3571 MB/sec
;;  HLMEM READ : 5.04 ms at 1587 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 10.00 MB
;;  CL-STORE-FASTER WRITE: 3.84 ms at 2604 MB/sec
;;  CL-STORE-FASTER READ : 9.48 ms at 1055 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 38.00MB
;;  CL-STORE WRITE: 422.39 ms at 90 MB/sec
;;  CL-STORE READ : 293.20 ms at 130 MB/sec
(defun long-list-of-random-fixnums (&optional (n 1000000))
  (make-list n :initial-element (random (- (expt 2 61) (expt 2 60)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 16.00 MB
;;  HLMEM WRITE: 23.28 ms at 687 MB/sec
;;  HLMEM READ : 15.68 ms at 1020 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 10.00 MB
;;  CL-STORE-FASTER WRITE: 13.44 ms at 744 MB/sec
;;  CL-STORE-FASTER READ : 13.84 ms at 723 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 48.00MB
;;  CL-STORE WRITE: 577.99 ms at 83 MB/sec
;;  CL-STORE READ : 701.99 ms at 68 MB/sec
(defun long-list-of-random-double-floats (&optional (n 1000000))
  (make-list n :initial-element (random 1d0)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 8.00 MB
;;  HLMEM WRITE: 2.96 ms at 2703 MB/sec
;;  HLMEM READ : 5.96 ms at 1342 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 6.00 MB
;;  CL-STORE-FASTER WRITE: 6.84 ms at 877 MB/sec
;;  CL-STORE-FASTER READ : 9.08 ms at 661 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 22.00MB
;;  CL-STORE WRITE: 307.20 ms at 72 MB/sec
;;  CL-STORE READ : 414.40 ms at 53 MB/sec
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
;;  HLMEM WRITE: 7.88 ms at 161 MB/sec
;;  HLMEM READ : 14.24 ms at 89 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.26 MB
;;  CL-STORE-FASTER WRITE: 0.12 ms at 10467 MB/sec
;;  CL-STORE-FASTER READ : 0.12 ms at 10467 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 40.05MB
;;  CL-STORE WRITE: 551.19 ms at 73 MB/sec
;;  CL-STORE READ : 490.39 ms at 82 MB/sec
(defun long-list-of-big-simple-bit-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 10000
	       collect (random 1))
         '(simple-array bit (*)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 16.02 MB
;;  HLMEM WRITE: 34.68 ms at 462 MB/sec
;;  HLMEM READ : 13.60 ms at 1178 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 8.01 MB
;;  CL-STORE-FASTER WRITE: 0.84 ms at 9531 MB/sec
;;  CL-STORE-FASTER READ : 1.60 ms at 5004 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 47.06MB
;;  CL-STORE WRITE: 577.99 ms at 81 MB/sec
;;  CL-STORE READ : 831.99 ms at 57 MB/sec
(defun long-list-of-big-simple-double-float-vectors ()
  (loop repeat 1000
	collect
	(coerce
	 (loop for i fixnum from 0 below 1000
	       collect (random 1d0))
         '(simple-array double-float (*)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 20.48 MB
;;  HLMEM WRITE: 34.12 ms at 600 MB/sec
;;  HLMEM READ : 30.96 ms at 662 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 8.06 MB
;;  CL-STORE-FASTER WRITE: 25.20 ms at 320 MB/sec
;;  CL-STORE-FASTER READ : 43.76 ms at 184 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 27.78MB
;;  CL-STORE WRITE: 448.79 ms at 62 MB/sec
;;  CL-STORE READ : 408.40 ms at 68 MB/sec
;; and using referencing to deduplicate the double-floats and strings
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 3.38 MB
;;  CL-STORE-FASTER WRITE: 65.52 ms at 52 MB/sec
;;  CL-STORE-FASTER READ : 12.32 ms at 274 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 4.88MB
;;  CL-STORE WRITE: 129.60 ms at 38 MB/sec
;;  CL-STORE READ : 110.40 ms at 44 MB/sec
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

;; With reference tracking on everyone
;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 3.20 MB
;;  HLMEM WRITE: 18.52 ms at 173 MB/sec
;;  HLMEM READ : 33.32 ms at 96 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.15 MB
;;  CL-STORE-FASTER WRITE: 28.56 ms at 40 MB/sec
;;  CL-STORE-FASTER READ : 29.36 ms at 39 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 1.24MB
;;  CL-STORE WRITE: 52.80 ms at 24 MB/sec
;;  CL-STORE READ : 63.60 ms at 20 MB/sec
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
;;  CL-STORE-FASTER WRITE: 31.88 ms at 36 MB/sec
;;  CL-STORE-FASTER READ : 32.12 ms at 36 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 1.24MB
;;  CL-STORE WRITE: 56.00 ms at 22 MB/sec
;;  CL-STORE READ : 70.00 ms at 18 MB/sec
(defun lots-of-symbols ()
  (loop for i fixnum from 0 below 100000
	collect (intern (format nil "~A" (random 250000)) 'cl-user)))

(defstruct blarg
  a
  b)

;; hyperluminal mem needs an extension for this so skipping it for now
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.79 MB
;;  CL-STORE-FASTER WRITE: 49.00 ms at 37 MB/sec
;;  CL-STORE-FASTER READ : 9.24 ms at 194 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 6.69MB
;;  CL-STORE WRITE: 171.20 ms at 39 MB/sec
;;  CL-STORE READ : 182.40 ms at 37 MB/sec
(defun lots-of-structure-objects ()
  (loop for i below 100000
        collect (make-blarg :a (random 1d0) :b (format nil "~A" (random 100)))))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 2.40 MB
;;  HLMEM WRITE: 4.00 ms at 600 MB/sec
;;  HLMEM READ : 2.48 ms at 968 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 0.99 MB
;;  CL-STORE-FASTER WRITE: 2.12 ms at 466 MB/sec
;;  CL-STORE-FASTER READ : 2.44 ms at 405 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 0.99MB
;;  CL-STORE WRITE: 12.00 ms at 82 MB/sec
;;  CL-STORE READ : 7.60 ms at 130 MB/sec
(defun simple-base-strings ()
  (loop for i below 100000
        collect (coerce (format nil "~A" (random 1000000)) 'simple-base-string)))

;; HYPERLUMINAL-MEM
;;  OUTPUT SIZE: 2.40 MB
;;  HLMEM WRITE: 4.52 ms at 531 MB/sec
;;  HLMEM READ : 3.12 ms at 769 MB/sec
;; CL-STORE-FASTER
;;  OUTPUT SIZE: 1.19 MB
;;  CL-STORE-FASTER WRITE: 4.04 ms at 294 MB/sec
;;  CL-STORE-FASTER READ : 8.68 ms at 137 MB/sec
;; CL-STORE
;;  OUTPUT SIZE: 2.08MB
;;  CL-STORE WRITE: 23.20 ms at 90 MB/sec
;;  CL-STORE READ : 11.60 ms at 179 MB/sec
(defun simple-strings ()
  (loop for i below 100000
        collect (format nil "~A~A" (random 1000000) #\U+03b1)))

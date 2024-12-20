(quicklisp:quickload "cl-store")
(require 'sb-sprof)

(defun long-simple-list ()
  (let ((a (loop repeat 1000000 collect 123)))
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      ;;(sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store-faster:store-to-file "blarg.bin" a))))
    (time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin")))
    (gc :full t)
    ;; Here we average 4 bytes per
    ;; 1 byte for each cons,
    ;; 1 byte for a reference
    ;; and two bytes for the reference?
    (time (dotimes (x 10) (cl-store:store a "blarg.bin")))
    (time (dotimes (x 10) (cl-store:restore "blarg.bin")))
    )
  (gc :full t))

;; cl-store-faster generates 3MB files, cl-store generates 4MB files.
;; WHICH         |  WRITING  |  READING  | TEST
;;---------------+-----------+-----------+
;;cl-store       |     625 ms|     700 ms| 10x 1M long list of 'a
;;cl-store-faster|     575 ms|     150 ms| 10x 1M long list of 'a with precomputed dispatch
;;cl-store-faster|     475 ms|     150 ms| 10x 1M long list of 'a with no precomputed dispatch
;;cl-store       |     675 ms|     675 ms| 10x 1M long list of 123
;;cl-store-faster|     425 ms|     115 ms| 10x 1M long list of 123 with precomputed dispatch
;;cl-store-faster|     325 ms|     115 ms| 10x 1M long list of 123 with no precomputed dispatch

;; pre-computed dispatch is slower... probably access to the dispatch list
;; Because of the dispatch array we cons a bit more on writing and it's actually slower!
;; Part of the problem is we are doing 1 million entries into the array, and it's not
;; a simple array.  we access globals multiple times to get to it.  So probably 10 ns
;; wasted each which adds up 100 ms or so.  Not so easy to fix given how fast things are,
;; it's weird that its faster to do the etypecase dispatch twice, but SBCL has precompiled
;; it to a perfect hash, so hard to beat!

;; Anyhow without the precomputed dispatch we are nominally storing 30 MB in 0.5 second which
;; is 60 MB/sec which is nowhere close enough to disk bandwidth limited (this is roughly 20 ns
;; per element, but this is a cache hot operation, so all the dispatch is zoomed through).

;; WARNING: sbcl hashing on single floats is terrible, so cl-store does not finish
(defun long-float-array (&optional (random nil))
  (let ((a (coerce (loop repeat 1000000 collect (if random (random 1f0) 1f0))
		   '(simple-array single-float (*)))))
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      ;;(sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store-faster:store-to-file "blarg.bin" a)))
      (time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin"))))
    (gc :full t)
    (time (dotimes (x 10) (cl-store:store a "blarg.bin")))
    (time (dotimes (x 10) (cl-store:restore "blarg.bin")))
    )
  (gc :full t))
;; Double float non-random (you can see the hot branch predicted path going zoom)
;; CL-STORE-FASTER: 145 ms write /  35 ms read; half is system time on write
;; CL-STORE:        600 ms write / 625 ms read.  Not a surprise of course.
;; Double-float random (here we are puthash limited tracking double-float references)
;; CL-STORE-FASTER:   767 ms write /    40 ms read
;; CL-STORE:        11400 ms write / 10825 ms read (this is because double float storing is slow)
;; Single float non-random
;; CL-STORE-FASTER:    83 ms write /    20 ms read
;; CL-STORE:          540 ms write /   670 ms read
;; Single float random
;; CL-STORE-FASTER:   100 ms write /    25 ms read (half is system time on write)
;; CL-STORE:          DNF ms write /   DNF ms read (terrible single float hashing stuff? on SBCL)

(defun long-complex-list ()
  (let ((a (loop repeat 1000000 collect (if (> (random 1000) 800)
					    1234
					    ;; (complex 1d0) ;; cl-store chokes
					    ;; (random 1d0) ;; cl-store chokes
					    (if (> (random 100) 50)
						;;(random 1f0) ;; <- makes cl-store take forever!
						'blarg
						(if (> (random 100) 50)
						    (cons (random 30) 2)
						    (if (= (random 2) 1)
							"hello"
							;; (random 1f0) slows cl-store crazily
							#())))))))
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      ;;      (sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store-faster:store-to-file "blarg.bin" a))))
    (time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin")))
    ;; (assert (equalp (cl-store-faster:restore-from-file "blarg.bin") a))
    ;; (gc :full t)
    ;;(sb-sprof:with-profiling (:report :graph)
    (time (dotimes (x 10) (cl-store:store a "blarg.bin")))
    (time (dotimes (x 10) (cl-store:restore "blarg.bin")))
    )
  (gc :full t))

;; Without the random single floats, otherwise cl-store takes forever
;; CL-STORE-FASTER: 925 ms write /  265 ms read
;; CL-STORE:       1400 ms write / 1400 ms read

(defun long-random-double-float-list ()
  (let ((a (loop repeat 1000000 collect (random 1d0))))
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      ;;      (sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store-faster:store-to-file "blarg.bin" a))))
    (time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin")))
    ;; (assert (equalp (cl-store-faster:restore-from-file "blarg.bin") a))
    (gc :full t)
    ;;(sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store:store a "blarg.bin")))
    (time (dotimes (x 10) (cl-store:restore "blarg.bin"))))
  (gc :full t))
;; long-random-double-float-list
;; CL-STORE-FASTER: 3500 ms write /   315 ms read
;; CL-STORE:       12700 ms write / 11000 ms read

(defun single-float-hash-test ()
  (let ((ht (Make-hash-table :test 'eql)))
    (loop repeat 1000000
	  ;;for val = (expt 2d0 (- (random 616d0) 308d0))
	  for val = (if (> (random 1000) 800)
			(random 1f0)
			(random 1f0))
	  do (incf (gethash (sxhash val) ht 0)))
    (let ((sum 0)
	  (max 0))
      (maphash (lambda (k v)
		 (setf max (max v max))
		 (incf sum v))
	       ht)
      (format t "HT size is ~A, maximum collisions is ~A, average collisions ~A~%"
	      (hash-table-size ht)
	      max
	      (* 1f0 (/ sum (hash-table-count ht)))))))


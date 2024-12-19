(quicklisp:quickload "cl-store")
(require 'sb-sprof)

(defun long-simple-list ()
  (let ((a (loop repeat 1000000 collect 'a)))
    ;; The reference should be 2 bytes (a tag and a value)
    ;; Plus 1 byte for each cons, so 3 MB
    ;; We will access the reference table 4 times, on the reference counting
    ;; step we do it once for each cons, once for each a, on the writing time
    ;; we do it once for each cons and once for each a.
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      (sb-sprof:with-profiling (:report :graph)
	(time (dotimes (x 100) (cl-store-faster:store-to-file "blarg.bin" a)))))
      ;;(time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin"))))
    (gc :full t)
    ;; Here we average 4 bytes per
    ;; 1 byte for each cons,
    ;; 1 byte for a reference
    ;; and two bytes for the reference?
    ;; (time (dotimes (x 10) (cl-store:store a "blarg.bin")))
    ;; (time (dotimes (x 10) (cl-store:restore "blarg.bin")))
    )
  (gc :full t))

;; WHICH         |  WRITING  |  READING  | TEST
;;---------------+-----------+-----------+
;;cl-store       |     650 ms|     700 ms| 10x 1M long list of 'a
;;cl-store-faster|     750 ms|     150 ms| 10x 1M long list of 'a with precomputed dispatch
;;cl-store-faster|     575 ms|     140 ms| 10x 1M long list of 'a with no precomputed dispatch
;;cl-store-faster|     490 ms|     145 ms| " no precomputed dispatch tweaks
;;cl-store-faster|     605 ms|     145 ms| " precomputed dispatch with tweaks
;;cl-store       |     675 ms|     730 ms| 10x 1M long list of 123
;;cl-store-faster|     525 ms|     110 ms| 10x 1M long list of 123
;;cl-store-faster|     415 ms|     110 ms| 10x 1M long list of 123 with no precomputed dispatch


;; Because of the dispatch array we cons a bit more on writing and it's actually slower!
;; Part of the problem is we are doing 1 million entries into the array, and it's not
;; a simple array.  we access globals multiple times to get to it.  So probably 10 ns
;; wasted each which adds up 100 ms or so.  Not so easy to fix given how fast things are,
;; it's weird that its faster to do the etypecase dispatch twice, but SBCL has precompiled
;; it to a perfect hash, so hard to beat!

;; So tweaking globals to be def-globals and some other stuff helps, it's mainly the jumping
;; in the simple vector during /storage that dominates it seems.

;; Anyhow without the precomputed dispatch we are nominally storing 30 MB in 0.5 second which
;; is 60 MB/sec which is nowhere close enough to disk bandwidth limited (this is roughly 20 ns
;; per element, but this is a cache hot operation, so all the dispatch is zoomed through).

(defun long-double-float-array ()
  (let ((a (coerce (loop repeat 1000000 collect 1d0)
		   '(simple-array double-float (*)))))
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

;; CL-STORE-FASTER: 143 ms write / 35 ms read; half system time on write
;; CL-STORE: 587 ms write / 643 ms read.  Not a surprise of course.


;; OK so CL-STORE is *TERRIBLE* on single floats
(defun long-complex-list ()
  (let ((a (loop repeat 1000000 collect (if (> (random 1000) 800)
				      ;; (complex 1 2)
				      (random 1d0)
				      (if (> (random 100) 50)
					  'a
					  (if (> (random 100) 50)
					      "hello" ;; (cons 1 2)
					      (if (= (random 2) 1)
						  5;; "hello"
						  6;; "bye"
						  ;; #()
						  )))))))
    (gc :full t)
    (let ((cl-store-faster::*support-shared-list-structures* nil))
      ;;      (sb-sprof:with-profiling (:report :graph)
      (time (dotimes (x 10) (cl-store-faster:store-to-file "blarg.bin" a))))
    (time (dotimes (x 10) (cl-store-faster:restore-from-file "blarg.bin")))
    ;; (assert (equalp (cl-store-faster:restore-from-file "blarg.bin") a))
    ;; (gc :full t)
    (time (dotimes (x 1) (cl-store:store a "blarg.bin")))
    (time (dotimes (x 1) (cl-store:restore "blarg.bin"))))
  (gc :full t))

;; CL-STORE-FASTER:  900 ms write /  225 ms read
;; CL-STORE:       40903 ms write / 2750 ms read

;; CL-STORE-FASTER: 1500 ms write /  230 ms read
;; CL-STORE:       40800 ms write / 2700 ms read

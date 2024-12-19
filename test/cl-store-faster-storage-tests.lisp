(defpackage #:cl-store-faster-storage-tests
  (:use #:common-lisp #:parachute #:cl-store-faster #:flexi-streams))

(in-package #:cl-store-faster-storage-tests)

(define-test write-test
  (loop
    with storage-size = 32
    with expected-output = nil
    for chunk-size-limit in (list storage-size (* 3 storage-size))
    for real-output
      = (flexi-streams:with-output-to-sequence (write-str)
	  (let ((storage (cl-store-faster::make-output-storage/stream write-str storage-size)))
	    (setf expected-output
		  (loop for item from 1 to 10
			for chunk-size = (random storage-size)
			do
			   (ensure-enough-room-to-write storage chunk-size)
			   (fill (storage-store storage) item
				 :start (storage-offset storage)
				 :end (+ (storage-offset storage) chunk-size))
			   (incf (storage-offset storage) chunk-size)
			appending (make-list chunk-size :initial-element item)))
	    (flush-storage storage)))
    do
       (is '= (length real-output) (length expected-output))
       (true (every #'= real-output expected-output))))


(in-package :cl-binary-store)

(defstruct tcp-instruction
  (signal 0 :type (unsigned-byte 8)))

(defconstant +start-timing+ 0)
(defconstant +end-timing+ 1)
(defconstant +shutdown+ 2)

;;#+#.(cl:when (cl:find-package :usocket) t)
(defun tcp-server (&key (port 50007) (client-sends-object-size-first nil))
  "Here we only use the socket for transferring cl-binary-store data, so no need to use a
 protocol that pre-calculates object sizes."
  (usocket:with-socket-listener (server "127.0.0.1" port :element-type '(unsigned-byte 8))
    (usocket:with-connected-socket (conn (usocket:socket-accept server))
      (let* ((stream (usocket:socket-stream conn))
             (timer-start 0)
             (timer-end 0)
             (codespace (gethash *read-version* *codespaces*)))
        (declare (type fixnum timer-start timer-end))
        (labels ((report-timer (i)
                   (format t "~d objects took ~,3f us~%"
                           i (/ (- timer-end timer-start)
                                internal-time-units-per-second 1f-6)))
                 (start-timer ()
                   (setf timer-start (get-internal-real-time)))
                 (end-timer ()
                   (setf timer-end (get-internal-real-time)))
                 (call-with-read-storage (thunk)
                   (with-storage/read (storage :flusher (make-read-into-storage/stream stream)
                                               :stream stream)
                     (funcall thunk storage)))
                 (receive-objects (from)
                   (loop for i fixnum from 0
                         for obj = (if client-sends-object-size-first
                                       (cl-binary-store::restore-from-stream/single-bounded-object
                                        from codespace)
                                       (cl-binary-store::restore-from-read-storage/single-object
                                        from codespace))
                         do
                            (cond
                              ((typep obj 'tcp-instruction)
                               (ecase (tcp-instruction-signal obj)
                                 (#.+start-timing+
                                  (start-timer))
                                 (#.+end-timing+
                                  (end-timer)
                                  (report-timer i))
                                 (#.+shutdown+
                                  (end-timer)
                                  (report-timer i)
                                  (return-from tcp-server i))))
                              (t
                               ;; (format t "Got object ~A~%" obj)
                               )))))
          (if client-sends-object-size-first
              (receive-objects stream)
              (call-with-read-storage (lambda (storage) (receive-objects storage)))))))))

;;#+#.(cl:when (cl:find-package :usocket) t)
(defun tcp-client (&key (port 50007) (send-object-size-first nil))
  (let ((cl-binary-store:*track-references* nil))
    ;; reduces overhead significantly, but may not match your needs
    (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type '(unsigned-byte 8) :nodelay t)
      (labels ((send (object)
                 (if send-object-size-first
                     (cl-binary-store:store-to-stream/single-bounded-object stream object)
                     (cl-binary-store:store-to-stream stream object))))
        (send (make-tcp-instruction :signal +start-timing+))
        (dotimes (j 10)
          (dotimes (i 200000)
            (send i))
          (send (make-tcp-instruction :signal +end-timing+)))
        (send (make-tcp-instruction :signal +shutdown+)))
      (force-output stream))))

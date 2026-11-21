

(let ((python-input "/tmp/input.cbs")
      (python-output "/tmp/output.cbs")
      (obj (coerce (iota 10 :start -5) '(simple-array (signed-byte 64) (*)))))
  (cl-binary-store:store python-input obj)
  (handler-bind
      ((error (lambda (e) (declare (ignore e)) (invoke-restart (find-restart 'continue)))))
    (print
     (uiop:run-program (list "python3" "cl_binary_store_loopback.py" python-input python-output)
                       :output :string :error-output :string)))
  (let ((result (cl-binary-store:restore python-output)))
    (print (type-of result))
    result))

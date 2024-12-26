(defsystem #:example-extension
  :depends-on (#:cl-binary-store)
  :components ((:file "example-extension")
	       (:file "example-extension-2" :depends-on ("example-extension"))))

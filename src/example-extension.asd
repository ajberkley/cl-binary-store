(defsystem #:example-extension
  :depends-on (#:cl-binary-store)
  :components ((:file "example-extension")
	       (:file "example-extension-codespace" :depends-on ("example-extension"))))

(in-package :example-extension)

(define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
  ;; Disable storing and loading of double-floats because we hate them or something
  (delete-store double-float)
  (delete-restore cl-binary-store::+double-float-code+)
  ;; Add low-level support for something-else objects
  (defstore something-else (store-something-else obj storage store-object))
  (defrestore +test-code+ (restore-something-else restore-object)))

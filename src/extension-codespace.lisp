(in-package :example-extension)

;; Since we delete the basic codespace upon loading of this file
(define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
  ;; Disable storing and loading of double-floats!
  (delete-store double-float)
  (delete-restore cl-binary-store::+double-float-code+)
  (defstore something-else (store-something-else obj storage store-object))
  (defrestore +test-code+ (restore-something-else restore-object)))

;; If we do not want this codespace to even exist
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (delete-codespace +basic-codespace+))

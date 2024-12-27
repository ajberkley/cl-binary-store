(in-package :example-extension)

(define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
  (defstore something-else (store-something-else obj storage store-object))
  (defrestore +test-code+ (restore-something-else restore-object)))

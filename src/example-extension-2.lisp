(in-package :example-extension)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstore something-else (store-something-else obj storage store-object))
  (defrestore +test-code+ (restore-something-else restore-object))
  (rebuild-dispatch))

(defsystem #:cl-binary-store
  :version "1.0.0"
  :description "Fast serialization / deserialization library"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :long-name "Fast serialization / deserialization library"
  :pathname "src/"
  :depends-on (#:flexi-streams
	       #:babel
	       #:static-vectors #:alexandria
	       #-sbcl #:cffi)
  :components ((:file "features")
	       (:file "cl-binary-store")
	       (:file "cl-binary-store-user" :depends-on ("cl-binary-store"))
	       (:file "codespaces" :depends-on ("features"))
	       (:file "sbcl-special-hash-tables" :if-feature :sbcl)
	       (:file "type-discrimination")
	       (:file "object-info")
	       (:file "basic-codespace-codes")
	       (:file "reference-coding" :depends-on ("basic-codespace-codes"))
	       (:file "actions" :depends-on ("storage" "features" "basic-codespace-codes"))
	       (:file "basic-codespace" :depends-on ("sbcl-special-hash-tables"
						     "type-discrimination"
						     "features" "codespaces"
						     "actions" "object-info"
						     "basic-codespace-codes" "cons"
						     "reference-coding"))
	       (:file "sap-ref")
	       (:file "storage" :depends-on ("features" "cl-binary-store" "sap-ref"))
	       (:file "unsigned-bytes" :depends-on ("storage" "features" "cl-binary-store"
							      "basic-codespace-codes" "sap-ref"))
	       (:file "referrers-and-fixup" :depends-on ("unsigned-bytes" "features"
									  "basic-codespace-codes"))
	       (:file "numbers" :depends-on ("unsigned-bytes" "referrers-and-fixup"
							      "features" "sap-ref"
							      "basic-codespace-codes"))
	       (:file "reference-count" :depends-on ("actions" "numbers" "features"))
               (:file "magic-numbers" :depends-on ("actions" "numbers"))
	       (:file "end-action" :depends-on ("actions" "numbers"))
	       (:file "cons" :depends-on ("referrers-and-fixup" "numbers" "unsigned-bytes"
								"features"))
	       (:file "sbcl-utilities" :if-feature :sbcl :depends-on ("features"))
	       (:file "simple-array"
		:depends-on ("referrers-and-fixup" "numbers" "features" "sap-ref"))
	       (:file "simple-vector" :depends-on ("unsigned-bytes" "referrers-and-fixup"
								    "features" "numbers"))
	       (:file "symbols" :depends-on ("unsigned-bytes" "referrers-and-fixup"
							      "features" "numbers"))
	       (:file "array" :depends-on ("unsigned-bytes" "cons" "symbols" "numbers"
							    "referrers-and-fixup" "features"))
	       (:file "pathname" :depends-on ("referrers-and-fixup" "symbols" "numbers"
								    "unsigned-bytes" "features"))
	       (:file "hash-table" :depends-on ("referrers-and-fixup" "symbols" "numbers" "unsigned-bytes" "features"))
	       (:file "objects" :depends-on ("symbols" "simple-vector" "referrers-and-fixup" "numbers" "unsigned-bytes" "features" "codespaces"))
	       (:file "user" :depends-on ("basic-codespace" "cons" "storage" "features" "magic-numbers" "reference-count" "end-action")))
  :license :BSD-3
  :in-order-to ((asdf:test-op (asdf:test-op :cl-binary-store-tests))))

(defsystem #:cl-binary-store/tests
  :description "Unit tests for CL-BINARY-STORE"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :license :BSD-3
  :depends-on (#:parachute)
  :pathname "test/"
  :components ((:file "cl-binary-store-tests"))
  :perform (test-op (o c) (uiop:symbol-call :parachute :test :cl-binary-store-tests)))

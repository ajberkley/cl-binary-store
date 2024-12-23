(defsystem #:cl-store-faster
  :version "0.0.1"
  :description "Fast serialization / deserialization library"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :long-name "Fast serialization / deserialization library"
  :pathname "src/"
  :depends-on (#:flexi-streams)
  :components ((:file "features")
	       (:file "cl-store-faster")
	       (:file "cl-store-faster-extensions")
	       (:file "codes" :depends-on ("features"))
	       (:file "storage" :depends-on ("features"))
	       (:file "unsigned-bytes" :depends-on ("storage" "features"))
	       (:file "referrers-and-fixup" :depends-on ("unsigned-bytes" "features"))
	       (:file "numbers" :depends-on ("unsigned-bytes" "referrers-and-fixup"
							      "features"))
               (:file "actions" :depends-on ("unsigned-bytes" "storage"))
	       (:file "reference-count" :depends-on ("actions" "numbers"))
               (:file "magic-numbers" :depends-on ("actions" "numbers"))
	       (:file "cons" :depends-on ("referrers-and-fixup" "numbers" "unsigned-bytes"
								"features"))
	       (:file "sbcl-utilities" :if-feature :sbcl :depends-on ("features"))
	       (:file "simple-array-sbcl" :if-feature :sbcl
		:depends-on ("referrers-and-fixup" "numbers" "features"))
	       (:file "simple-vector" :depends-on ("unsigned-bytes" "referrers-and-fixup"
								    "features"))
	       (:file "symbols" :depends-on ("unsigned-bytes" "referrers-and-fixup"
							      "features"))
	       (:file "array" :depends-on ("unsigned-bytes" "cons" "symbols" "numbers"
							    "referrers-and-fixup" "features"))
	       (:file "pathname" :depends-on ("referrers-and-fixup" "symbols" "numbers"
								    "unsigned-bytes" "features"))
	       (:file "hash-table" :depends-on ("referrers-and-fixup" "symbols" "numbers" "unsigned-bytes" "features"))
	       (:file "objects" :depends-on ("symbols" "simple-vector" "referrers-and-fixup" "numbers" "unsigned-bytes" "features"))

	       (:file "dispatch" :depends-on ("codes" "array" "symbols" "simple-vector" "cons"
						      "hash-table" "features" "actions" "magic-numbers"
						      "referrers-and-fixup" "numbers" "pathname"
						      "unsigned-bytes" "objects" "storage"
						      "reference-count"))
	       (:file "user" :depends-on ("dispatch" "storage" "features" "magic-numbers"))
	       )
  :license :BSD-3
  :in-order-to ((asdf:test-op (asdf:test-op :cl-store-faster-tests))))

(defsystem #:cl-store-faster-tests
  :description "Unit tests for CL-STORE-FASTER"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :license :BSD-3
  :depends-on (#:parachute)
  :pathname "test/"
  :components ((:file "cl-store-faster-tests"))
  :perform (test-op (o c) (uiop:symbol-call :parachute :test :cl-store-faster-tests)))

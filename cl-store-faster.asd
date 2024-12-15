(defsystem #:cl-store-faster
  :version "0.0.1"
  :description "Fast serialization / deserialization library"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :long-name "Fast serialization / deserialization library"
  :pathname "src/"
  :depends-on (#:flexi-streams)
  :components ((:file "cl-store-faster")
	       (:file "cl-store-faster-extensions")
	       (:file "codes")
	       (:file "storage")
	       (:file "unsigned-bytes" :depends-on ("storage"))
	       (:file "numbers" :depends-on ("unsigned-bytes"))
	       (:file "referrers-and-fixup" :depends-on ("unsigned-bytes"))
	       (:file "cons" :depends-on ("referrers-and-fixup"))
	       (:file "sbcl-utilities" :if-feature :sbcl)
	       (:file "simple-array-sbcl" :if-feature :sbcl)
	       (:file "simple-vector" :depends-on ("unsigned-bytes" "referrers-and-fixup"))
	       (:file "symbols" :depends-on ("unsigned-bytes"))
	       (:file "array" :depends-on ("unsigned-bytes" "cons" "symbols"
							    "referrers-and-fixup"))
	       (:file "pathname")
	       (:file "objects" :depends-on ("symbols" "simple-vector" "referrers-and-fixup"))
	       (:file "dispatch" :depends-on ("codes" "array" "symbols" "simple-vector" "cons"
						      "referrers-and-fixup" "numbers" "pathname"
						      "unsigned-bytes" "objects")))
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

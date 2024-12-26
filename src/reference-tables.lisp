(in-package :cl-binary-store)

;; Here we register the types of reference tables we want to keep track of

(register-references num-eq-refs (make-hash-table :test #'eq))
(register-references double-float-refs (make-hash-table :test #'double-float-=))
(register-references eq-refs (make-hash-table :test #'eq))

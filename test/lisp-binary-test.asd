(asdf:defsystem :lisp-binary-test
  :author ("Jeremy Phelps")
  :version "1"
  :license "GPLv3"
  :description  "Test the LISP-BINARY system."
  :depends-on (:lisp-binary)
  :components
  ((:file "unit-test")
   (:file "basic-test" :depends-on ("unit-test"))
   (:file "single-field-tests" :depends-on ("basic-test"))
   (:file "type-field-expansion-tests" :depends-on ("basic-test"))))

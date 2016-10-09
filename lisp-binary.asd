(asdf:defsystem :lisp-binary
  :author ("Jeremy Phelps")
  :version "1"
  :license "GPLv3"
  :description  "Declare binary formats as structs and then read and write them."
  :depends-on (:closer-mop :moptilities :flexi-streams :quasiquote-2.0
			   :cffi)
  :components
  ((:file "binary" :depends-on ("utils" "float" "integer" "simple-bit-stream" "reverse-stream"))
   (:file "simple-bit-stream" :depends-on ("integer"))
   (:file "reverse-stream" :depends-on ("integer"))
   (:file "integer" :depends-on ("utils"))
   (:file "float" :depends-on ("integer"))
   (:file "utils")))

(let ((asdf-version (when (find-package :asdf)
                      (let ((ver (symbol-value
                                  (or (find-symbol (string :*asdf-version*) :asdf)
                                      (find-symbol (string :*asdf-revision*) :asdf)))))
                        (etypecase ver
                          (string ver)
                          (cons (with-output-to-string (s)
                                  (loop for (n . m) on ver
                                    do (princ n s)
                                    (when m (princ "." s)))))
                          (null "1.0"))))))
  (unless (string>= asdf-version "3.1.5")
    (error "LISP-BINARY requires ASDF 3.1.5 or better. You have ~a" asdf-version)))

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

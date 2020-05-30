#-lisp-binary/never-use-own-asdf
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
    (pushnew :lisp-binary-upgrade-asdf *features*)))

(asdf:defsystem :lisp-binary
  :author ("Jeremy Phelps")
  :version "1"
  :license "GPLv3"
  :description  "Declare binary formats as structs and then read and write them."
  :depends-on (:closer-mop :moptilities :flexi-streams :quasiquote-2.0
			   :cffi)
  :components
  ((:file "binary-1" :depends-on ("utils" "float" "integer" "simple-bit-stream" "reverse-stream"))
   (:file "binary-2" :depends-on ("utils" "float" "integer" "simple-bit-stream" "reverse-stream" "binary-1"))
   (:file "types" :depends-on ("utils" "binary-1" "binary-2"))
   #+(and lisp-binary-upgrade-asdf
	  (not lisp-binary/never-use-own-asdf)) (:file "asdf")
   (:file "simple-bit-stream" :depends-on ("integer"))
   (:file "reverse-stream" :depends-on ("integer"))
   (:file "integer" :depends-on ("utils"))
   (:file "float" :depends-on ("integer"))
   (:file "utils")))

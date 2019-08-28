(format t "~%>>>>>>>>>>> Test program loading~%")
(load "init.lisp")
(load "fake-asdf.lisp")
(format t "~%>>>>>>>>>>>>> Loaded init file~%")
(ql:quickload :asdf)
(push '*default-pathname-defaults* asdf:*central-registry*)

(defmacro warnings-to-errors (&body body)
  `(handler-bind (((and warning
			#-ccl (not asdf/parse-defsystem::bad-system-name))
		   (lambda (exn) (error exn))))
     ,@body))

(defun local-projects-dir ()
  (with-open-file (in "local-projects-dir.txt")
    (let ((result (make-string (file-length in))))
      (read-sequence result in)
      result)))

(warnings-to-errors
 (fake-asdf-load (merge-pathnames (local-projects-dir) "lisp-binary.asd"))
 (fake-asdf-load "lisp-binary-test.asd"))

(format t "~%>>>>>>>> LISP-BINARY test system successfully loaded~%")

(lisp-binary-test::run-test)
#+sbcl (exit)
#+clisp (ext:exit)
#+ccl (ccl:quit)

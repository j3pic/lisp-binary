(format t "~%>>>>>>>>>>> Test program loading~%")
(load "init.lisp")
(format t "~%>>>>>>>>>>>>> Loaded init file~%")
(ql:quickload :asdf)
(push '*default-pathname-defaults* asdf:*central-registry*)

(defmacro warnings-to-errors (&body body)
  `(handler-bind ((warning
		   (lambda (exn) (error exn))))
     ,@body))

(warnings-to-errors
 (ql:quickload :lisp-binary-test))

(format t "~%>>>>>>>> LISP-BINARY test system successfully loaded~%")

(lisp-binary-test::run-test)
#+sbcl (exit)
#+clisp (ext:exit)
#+ccl (ccl:quit)

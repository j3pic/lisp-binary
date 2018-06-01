(load "init.lisp")
(ql:quickload :lisp-binary)
(format t "~%>>>>>>>> LISP-BINARY library successfully loaded~%")
(load "basic-test.lisp")

(format t "~%>>>>>>>>>> Test program successfully loaded~%")

(lisp-binary-test::run-test)
(exit)

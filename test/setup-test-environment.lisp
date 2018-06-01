(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(defun ql-util:press-enter-to-continue ()
  t)

(ql:add-to-init-file)
#+sbcl (exit)
#+ccl (ccl:quit)
#+clisp (ext:exit)

(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(defun ql-util:press-enter-to-continue ()
  t)

(ql:add-to-init-file)
#+(or sbcl clisp)(exit)
#+ccl (ccl:quit)

(defpackage :unit-test
  (:use :common-lisp)
  (:export :unit-test :do-tests))

(in-package :unit-test)

(defmacro aif (cond true &optional false)
  `(let ((it ,cond))
     (if it ,true ,false)))

(defmacro set-assoc (key place new-value)
  `(aif (assoc ,key ,place)
	(setf (cdr it) ,new-value)
	(push (cons ,key ,new-value) ,place)))
       

(defun sift (pred list)
  (loop for item in list
       if (funcall pred item)
       collect item into gold
       else collect item into dirt
       finally (return (values gold dirt))))

(defparameter *unit-tests* nil)

(defmacro unit-test (name &body body)
  `(progn (set-assoc ,name *unit-tests*
		(lambda ()
		  (handler-bind ((condition
				  (lambda (exn)
				    (declare (ignorable exn))
				    (aif (find-restart 'fail-test)
					 (invoke-restart it)))))
		    (compile ,name (list 'lambda nil
					 '(declare (optimize (speed 3) (debug 0)))
					 (list* 'let nil ',body)))
		    ;; The test has to be run twice: Once compiled and with full optimization,
		    ;; to make it the most likely that compiler macros will be used, and the
		    ;; second time interpreted, to make it unlikely that they'll be used.
		    ;; This way we test both the compiler macro and the real functions
		    ;; where compiler macros exist.
		    (eval (list* 'let nil ',body))
		    (funcall ,name)
		    t)))))

(defun do-tests ()
  (sift (lambda (result)
	  (eq (second result) :fail))
	(mapcar (lambda (test)
		  (format t "~%>>>>> ~a~%" (first test))
		  (list (first test)
			(restart-case
			    (progn
			      (funcall (cdr test))
			      :pass)
			  (fail-test () :fail)))) *unit-tests*)))

(defun run-test (name)
  (funcall (cdr (assoc name *unit-tests*))))

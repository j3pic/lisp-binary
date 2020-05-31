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

(defmacro unit-test (description &body body)
  `(progn (set-assoc ,description *unit-tests*
		(lambda ()
		  (handler-bind ((condition
				  (lambda (exn)
				    (declare (ignorable exn))
				    (aif (find-restart 'fail-test)
					 (invoke-restart it)))))
		    (eval (list* 'let nil ',body)))))
	  t))

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

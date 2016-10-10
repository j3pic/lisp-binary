(defpackage :fast-kw
  (:use :common-lisp)
  (:export :defkwfun))

(in-package :fast-kw)

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate
		 (cons 'string (mapcar #'symbol-name symbols)))
	  (symbol-package (car symbols))))

(defun lambda-keyword-p (symbol)
  (and (symbolp symbol)
       (char= (aref (symbol-name symbol) 0) #\&)))

(defun split-lambda-list (lambda-list)
  (let ((result '(&positional))
	(current-group nil))
    (loop for symbol in lambda-list
	 do (cond ((lambda-keyword-p symbol)
		   (push (reverse current-group) result)
		   (push symbol result)
		   (setf current-group nil))
		  (t (push symbol current-group))))
    (push (reverse current-group) result)
    (reverse result)))

(defun make-kw-macro-body (real-name flat-lambda-list)
  `(,real-name ,@flat-lambda-list))

(defmacro defkwfun (name lambda-list &body body)
  (let* ((parsed-lambda-list (split-lambda-list lambda-list))
	 (static-lambda-list (getf parsed-lambda-list '&positional))
	 (sections 
	  (remove '&positional
		  (remove-if-not #'symbolp parsed-lambda-list)))
	 (real-function-name (symbol-append name '- 'flat-arglist)))
    (setf static-lambda-list
	  (append static-lambda-list
		  (loop for section in sections
		     append
		       (loop for arg in (getf parsed-lambda-list section)
			  if (symbolp arg) collect arg
			  else if (and (listp arg)
				       (= (length arg) 3))
			  collect (first arg)
			  and collect (third arg)
			  else if (listp arg) collect (first arg)))))
    `(progn 
       (defun ,real-function-name ,static-lambda-list
	 ,@body)
       (defmacro ,name ,lambda-list
	 (make-kw-macro-body ',real-function-name (list ,@static-lambda-list))))))
       
	 
       

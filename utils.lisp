(cl:defpackage lisp-binary-utils
  (:use :common-lisp :moptilities)
  (:shadowing-import-from :closer-mop)
  (:export :subst* :struct-like-defclass :bind-class-slots :remove-plist-keys :recursive-find
	   :recursive-map :assoc-cdr :aif :awhen :it :destructuring-case :destructuring-lambda
	   :no-destructuring-match :recursive-find/collect :plist-replace
	   :recursive-find-if :recursive-mapcar :letf :relative-file-position :group :let-values* :let-values :divisiblep
	   :named-let :pushover :insert-before :has-sublist-p :find-sublist :recursive-find-sublist :remove-binding :mapseq
	   :with-file-position :simple-define-condition))

(in-package :lisp-binary-utils)

(defun divisiblep (num denom)
  (= (mod num denom) 0))

(defmacro simple-define-condition (name parent-classes slots)
  `(define-condition ,name ,parent-classes
                     ,(loop for slot-name in slots
                        collect (if (listp slot-name)
                                    slot-name
                                    `(,slot-name :initarg ,(intern (symbol-name slot-name) :keyword))))))

(define-condition no-destructuring-match (warning) ())

(defmacro named-let (name defs &body body)
  (let ((vars (mapcar #'first defs))
	(values (mapcar #'second defs)))
    `(labels ((,name ,vars ,@body))
       (,name ,@values))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun take-while (pred list)
    (loop for item = (car list)
       while (funcall pred item)
       collect (pop list) into taken
       finally (return (values taken list))))

  (defun recursive-find/collect (pred tree)
    (loop for (first . rest) on tree
       if (funcall pred first) collect first into result
       else if (listp first) append (recursive-find/collect pred first) into result
       unless (listp rest)
       return (if (funcall pred rest)
		  (append result (list rest))
		  result)
       finally (return result))))

  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro destructuring-case (expression &rest cases)
    "Matches the EXPRESSION against one of the CASES. Each CASE is of
the form (lambda-list &key where &body body) and can have a guard clause.

The first pattern that successfully DESTRUCTURING-BINDs against the EXPRESSION
without an error *and* satisfies the optional :WHERE expression is the one
that will be selected. OTHERWISE is supported and works just as in the CL:CASE
form.

Example:

   (destructuring-case '(1 2 3)
      ((a b c) :where (symbolp a)
       (declare (ignore b c))
       (list :symbol  a))
      ((a b &optional c) :where (number b)
       (declare (ignore a c)))
       (list :number b))
      ((a &rest b)
       (declare (ignore b))
       (list :whatever a))
      (otherwise
       :nothing-matched))

The DECLARE form is processed before the :WHERE clause even though it appears
after it.
"
    (let ((var (gensym))
	  (block-name (gensym))
	  (result-form nil))
      (loop for (lambda-list . body) in (reverse cases)
	 for case-implementation = (if (eq lambda-list 'otherwise)
				       `(return-from ,block-name (progn ,@body))
				       (let ((catch-tag (gensym))
					     (local-result (gensym))
					     (match-success-flag (gensym))
					     (where-clause (when (eq (car body) :where)
							     (pop body)
							     (pop body))))
					 (multiple-value-bind (declarations real-body)
					     (take-while (lambda (form)
							   (and (listp form)
								(eq (car form) 'declare)))
							 body)
					   (loop for var in (remove-duplicates
							     (recursive-find/collect
							      (lambda (elem)
								(and (symbolp elem)
								     (equalp (symbol-name elem) "_")))
							      lambda-list) :test #'eq)
					      do (push `(declare (ignore ,var)) declarations))
					   `(let ((,local-result
						   (let ((,match-success-flag nil))
						     (catch ',catch-tag
						       (handler-bind ((t (lambda (exn)
									   (declare (ignore exn))
									   (unless ,match-success-flag
									     (throw ',catch-tag ',catch-tag)))))
							 (destructuring-bind ,lambda-list ,var
							   ,@declarations
							   ,@(if where-clause
								 `((unless ,where-clause
								     (throw ',catch-tag ',catch-tag))))
							   (setf ,match-success-flag t)
							   (return-from ,block-name
							     (progn
							       ,@real-body))))))))
					      (if (eq ,local-result ',catch-tag)
						  ,result-form
						  ,local-result)))))
	 do (setf result-form case-implementation))
      `(let ((,var ,expression))
	 (block ,block-name
	   ,result-form))))
  (defmacro destructuring-lambda (lambda-list &body body)
    (let ((args (gensym)))
      `(lambda (&rest ,args)
	 (destructuring-case ,args
	   (,lambda-list ,@body)
	   (otherwise (warn 'no-destructuring-match)))))))

(defun group (list &key (test #'eql) (key #'identity))
  (let ((groups nil)
	(current-group nil))
    (loop for (first . rest) on list
       do (push first current-group)
	 (unless (and rest
		      (funcall test (funcall key first)
			       (funcall key (car rest))))
	   (push (reverse current-group) groups)
	   (setf current-group nil)))
    (reverse groups)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optimize-let-values (let let-values bindings body)
    (flet ((multiple-value-binding-p (binding)
	     (listp (car binding))))
      (let ((binding-groups
	     (group (reverse bindings)
		    :key #'multiple-value-binding-p))
	    (result `(progn ,@body)))
	(loop for bindings in binding-groups
	   do (setf result
		    `(,(if (multiple-value-binding-p (car bindings))
			   let-values
			   let) ,(reverse bindings) ,@(if (eq (car result) 'progn)
							  (cdr result)
							  (list result)))))
	result))))
  
(defmacro let-values* (bindings &body body)
  "Optimized implementation of LET-VALUES*. You can bind a multiple-value expression to
a list of variables, or you can bind a single value. Generates as few nested MULTIPLE-VALUE-BIND
and LET* forms as possible."
  (optimize-let-values 'let* 'let-values/stupid* bindings body))

(defmacro let-values/stupid* (bindings &body body)
  "A naive implementation of LET-VALUES*. Each binding must bind a multiple-value expression
to a list of variables, and will expand to a nested MULTIPLE-VALUE-BIND expression."
  (let ((result `(progn ,@body)))
    (loop for (variables expression) in (reverse bindings)
       do (setf result `(multiple-value-bind ,variables ,expression
			  ,@(if (eq (car result) 'progn)
				(cdr result)
				(list result)))))
    result))

(defun recursive-map (function tree)
  (mapcar (lambda (node)
	    (if (listp node)
		(recursive-map function node)
		(funcall function node)))
	  tree))

(defun subst* (bindings form &key (test #'eql))
  (recursive-map
   (lambda (node)
     (block mapper
       (loop for (var val) in bindings
	  when (funcall test node var) do (return-from mapper val))
       node)) form))

(defun expand-struct-like-defclass-slot (class-name name default-value &key type)
  `(,name :accessor ,(intern (format nil "~a-~a" class-name name))
	  :initform ,default-value
	  :initarg ,(intern (symbol-name name) :keyword)
	  ,@(if type `(:type ,type))))

(defun add-default-value-if-needed (def)
  (if (keywordp (car def))
      (cons nil def)
      def))

(defun assoc-cdr (item assoc-list &key (test #'eql))
  (loop for (car . cdr) in assoc-list
       if (funcall test item cdr) return (cons car cdr)))

(defmacro struct-like-defclass (name superclasses &rest slot-defs)
  "An extremely simplified version of DEFCLASS. Written because I needed to be able
to list the slots of a certain struct."
  `(progn (defparameter ,name 
	    (defclass ,name ,superclasses 
	      ,(loop for def in slot-defs collect
		    (if (listp def)
			(apply #'expand-struct-like-defclass-slot (cons name (add-default-value-if-needed def)))
			(expand-struct-like-defclass-slot name def nil)))))))

(defmacro bind-class-slots (class instance &body body)
  "Evaluates BODY with the slots from the CLASS bound to the values taken from
INSTANCE. The CLASS must be a class object at compile-time, so the macro can
extract the needed variable names for binding."
  (let ((instance-value (gensym))
	(slot-names (slot-names (eval class))))
    `(let ((,instance-value ,instance))
       (declare (ignorable ,instance-value))
       (let ,(loop for name in slot-names collect
		  `(,name (slot-value ,instance ',name)))
	 (declare (ignorable ,@slot-names))
       ,@body))))

(defun remove-plist-keys (plist &rest keys)
  (loop for (key value) of-type (keyword t) on plist by #'cddr
       unless (member key keys)
         collect key
         and collect value))	       	     	 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro aif (test then &optional else)
    `(let ((it ,test))
       (if it ,then ,else)))

  (defmacro awhen (test &rest body)
    `(aif ,test (progn ,@body)))
  (defmacro acond (&rest cases)
    (let ((result nil))
      (loop for (condition . body) in (reverse cases)
	 do (setf result
		  `(aif ,condition
			(progn ,@body)
			,result)))
      result)))

(defun recursive-find (item tree &key (test #'eql))
  (acond ((null tree)
	 nil)
	((atom tree)
	 (funcall test item tree))
	((consp tree)
	 (acond ((funcall test item (car tree))
		 (car tree))
		((recursive-find item (car tree) :test test)
		 it)
		(t (recursive-find item (cdr tree) :test test))))))


(defun list-begins-with-p (list prefix &key (test #'eql))
  (named-let local-loop
      ((list list)
       (prefix* prefix))
    (cond ((endp prefix*)
	   t)
	  ((endp list)
	   nil)
	  ((not (funcall test (car list) (car prefix*)))
	   nil)
	  (t
	   (local-loop (cdr list) (cdr prefix*))))))
	       
(defun find-sublist (sublist list)
  "Finds any part of LIST is equal to SUBLIST and returns it, otherwise
returns NIL"
  (loop for remaining on list
     when (list-begins-with-p remaining sublist)
       return remaining))

(defun recursive-find-if (pred tree)
  (if (funcall pred tree)
      tree
      (recursive-find nil tree
		      :test (lambda (bullshit node)
			      (declare (ignore bullshit))
			      (funcall pred node)))))

(defun mapseq (function sequence)
  (map (type-of sequence) function sequence))

(defun recursive-find-sublist (sublist tree)
  (recursive-find-if
   (lambda (node)
     (aif (and (listp node)
	       (find-sublist sublist node))
	  (return-from recursive-find-sublist it)))
   tree))

(defun plist-replace (plist indicator new-value)
  (loop for (key value) on plist by #'cddr
     collect key
     if (eq key indicator)
       collect new-value
     else collect value))

;(defmacro pop-lazy (place)
;  "Like CL:POP, but for CLAZY lazy lists."
;  `(prog1 (lazy:head ,place)
;     (setf ,place (lazy:tail place))))

;(defmacro push-lazy (obj place)
;  "Like CL:PUSH, but for CLAZY lazy lists."
;  `(setf ,place
;	 (lazily (cons ,obj ,place))))

(defmacro pushover (obj* place &key (key '#'identity) (test '#'eql))
  "Pushes the OBJ* into the PLACE. If an \"identical\" object is already there, it is overwritten.
Whether something in the PLACE is considered \"identical\" can be controlled with the :TEST
and :KEY keywords."
  (alexandria:with-gensyms (tail obj)
    `(let ((,obj ,obj*))
       (loop for ,tail on ,place
	  when (funcall ,test
			(funcall ,key (car ,tail))
			(funcall ,key ,obj))
	  do (setf (car ,tail)
		   ,obj)
	    (return ,obj)
	  finally (push ,obj ,place)))))

(defun recursive-mapcar (function tree &optional traverse-results result)
  "Maps the FUNCTION onto every node in the TREE. Works correctly with
improper lists.

If TRAVERSE-RESULTS is non-NIL, then RECURSIVE-MAPCAR will traverse
the result of the FUNCTION even if it is not EQ to the original value.

"
  (let ((new-tree (funcall function tree)))
    (when traverse-results
      (setf tree new-tree))
    (cond ((not (eq new-tree tree))
	   (append (reverse result) new-tree))
	  ((null tree)
	   (reverse result))
	  ((atom tree)
	   (if result
	       (append (reverse result) tree)
	       tree))
	  ((consp tree)
	   (recursive-mapcar function (cdr tree)
		     traverse-results
		     (cons (recursive-mapcar function (car tree)
					     traverse-results) result))))))

(defun remove-binding (var form)
  "Removes any binding for VAR from all LET, LET*, LET-VALUES, or LET-VALUES* forms found
in the FORM."
  (recursive-mapcar
   (lambda (node)
     (destructuring-case node
       ((let bindings &rest body)
	:where (member let '(let let* let-values let-values*))
	`(,let ,(remove var bindings :key #'car)
	   ,@body))
       (otherwise node)))
   form t))

(defmacro with-letf-bindings ((temp-var place-var value-var temp-binding place-binding) &body body)
  `(destructuring-bind (,temp-var ,place-var) ,temp-binding
     (declare (ignore ,place-var))
     (destructuring-bind (,place-var ,value-var) ,place-binding
       ,@body)))
       

(defun insert-before (before-item new-item list &key (test #'eql) (key #'identity))
  (let ((result nil))
    (loop for item = (pop list)
	 while item
       do (if (funcall test (funcall key item) before-item)
	      (return-from insert-before
		(append (reverse result)
			(list* new-item item list)))
	      (push item result)))
    (nreverse result)))
    

(defmacro letf (place-bindings &body body)
  "Temporarily rebind places like you would special variables. Before control enters the BODY,
the PLACE-BINDINGS are altered using SETF, and after exiting the BODY, their values are restored
to their original values with SETF.

Similar to CL-LETF in Emacs."
  (let* ((temp-gensyms (loop repeat (length place-bindings)
			 collect (gensym)))
	 (temp-bindings 
	  (loop for temp-binding in temp-gensyms
	     for place in (mapcar #'car place-bindings)
	     collect `(,temp-binding ,place)))
	 (reversed-temp-bindings (reverse temp-bindings))
	 (reversed-place-bindings (reverse place-bindings))
	 (result-body nil))
      (setf result-body `(progn ,@body))
      (loop for (temp-var nil) in reversed-temp-bindings
	 for (place value) in reversed-place-bindings
	 do (setf result-body
		  `(progn
		     (setf ,place ,value)
		     (unwind-protect
			  ,result-body
		       (setf ,place ,temp-var)))))
      `(let ,temp-bindings
	 ,@(cdr result-body))))
	       
(defun relative-file-position (stream offset)
  (let* ((starting-position (file-position stream))
	 (ending-position (max (+ starting-position offset)
			       0)))
    (file-position stream ending-position)))

(defmacro with-file-position ((position stream) &body body)
  (let ((original-file-position (gensym)))
    `(let ((,original-file-position (file-position ,stream)))
       (unwind-protect
	    (progn
	      (file-position ,stream ,position)
	      ,@body)
	 (file-position ,stream ,original-file-position)))))

(defun determine-load-order (components &optional load-order)
  "Given the COMPONENTS (which is just the :components section of the ASDF
def above), determine the order in which to load things so that dependencies
are always satisfied."
  (cond ((null components)
	 (reverse load-order))
	(t
	 (destructuring-bind (&key file depends-on) (car components)
	   (if (every (lambda (dep)
			(member dep load-order :test #'string=))
		      depends-on)
	       (determine-load-order (cdr components)
				     (cons file load-order))
	       (determine-load-order (append (cdr components)
					     (list (car components)))
				     load-order))))))

(defmacro acond (&rest cases)
    (let ((result nil))
      (loop for (condition . body) in (reverse cases)
	 do (setf result
		  `(aif ,condition
			(progn ,@body)
			,result)))
      result))

(defmacro aif (test then &optional else)
    `(let ((it ,test))
       (if it ,then ,else)))

(defun read-all-forms (stream)
  (let ((eof (gensym)))
    (loop for next-form = (read stream nil eof)
       until (eq next-form eof)
	 collect next-form)))


(defun recursive-find-if (pred tree)
  (if (funcall pred tree)
      tree
      (recursive-find nil tree
		      :test (lambda (bullshit node)
			      (declare (ignore bullshit))
			      (funcall pred node)))))

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


(defun load-system-def (pathname)
  (with-open-file (in pathname)
    (recursive-find-if
     (lambda (form)
       (and (consp form)
	    (eq (car form) 'asdf:defsystem)))
     (read-all-forms in))))

(defun components (system-def)
  (getf system-def :components))

(defun dirname (pathname)
  (let ((directory (pathname-directory pathname))
	(name (pathname-name pathname)))
    (unless name
      (setf directory (butlast directory)))
  (let ((dirname (make-pathname :host (pathname-host pathname)
				:device (pathname-device pathname)
				:directory directory)))
    (if (equalp dirname #P"")
	nil
	dirname))))

(defun fake-asdf-load (asd-pathname)
  "Load a system from a .asd file without ASDF."
  (let ((system-def (load-system-def asd-pathname)))
    (ql:quickload (getf system-def :depends-on))
    (let ((*default-pathname-defaults* (or (dirname asd-pathname)
					   *default-pathname-defaults*)))
      (loop for file in (determine-load-order (components system-def))
	 do (load file)))))

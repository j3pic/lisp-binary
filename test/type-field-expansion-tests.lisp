(in-package :lisp-binary-test)

(defvar *stream* nil)
(defvar *previous-defs* nil)
(defvar *byte-count* nil)
(defvar *index* nil)
(defvar *bit-stream-id* nil)
(defvar *field* nil)

(defun call-form (binary-field-object stream which-form)
  (check-type binary-field-object lisp-binary::binary-field)
  (let ((*stream* stream))
    (eval (slot-value binary-field-object (intern (symbol-name which-form) :lisp-binary)))))

(defmacro with-read-stream (contents &body body)
  `(progn
     (with-open-binary-file (out "/tmp/foobar.bin"
				 :direction :output
				 :if-exists :supersede)
       (write-sequence ,contents out))
     (with-open-binary-file (*stream* "/tmp/foobar.bin"
				      :direction :input)
       ,@body)))

(defmacro with-write-stream-to-buffer (&body body)
  `(progn
     (with-open-binary-file (*stream* "/tmp/foobar.bin"
				      :direction :output
				      :if-exists :supersede)
       ,@body)
     (with-open-binary-file (in "/tmp/foobar.bin")
       (read-binary-type `(simple-array (unsigned-byte 8) (,(file-length in))) in))))

(defun expand-defbinary-field (default-value &rest keys)
  (apply #'lisp-binary::expand-defbinary-field (list* '*field* default-value
						      :stream-symbol '*stream*
						      :previous-defs-symbol '*previous-defs*
						      :byte-count-name '*byte-count*
						      :bind-index-to '*index*
						      keys)))


(unit-test 'base-pointer-test
    (let ((binary-field-object (expand-defbinary-field 0 :type 'base-pointer)))
      (assert-equal (slot-value binary-field-object 'lisp-binary::defstruct-field)
		    '(*field* 0 :type t))
      (let ((lisp-binary::*base-pointer-tags* nil))
	(with-read-stream (buffer 0 0 0)
	  (read-bytes 2 *stream*)
	  (multiple-value-bind (file-position bytes-read)
	      (call-form binary-field-object *stream* :read-form)
	    (assert= bytes-read 0)
	    (assert= file-position 2)
	    (assert-equal (assoc '*field* lisp-binary::*base-pointer-tags*)
			  '(*field* . 2)))))
      (with-write-stream-to-buffer
	(write-bytes (buffer 0 0) *stream*)
	(let ((*field* 0)
	      (lisp-binary::*base-pointer-tags* nil))
	  (call-form binary-field-object *stream* :write-form)
	  (assert= *field* 2)
	  (assert-equal (assoc '*field* lisp-binary::*base-pointer-tags*)
			'(*field* . 2))))))

(unit-test 'file-position-test
    (let ((binary-field-object (expand-defbinary-field 0 :type 'file-position)))
      (assert-equal (slot-value binary-field-object 'lisp-binary::defstruct-field)
		    '(*field* 0 :type integer))
      (with-read-stream (buffer 0 0 0)
	(read-bytes 2 *stream*)
	(multiple-value-bind (file-position bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert= bytes-read 0)
	  (assert= file-position 2)))
      (with-write-stream-to-buffer
	(write-bytes (buffer 0 0) *stream*)
	(assert= (call-form binary-field-object *stream* :write-form) 0)
	(assert= *field* 2))))


(unit-test 'custom-type-test
    (let ((binary-field-object (expand-defbinary-field nil :type `(custom :reader ,(lambda (stream)
											   (values (read-byte stream) 1))
									  :writer ,(lambda (obj stream)
											   (write-byte obj stream)
											   1)
									  :lisp-type (unsigned-byte 4)))))
      (assert-equal (slot-value binary-field-object 'lisp-binary::defstruct-field)
		    '(*field* nil :type (unsigned-byte 4)))
      (with-read-stream (buffer 15)
	(multiple-value-bind (byte bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert= byte 15)
	  (assert= bytes-read 1)))
      (assert-equalp
       (with-write-stream-to-buffer
	 (let ((*field* 15))
	   (assert= (call-form binary-field-object *stream* :write-form) 1)))
       (buffer 15))))
	 
(unit-test 'magic-number-test
    (let ((binary-field-object (expand-defbinary-field nil
						       :type '(magic :actual-type (unsigned-byte 8)
							       :value #x56))))
      (with-read-stream (buffer 15)
	(handler-case
	    (progn
	      (call-form binary-field-object *stream* :read-form)
	      (error "No BAD-MAGIC-VALUE error!"))
	  (bad-magic-value ()
	    t)))
      (with-read-stream (buffer #x56)
	(multiple-value-bind (magic bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert= magic #x56)
	  (assert= bytes-read 1)))
      (assert-equalp
       (with-write-stream-to-buffer
	 (let ((*field* nil))
	   (call-form binary-field-object *stream* :write-form)))
       (buffer #x56))))

(unit-test 'fixed-string-test
    (let ((binary-field-object (expand-defbinary-field "12345678"
						       :type '(fixed-string 8 :padding-character (code-char 1)))))
      (assert-equalp (slot-value binary-field-object 'lisp-binary::defstruct-field)
		     '(*field* "12345678" :type string))
      (with-read-stream (buffer 49 50 51 52 53 54 55 56 12 12 12 12 12)
	(multiple-value-bind (value bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert-equalp value "12345678")
	  (assert= bytes-read 8)))
      (assert-equalp
       (buffer 49 50 51 1 1 1 1 1)
       (with-write-stream-to-buffer
	 (assert= 8
		  (let ((*field* "123"))
		    (call-form binary-field-object *stream* :write-form)))))))

(unit-test 'counted-string-test
    (let ((binary-field-object (expand-defbinary-field "foobar"
						       :type '(counted-string 8))))
      (assert-equalp (slot-value binary-field-object 'lisp-binary::defstruct-field)
		     '(*field* "foobar" :type string))
      (with-read-stream (buffer 5 0 0 0 0 0 0 0 120 121 122 122 121)
	(multiple-value-bind (value bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert-equalp value "xyzzy")
	  (assert-equalp bytes-read 13)))
      (assert-equalp (buffer 5 0 0 0 0 0 0 0 120 121 122 122 121)
		     (with-write-stream-to-buffer
		       (let ((*field* "xyzzy"))
			 (call-form binary-field-object *stream* :write-form))))))
		     

(unit-test 'counted-buffer-test
    (let ((binary-field-object (expand-defbinary-field (buffer 1 2 3)
						       :type '(counted-buffer 1))))
      (assert-equalp (slot-value binary-field-object 'lisp-binary::defstruct-field)
		     `(*field* ,(buffer 1 2 3) :type (simple-array (unsigned-byte 8))))
      (with-read-stream (buffer 3 1 2 3)
	(multiple-value-bind (buf bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert-equalp buf (buffer 1 2 3))
	  (assert= bytes-read 4)))
      (assert-equalp (buffer 4 4 3 2 1)
		     (with-write-stream-to-buffer
		       (let ((*field* (buffer 4 3 2 1)))
			 (call-form binary-field-object *stream* :write-form))))))

(unit-test 'counted-array-test
    (let* ((array-of-strings (coerce (vector "one" "two" "three") 'simple-array))
	   (binary-field-object (expand-defbinary-field array-of-strings
							:type '(counted-array 1 (counted-string 1)
								:bind-index-to *index*))))
      (with-read-stream (buffer 3 3 111 110 101 3 116 119 111 5 116 104 114 101 101)
	(multiple-value-bind (value bytes-read)
	    (call-form binary-field-object *stream* :read-form)
	  (assert-equalp value array-of-strings)
	  (assert= bytes-read 15)))
      (assert-equalp (buffer 3 3 111 110 101 3 116 119 111 5 116 104 114 101 101)
		     (with-write-stream-to-buffer
		       (let ((*field* array-of-strings))
			 (call-form binary-field-object *stream* :write-form)))))
  
  (let* ((type `(counted-array 1
			       (custom :reader (lambda (stream)
						 (assert= *index* 0)
						 (values (read-byte stream) 1))
				       :writer (lambda (obj stream)
						 (write-byte obj stream)
						 1)
				       :lisp-type (unsigned-byte 8))
			       :bind-index-to *index*))
				       
			       
	 (binary-field-object (expand-defbinary-field (coerce #() 'simple-array)
						      :type type)))
    (assert-equalp (slot-value binary-field-object 'lisp-binary::defstruct-field)
		   `(*field* ,(coerce #() 'simple-array) :type (simple-array (unsigned-byte 8))))
    (with-read-stream (buffer 1 14)
      (multiple-value-bind (byte bytes-read)
	  (call-form binary-field-object *stream* :read-form)
	(assert-equalp byte (buffer 14))
	(assert= bytes-read 2)))))

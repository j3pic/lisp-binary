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
  (apply #'lisp-binary::expand-defbinary-field (list* 'test-struct '*field* default-value
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

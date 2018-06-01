(defpackage :lisp-binary-test
  (:use :common-lisp :lisp-binary))

(in-package :lisp-binary-test)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defbinary other-binary ()
  (x 1 :type (unsigned-byte 1))
  (y #x1d :type (unsigned-byte 7)))

(defbinary dynamic-other-binary (:byte-order :dynamic)
  (x 1 :type (unsigned-byte 1))
  (y #x1d :type (unsigned-byte 7)))

(defbinary be-other-binary (:byte-order :big-endian)
  (x 1 :type (unsigned-byte 1))
  (y #x1d :type (unsigned-byte 7)))

(defbinary with-terminated-buffer ()
  (c-string (buffer 12 13 14) :type (terminated-buffer 1 :terminator 0)))

(defun write-other-binary (ob stream)
  (write-binary ob stream))

(defun read-other-binary (stream)
  (read-binary 'other-binary stream))

(defbinary simple-binary (:export t
				  :byte-order :little-endian)
  (magic 38284 :type (magic :actual-type (unsigned-byte 16)
			    :value 38284))
  (size 0 :type (unsigned-byte 32))
  (oddball-value 1 :type (unsigned-byte 32)
		 :byte-order :big-endian)
  (b 0 :type (unsigned-byte 2))
  (g 0 :type (unsigned-byte 3))
  (r 0 :type (unsigned-byte 3))
  (name "" :type (counted-string 1 :external-format :utf8))
  (alias (buffer) :type (counted-buffer 4)
	 :byte-order :big-endian)
  (floating-point 0.0d0 :type double-float)
  (big-float 0 :type octuple-float)
  (odd-float 0.0d0 :type (double-float :byte-order :big-endian))
  (c-string (buffer) :type (terminated-buffer 1 :terminator 0))
  (nothing nil :type null) ;; Reads and writes nothing.
  (other-struct (make-other-binary :x 0 :y #x20)
		:type other-binary 
		:reader #'read-other-binary
		:writer #'write-other-binary)
  (struct-array (make-array 0 :element-type 'other-binary
			    :initial-element (make-other-binary :x 1 :y #x13))
		:type (counted-array 1 other-binary))
  (blah-type 0 :type (unsigned-byte 32))
  (blah nil :type (eval (case oddball-value
			  ((1) '(unsigned-byte 32))
			  ((2) '(counted-string 2)))))
  (an-array (make-array 0 :element-type '(unsigned-byte 32))
	    :type (simple-array (unsigned-byte 32) ((length c-string))))
  (body (make-array 0 :element-type '(unsigned-byte 8))
	:type (simple-array (unsigned-byte 8) (size))))

(defun call-test-round-trip (test-name write-thunk read-thunk)
  (format t ">>>>>>>>>>>>>>>>>>>>> ~a~%" test-name)
  (format t "Writing data....~%")
  (with-open-binary-file (*standard-output* "/tmp/foobar.bin"
					    :direction :output
					    :if-exists :supersede)
    (funcall write-thunk))
  (format t "Reading it back...~%")
  (with-open-binary-file (*standard-input* "/tmp/foobar.bin")
    (funcall read-thunk)))
					   

(defmacro test-round-trip (test-name read-form write-form)
  `(call-test-round-trip ,test-name (lambda () ,read-form)
			 (lambda () ,write-form)))

(defmacro assert-equal (x y)
  (let ((x* (gensym "X"))
	(y* (gensym "Y")))
    `(let ((,x* ,x)
	   (,y* ,y))
       (or (equalp ,x* ,y*)
       (error "~S is not EQUAL to ~S" ,x* ,y*)))))

(defun simple-test ()
  (declare (optimize (safety 3) (debug 3) (speed 0)))
  (let ((simple-binary (make-simple-binary :blah 34)))
    (test-round-trip "OVERALL ROUND TRIP TEST"
		     (write-binary simple-binary *standard-output*)
		     (let ((input-binary (read-binary 'simple-binary *standard-input*)))
		       (assert-equal simple-binary input-binary)))))

(defconstant +round-trip-test-parameters+
  `((octuple-precision-floating-point-test
     (0 1/2 1/3)
     'octuple-float)
    ((quad-precision-floating-point-test
      (0 1/2 1/3)
      'quadruple-float))
    ((double-precision-floating-point-test
      (0.0d0 0.5d0 0.33333333d0)))))

(defun octuple-precision-floating-point-test ()
  (test-round-trip "OCTUPLE PRECISION FLOATING POINT TEST"
		   (write-binary-type 0 'octuple-float *standard-output*)
		   (assert (= (read-binary-type 'octuple-float *standard-input*)
			      0))))

(defun octuple-precision-floating-point-test ()
  (test-round-trip "OCTUPLE PRECISION FLOATING POINT TEST"
		   (write-binary-type 0 'octuple-float *standard-output*)
		   (assert (= (read-binary-type 'octuple-float *standard-input*)
			      0))))


(defun other-binary-dynamic-test (&optional (*byte-order* :little-endian))
  (let ((other-binary (make-dynamic-other-binary :x 0 :y #x20)))
    (test-round-trip (format nil "BIT STREAMS - DYNAMIC BYTE ORDER ~a TEST" *byte-order*)
		     (write-binary other-binary *standard-output*)
		     (assert-equal (read-binary 'dynamic-other-binary *standard-input*)
				   other-binary))))

(defun other-binary-le-test ()
  (let ((other-binary (make-other-binary :x 0 :y #x20)))
    (test-round-trip (format nil "BIT STREAMS - STATIC BYTE ORDER LITTLE-ENDIAN TEST")
		     (write-binary other-binary *standard-output*)
		     (assert-equal (read-binary 'other-binary *standard-input*)
				   other-binary))))

(defun other-binary-be-test ()
  (let ((other-binary (make-be-other-binary :x 0 :y #x20)))
    (test-round-trip (format nil "BIT STREAMS - STATIC BYTE ORDER BIG-ENDIAN TEST")
		     (write-binary other-binary *standard-output*)
		     (assert-equal (read-binary 'be-other-binary *standard-input*)
				   other-binary))))

(defun other-binary-functions-test ()
  (let ((other-binary (make-other-binary :x 0 :y #x20)))
    (test-round-trip "OTHER-BINARY TEST"
		     (write-other-binary other-binary *standard-output*)
		     (assert-equal (read-other-binary *standard-input*)
				   other-binary))))

(defun terminated-buffer-test ()
  (let ((terminated-buffer (make-with-terminated-buffer)))
    (test-round-trip "TERMINATED-BUFFER TEST"
		     (write-binary terminated-buffer *standard-output*)
		     (read-binary 'with-terminated-buffer *standard-input*))))

(defun run-test ()
  (terminated-buffer-test)
  (other-binary-dynamic-test)
  (other-binary-dynamic-test :big-endian)
  (other-binary-le-test)
  (other-binary-be-test)
  (other-binary-functions-test)
  (octuple-precision-floating-point-test)
  (simple-test))

(defpackage :lisp-binary-test
  (:use :common-lisp :lisp-binary))

(in-package :lisp-binary-test)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defbinary other-binary ()
  (x 1 :type (unsigned-byte 1))
  (y #x1d :type (unsigned-byte 7)))

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

(defun run-test ()
  (declare (optimize (safety 3) (debug 3) (speed 0)))
  (let ((simple-binary (make-simple-binary :blah 34)))
    simple-binary))
					   

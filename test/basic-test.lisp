(defpackage :lisp-binary-test
  (:use :common-lisp :lisp-binary))

(in-package :lisp-binary-test)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defbinary other-binary ()
  (x 1 :type (unsigned-byte 1))
  (y #x1d :type (unsigned-byte 7)))

(defbinary simple-binary (:export t
				  :byte-order :little-endian)
  (magic 38284 :type (magic :actual-type (unsigned-byte 16)
			    :value 38284))
  (size 0 :type (unsigned-byte 32))
  (oddball-value 0 :type (unsigned-byte 32)
		 :byte-order :big-endian)
  (b 0 :type (unsigned-byte 2))
  (g 0 :type (unsigned-byte 3))
  (r 0 :type (unsigned-byte 3))
  (name "" :type (counted-string 1 :external-format :utf8))
  (alias #() :type (counted-buffer 4)
	 :byte-order :big-endian)
  (floating-point 0.0d0 :type double-float)
  (big-float 0 :type octuple-float)
  (odd-float 0 :type (double-float :byte-order :big-endian))
  (c-string "" :type (terminated-string 1 :terminator 0))
  (nothing nil :type null) ;; Reads and writes nothing.
  (other-struct nil :type other-binary 
		:reader #'read-other-binary
		:writer #'write-other-binary)
  (struct-array #() :type (counted-array 1 simple-binary))
  (blah-type 0 :type (unsigned-byte 32))
  (blah nil :type (eval (case oddball-value
			  ((1) '(unsigned-byte 32))
			  ((2) '(counted-string 2)))))
  (an-array #() :type (simple-array (unsigned-byte 32) ((length c-string))))
  (body #() :type (simple-array (unsigned-byte 8) (size))))

(in-package :lisp-binary-test)

(unit-test 'read/write-integer-test
  (let* ((integer 2948)
	 (le-buffer (flexi-streams:with-output-to-sequence (out)
		      (write-integer integer 4 out)))
	 (be-buffer (flexi-streams:with-output-to-sequence (out)
		      (write-integer integer 4 out :byte-order :big-endian)))
	 (signed-buffer (flexi-streams:with-output-to-sequence (out)
			  (write-integer (- integer) 2 out :byte-order :big-endian :signed t))))
    (assert-equalp le-buffer #(#x84 #xb 0 0))
    (assert-equalp be-buffer #(0 0 #xb #x84))
    (assert-equalp signed-buffer #(#xf4 #x7c))
    (assert-equalp integer (flexi-streams:with-input-from-sequence (in le-buffer)
			     (read-integer 4 in)))
    (assert-equalp integer (flexi-streams:with-input-from-sequence (in be-buffer)
			     (read-integer 4 in :byte-order :big-endian)))
    (assert-equalp (- integer) (flexi-streams:with-input-from-sequence (in signed-buffer)
				 (read-integer 2 in :byte-order :big-endian :signed t)))))

(defmethod read-bytes (n (stream sequence)
		       &rest options &key &allow-other-keys)
  (flexi-streams:with-input-from-sequence (in stream)
    (apply #'read-bytes (list* n in options))))


(unit-test 'pi-test
  (let* ((calculated-pi
	  (+ 3 (loop
		  for a from 2 by 2
		  for b from 3 by 2
		  for c from 4 by 2
		  for add = t then (not add)
		  repeat 2000
		  sum (if add
			  (/ 4.0 (* a b c))
			  (- (/ 4.0 (* a b c)))))))
	 (floats (loop for type in '(half-float single-float double-float quadruple-float octuple-float)
		    collect (cons type (flexi-streams:with-output-to-sequence (out)
					 (write-binary-type calculated-pi type out))))))
    (format t ">>>>>>>>>>>>> TESTING ENCODING AND DECODING OF ~a IN VARIOUS FLOATING-POINT PRECISIONS~%" (coerce calculated-pi 'double-float))
    (format t ">>>>>>>>> HALF-FLOAT~%")
    (assert= (read-binary-type 'half-float (cdr (assoc 'half-float floats)))
	     201/64)
    (format t ">>>>>>>>> SINGLE-FLOAT~%")
    (assert= (read-binary-type 'single-float (cdr (assoc 'single-float floats)))
	     3.1415927)
    (loop for type in '(double-float quadruple-float octuple-float)
	 do
	 (format t ">>>>>>>> ~a~%" type)
	 (assert= (read-binary-type type (cdr (assoc type floats))) calculated-pi))))
	 
(unit-test 'float-denormal-test
    (let* ((smallest-denormal (lisp-binary/float::make-smallest-denormal :single 'number))
	   (largest-denormal (lisp-binary/float::make-largest-denormal :single 'number))
	   (test-value (+ smallest-denormal
			  (/ (- largest-denormal
				smallest-denormal)
			     2)))
	   (test-buffer (flexi-streams:with-output-to-sequence
			    (out :element-type '(unsigned-byte 8))
			  (write-binary-type test-value 'single-float out :byte-order :big-endian))))
      (assert-equalp test-buffer (buffer 0 #x40 0 0))
      (assert= (flexi-streams:with-input-from-sequence
		   (in test-buffer)
		 (read-binary-type 'single-float in :byte-order :big-endian))
	       test-value)))

(unit-test 'string-tests
  (let ((ascii-string "The quick brown fox jumps over the lazy dog."))
    (let ((counted-string-buffer (flexi-streams:with-output-to-sequence (out)
				   (write-binary-type ascii-string '(counted-string 1) out)))
	  (terminated-string-buffer (flexi-streams:with-output-to-sequence (out)
				      (write-binary-type ascii-string '(terminated-string 1 :terminator 0) out))))
      (assert= (aref counted-string-buffer 0) (length ascii-string))
      (assert= (length counted-string-buffer) (1+ (length ascii-string)))
      (assert-equalp (flexi-streams:with-input-from-sequence (in counted-string-buffer)
		       (read-binary-type '(counted-string 1) in))
		     ascii-string)
      (assert= (aref terminated-string-buffer (1- (length terminated-string-buffer))) 0)
      (assert= (length terminated-string-buffer) (1+ (length ascii-string)))
      (assert-equalp (flexi-streams:with-input-from-sequence (in terminated-string-buffer)
		       (read-binary-type '(terminated-string 1 :terminator 0) in))
		     ascii-string))))
      
(unit-test 'ones-complement-test
    (let ((buffer (flexi-streams:with-output-to-sequence (out)
		    (write-integer #xf0 1 out :byte-order :big-endian))))
      (loop for (complement value)
	 in '((:ones-complement -15)
	      (:twos-complement -16))
	   do
	   (assert= (flexi-streams:with-input-from-sequence (in buffer)
		      (read-binary-type `(signed-byte 8 :signed-representation ,complement)
					in :byte-order :big-endian))
		    value)
	   (assert-equalp (flexi-streams:with-output-to-sequence (out)
			    (write-binary-type value `(signed-byte 8 :signed-representation ,complement)
					       out :byte-order :big-endian))
			  buffer))))
      		   
(unit-test 'short-input-test
    (loop for wrapper in (list 'simple-bit-stream:wrap-in-bit-stream 'identity)
	 do
	 (handler-case 
	     (flexi-streams:with-input-from-sequence (in #(1 1))
	       (read-binary-type '(unsigned-byte 32) in)
	       (error "Failed to short input test with wrapper ~a." wrapper))
	   (end-of-file ()
	     :pass))))

(unit-test 'short-bit-input-test
    (flexi-streams:with-input-from-sequence (in #(255))
      (with-wrapped-in-bit-stream (in-bitstream in)
	(read-bytes 1/2 in-bitstream)
	(handler-case
	    (progn
	      (read-bytes 1 in-bitstream)
	      (error "Failed short-bit-input test."))
	  (end-of-file ()
	    :pass)))))
	 

(unit-test 'read-sequence-bit-stream-test
    (flexi-streams:with-input-from-sequence (in #(155))
      (with-wrapped-in-bit-stream (in-bitstream in)
	(read-bytes 1/2 in-bitstream)
	(let ((seq (make-array 1)))
	  (assert-equalp
	   (read-sequence seq in-bitstream)
	   0)))))
	
      
(unit-test 'eval-no-case-test
    (with-read-stream #(12 12 12 12)
      (assert-equalp
       (read-binary-type `(eval '(unsigned-byte 32)) *stream*)
       #x0c0c0c0c))
  (assert-equalp
   (with-write-stream-to-buffer
     (write-binary-type #x0c0c0c0c `(eval '(unsigned-byte 32)) *stream*))
   #(12 12 12 12)))

(unit-test 'eval-with-case-test
    (with-read-stream #(12 12 12 12)
      (assert-equalp
       (read-binary-type `(eval (case t (otherwise '(unsigned-byte 32)))) *stream*)
       #x0c0c0c0c))
  (assert-equalp
   (with-write-stream-to-buffer
     (write-binary-type #x0c0c0c0c `(eval (case t (otherwise '(unsigned-byte 32)))) *stream*))
   #(12 12 12 12)))

(define-enum simple-enum 1 (:byte-order :big-endian)
   zero one two)

(unit-test 'simple-enum-test
    (with-read-stream #(0 1 2)
      (assert-equalp (read-binary-type 'simple-enum *stream*) 'zero)
      (assert-equalp (read-binary-type 'simple-enum *stream*) 'one)
      (assert-equalp (read-binary-type 'simple-enum *stream*) 'two))
  (assert-equalp
      (with-write-stream-to-buffer
	(loop for symbol in '(zero one two)
	   do (write-binary-type symbol 'simple-enum *stream*)))
      #(0 1 2)))

(define-enum custom-reader-enum 1 (:reader (lambda (size stream &rest boo-hoo)
					     (declare (ignore boo-hoo size stream))
					     (values 123 1))
				   :writer (lambda (n size stream &rest boo-hoo)
					     (declare (ignore boo-hoo size n))
					     (write-integer 255 1 stream)))
	     (:the-value 123))

(unit-test 'custom-reader-enum-test
  (with-read-stream #(0 1 2 3)
    (multiple-value-bind (value bytes-read)
	(read-binary-type 'custom-reader-enum *stream*)
      (assert-equalp value :the-value)
      (assert= bytes-read 1)))
  (assert-equalp
      (with-write-stream-to-buffer
	(assert= (write-binary-type :whatever 'custom-reader-enum *stream*)
	    1))
      #(255)))

(define-enum boolean-enum 1 ()
  (nil 0)
  (t 1))

(unit-test 'boolean-enum-test
    (with-read-stream #(0 1)
      (assert-equalp
	  (loop repeat 2 collect (read-binary-type 'boolean-enum *stream*))
	  '(nil t)))
  (assert-equalp
      (with-write-stream-to-buffer
	(loop for val in '(t nil)
	   do (write-binary-type val 'boolean-enum *stream*)))
      #(1 0)))

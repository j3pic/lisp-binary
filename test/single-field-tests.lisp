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
      
      
				   

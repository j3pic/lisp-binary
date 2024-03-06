(in-package :lisp-binary-test)

(defbinary object-with-pointers ()
  (pointer-1 nil :type (pointer :pointer-type (unsigned-byte 16)
                                :data-type  (terminated-string 1)
                                :base-pointer-name foo-base
                                :region-tag foo-region))
  (pointer-2 0   :type (pointer :pointer-type (unsigned-byte 16)
                                :data-type quadruple-float
                                :base-pointer-name foo-base
                                :region-tag foo-region)))

(defbinary object-with-base-pointer ()
  (foo-base 0 :type base-pointer)
  (bar nil :type object-with-pointers)
  ;; POINTER-1 and POINTER-2 will point to this:
  (foo-region nil :type (region-tag :base-pointer-name foo-base)))


(with-local-pointer-resolving-context
  (let ((input (with-open-binary-file (in "foo.bin")
                 (read-binary 'foo in))))
    (with-open-binary-file (out "bar.bin"
                                :direction :io)
      (write-binary input stream))))

(unit-test 'pointers-and-tags
    (let* ((object-with-pointers
	     (make-object-with-pointers
	      :pointer-1 "This is a string!"
	      :pointer-2 22/7))
	   (object-with-base-pointer
	     (make-object-with-base-pointer
	      :bar object-with-pointers)))
      (test-round-trip "POINTER AND TAG TEST"
	(write-binary object-with-base-pointer *standard-output*)
       (let ((round-trip-object (read-binary 'object-with-base-pointer *standard-input*)))
	 (assert
	  (<
	   (abs (- (slot-value
	            (slot-value round-trip-object 'bar)
		    'pointer-2)
		   (slot-value
	            (slot-value object-with-base-pointer 'bar)
		    'pointer-2)))
	   1/1000000))
	 (setf (slot-value
		(slot-value round-trip-object 'bar)
		'pointer-2) 22/7)
	 (assert-equalp object-with-base-pointer
			round-trip-object))
       :out-direction :io)))

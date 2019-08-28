(defpackage :lisp-binary/integer
  (:use :common-lisp :lisp-binary-utils)
  (:export :get-lsb-byte :encode-lsb :decode-lsb :encode-msb :decode-msb
	   :signed->unsigned :unsigned->signed :unsigned->signed/bits
	   :signed->unsigned/bits 
	   :read-integer :write-integer :read-bytes :write-bytes :pop-bits
	   :split-bit-field :join-field-bits :pop-bits/le
	   :push-bits :push-bits/le :bit-stream))

(in-package :lisp-binary/integer)

;; (declaim (optimize (debug 0) (speed 3)))


(defun get-lsb-byte (number byte)
  (declare (type integer number)
	   (type (signed-byte 32) byte))
  (logand #xff (ash number (* byte -8))))

(defun encode-lsb (number bytes)
  (declare (type integer number))
  (let ((result (make-array (list bytes) :element-type '(unsigned-byte 8))))
   (loop for x from 0 below bytes
      do (setf (aref result x) (get-lsb-byte number x)))
   result))

(declaim (inline encode-lsb))

(defun decode-lsb (bytes)
;;  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((result 0))
    (declare (type integer result))
    (loop for b across bytes
	  for ix from 0 do
	 (setf result (logior result (ash b (* ix 8)))))
    result))

(declaim (inline decode-lsb))

(defun  decode-msb (bytes)
  (decode-lsb  (reverse bytes)))

(declaim (inline decode-msb))

(defun encode-msb (number bytes)
  (declare (type integer number))
  (reverse (encode-lsb number bytes)))
(declaim (inline encode-msb))

(defun signed->unsigned/bits (n bits)
  (let ((negative-offset (expt 2 bits)))
    (if (< n 0)
	(the integer (+ n negative-offset))
	n)))

(defun signed->unsigned (n bytes &optional (type :twos-complement))
  (let ((n (ecase type
	     (:twos-complement n)
	     (:ones-complement (ones-complement->twos-complement n)))))
    (signed->unsigned/bits n (* 8 bytes))))

(defun twos-complement->ones-complement (n bits)
  "Given a number that has been decoded as two's complement,
correct it to what its value should be if the original bits
were a one's complement representation."
  (cond ((>= n 0)
	 n)
	((= n (1- (expt 2 bits)))
	 0)
	(t
	 (1+ n))))

(defun ones-complement->twos-complement (n)
  "Given a number that has been decoded as one's complement,
correct it to what its value should be if the original bits
were a two's complement representation. This function doesn't
need the number of bits because all ones in one's complement
represents 'negative zero', a value that can't be represented
in Common Lisp integers."
  (if (>= n 0)
      n
      (1- n)))
	

(defun unsigned->signed/bits (n bits)
  (let* ((negative-offset (expt 2 bits))
	 (max (- (/ negative-offset 2) 1)))
    (if (> n max)
	(- n negative-offset)
	n)))

(defun unsigned->signed (n bytes &key (type :twos-complement))
  (let ((twos-complement (unsigned->signed/bits n (* 8 bytes))))
    (ecase type
      (:twos-complement twos-complement)
      (:ones-complement (twos-complement->ones-complement twos-complement (* 8 bytes))))))

(defgeneric write-bytes (buffer stream &optional bytes)
  (:documentation "Write BYTES bytes of the BUFFER into the STREAM. If
BYTES is not provided, then the whole BUFFER is written.

For some types of stream, it is legal to use a fractional number for BYTES. In that case,
the whole bytes are written first, and then the leftover bits. The leftover bits must be given
their own byte at the end of the BUFFER. WRITE-BYTES assumes that all bytes are 8 bits long,
so to write 4 bits, you would give 1/2 as the value of BYTES.

NOTE: If you're using this with a bit-stream created with WRAP-IN-BIT-STREAM, the
:BYTE-ORDER given to that function should match the one given to this function."))

(defmethod write-bytes (buffer stream &optional bytes)
  (setf bytes (or bytes (length buffer)))
  (check-type bytes integer)
  (write-sequence buffer stream :end bytes)
  (length buffer))

(defgeneric read-bytes (n stream &key element-type)
  (:documentation "Read N bytes of type ELEMENT-TYPE from STREAM and return them in a newly-allocated array.

Returns two values: The array containing the bytes, and the number of bytes read.

For some types of stream, it is legal to use a fractional number for N. In that case,
the whole bytes are read first, and then the leftover bits. The leftover bits are given
their own byte at the end of the returned array. The second return value (# of bytes read)
will also be fractional in this case. The fractional part can be used to calculate
how many bits the partial byte represents.

If you're using 8-bit bytes and want to read 11 bits (a whole byte plus three bits), give
11/8 as the value of N.


NOTE: If you're using this with a bit-stream created with WRAP-IN-BIT-STREAM, the :BYTE-ORDER given
to that function should match the one given to this function."))

(defmethod read-bytes (n stream &key (element-type '(unsigned-byte 8)))
  (let ((result (make-array n :element-type element-type)))
    (values result (read-sequence result stream))))

(defun write-integer (number size stream &key (byte-order :little-endian)
					   (signed-representation :twos-complement)
					   signed)
  (when signed
    (setf number (signed->unsigned number size signed-representation)))
  (cond ((integerp size)
	 (write-bytes (ecase byte-order
			((:big-endian) (encode-msb number size))
			((:little-endian) (encode-lsb number size))
			(otherwise (error "Invalid byte order: ~a" byte-order)))
		      stream))
	(t (let* ((whole-bytes (floor size))
		  (too-big (funcall ;; TOO-BIG encodes the integer to be written with one more
			    ;; byte for the fractional part.
			    (ecase byte-order
			      (:big-endian #'encode-msb)
			      (:little-endian #'encode-lsb))
			    number (1+ whole-bytes))))
	     (write-bytes too-big stream size)))))

(defmacro tlabels (labels &body body)
  `(labels ,(loop for (name args . bod) in labels
	       for gs = (gensym)
	       collect `(,name ,args
			       (let ((,gs (progn ,@bod)))
				 (format t "~s returned ~s~%"
					 (list ',name ,@args)
					 ,gs)
				 ,gs)))
     ,@body))

(defmacro tif (expr if-t if-nil)
  (let ((expr* (gensym))
	(res (gensym)))
    `(let ((,expr* ,expr))
       (if ,expr*
	   (let ((,res ,if-t))
	     (format t "IF condition: ~s~%Test result: TRUE~%Value: ~S~%"
		     ,expr* ,res)
	     ,res)
	   (let ((,res ,if-nil))
	     (format t "IF condition: ~s~%Test result: FALSE~%Value: ~S~%"
		     ,expr* ,res)
	     ,res)))))

(defun ash* (&rest integers)
  (apply #'ash integers))

(defun logior* (&rest args)
  (apply #'logior args))

(defun read-integer (length stream &key (byte-order :little-endian)
				     signed
				     (signed-representation :twos-complement))
  "Reads an integer of LENGTH bytes from the STREAM in the specified BYTE-ORDER.

If SIGNED is non-NIL, the number is interpreted as being in two's complement format.

If the STREAM is a BIT-STREAM, then the LENGTH doesn't have to be an integer."
  
  (multiple-value-bind (bytes bytes-read) (read-bytes length stream)
    (let ((bytes (if (integerp bytes-read)
		     bytes
		     (subseq bytes 0 (1- (length bytes)))))
	  (partial-byte (unless (integerp bytes-read)
			  (aref bytes (1- (length bytes)))))
	  (extra-bits (multiple-value-bind (whole frac) (floor bytes-read)
			(declare (ignore whole))
			(* frac 8))))
      (labels ((add-extra-bits (int)
		 (if partial-byte
		     (ecase byte-order
		       (:big-endian
			(logior
			 ;; Note: SBCL 1.4.13.debian claims that both
			 ;; calls to ASH are unreachable, and prints
			 ;; the message "deleting unreachable code"
			 ;; for them. Yet, I have confirmed through
			 ;; extensive tracing that the code is
			 ;; indeed executed, and removing it changes
			 ;; the return value of READ-INTEGER in
			 ;; the relevant case (where BIT-STREAMs are
			 ;; involved)
			 (ash int extra-bits)
			 partial-byte))
		       (:little-endian
			(logior
			 (ash partial-byte (* (floor length) 8))
			 int)))
		     int))		       
	       (decode-msb* (bytes)
		 (add-extra-bits
		  (decode-msb bytes)))
	       (decode-lsb* (bytes)
		 (add-extra-bits
		  (decode-lsb bytes))))
	(declare (inline add-extra-bits decode-msb* decode-lsb*))
	(values
	 (let ((result (case byte-order
			 ((:big-endian) (decode-msb* bytes))
			 ((:little-endian) (decode-lsb* bytes))
			 (otherwise (error "Invalid byte order: ~a" byte-order)))))
	   (if signed
	       (unsigned->signed result length :type signed-representation)
	       result))
	 bytes-read)))))


(defun split-bit-field (integer field-bits &optional field-signedness)
  "Given the INTEGER, split it up as a bit field. The sizes and number of elements are given
by the FIELD-BITS parameter. If FIELD-SIGNEDNESS is specified, then it must be a list
that contains NIL for each element that will be interpreted as an unsigned integer,
and non-NIL for signed integers.

Example:

    CL-USER> (split-bit-field #xffaaff '(8 8 8))
    255
    170
    255
    CL-USER>

Better performance could be acheived if INTEGER could be a FIXNUM, but it can't.
"
  (declare (type integer integer)
	   (type list field-bits))
  (setf field-signedness (reverse field-signedness))
  (apply #'values
	 (reverse (loop for bits of-type (unsigned-byte 29) in (reverse field-bits) 
		     for mask = (- (ash 1 bits)
				   1)
		       for signed = (pop field-signedness)
		     collect (let ((unsigned-result (logand mask integer)))
			       (if signed
				   (unsigned->signed/bits unsigned-result bits)
				   unsigned-result))
		       do (setf integer (ash integer (- bits)))))))

(defun join-field-bits (field-bits field-signedness field-values)
  (let ((result 0))
    (loop for (bits next-bits) 
       on field-bits
       for value in field-values
       for signed = (pop field-signedness)
       do
	  (setf result
		(logior result
			(if signed
			    (signed->unsigned value (/ bits 8))
			    value)))
	 (when next-bits
	   (setf result (ash result next-bits))))
    result))

(defmacro push-bits (n integer-size integer-place)
  "Pushes N onto the front of INTEGER-PLACE,
the 'front' being defined as the MOST significant bits. The
INTEGER-SIZE specifies how many bits are already in the
INTEGER-PLACE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion integer-place)
    (let ((old-value (gensym))
	  (integer-size-temp (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,old-value ,getter)
		(,integer-size-temp ,integer-size)
		(,(car newval) (+ ,old-value (ash ,n ,integer-size-temp)))
		,@(cdr newval))
         ,setter))))

(defmacro push-bits/le (n n-bits integer-place)
  "Pushes N-BITS bits from N onto the front of INTEGER-PLACE,
the 'front' being defined as the LEAST significant bits. The
INTEGER-SIZE specifies how many bits are already in the
INTEGER-PLACE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion integer-place)
    (let ((old-value (gensym))
	  (n-ones (gensym))
	  (n-bits-temp (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,old-value ,getter)
		(,n-bits-temp ,n-bits)
		(,n-ones (1- (ash 1 ,n-bits-temp)))
		(,(car newval) (+ (ash ,old-value ,n-bits-temp) (logand ,n ,n-ones)))
		,@(cdr newval))
         ,setter))))
	       

(defmacro pop-bits (n-bits integer-size integer-place)
  "Pops the N most significant bits off the front of the INTEGER-PLACE and returns it.
INTEGER-SIZE is the number of unpopped bits in the integer."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion integer-place)
    (let ((old-value (gensym "OLD-VALUE-"))
	  (n-ones (gensym "N-ONES-"))
	  (integer-size-temp (gensym "INTEGER-SIZE-"))
	  (n-bits-temp (gensym "N-BITS-"))
	  (selected-bits (gensym "SELECTED-BITS-")))
      `(let* (,@(mapcar #'list dummies vals)
              (,old-value ,getter)
		(,n-bits-temp ,n-bits)
		(,integer-size-temp ,integer-size)
		(,n-ones (1- (ash 1 ,n-bits-temp)))
		(,selected-bits (logand ,old-value (ash ,n-ones (- ,integer-size-temp ,n-bits-temp))))
		(,(car newval) (- ,old-value ,selected-bits))
		,@(cdr newval))
         ,setter
         (ash ,selected-bits
	      (- (- ,integer-size-temp ,n-bits-temp)))))))

(defmacro pop-bits/le (n-bits integer-place)
  "Pops the N LEAST significant bits off the front of the INTEGER-PLACE and returns it.
INTEGER-SIZE is the number of bits in the integer."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion integer-place)
    (let ((old-value (gensym))
	  (n-ones (gensym))
	  (n-bits-temp (gensym))
	  (selected-bits (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,old-value ,getter)
		(,n-bits-temp ,n-bits)
		(,n-ones (1- (ash 1 ,n-bits-temp)))
		(,selected-bits (logand ,old-value ,n-ones))
		(,(car newval) (ash ,old-value (- ,n-bits-temp)))
		,@(cdr newval))
         ,setter
         ,selected-bits))))

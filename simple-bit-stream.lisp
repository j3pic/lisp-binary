(defpackage :simple-bit-stream
  (:use :common-lisp :trivial-gray-streams :lisp-binary/integer)
  (:export :wrap-in-bit-stream :with-wrapped-in-bit-stream :bit-stream :read-bits
	   :write-bits :read-bytes-with-partial :byte-aligned-p))

(in-package :simple-bit-stream)

(defclass bit-stream (fundamental-binary-stream)
  ((element-bits :type fixnum :initform 8 :initarg :element-bits)
   (real-stream :type stream :initarg :real-stream)
   (last-byte :type (or unsigned-byte null) :initform 0)
   (last-op :type symbol :initform nil)
   (bits-left :type integer :initform 0)
   (byte-order :type keyword :initarg :byte-order :initform :little-endian)))
       		

(defun byte-aligned-p (bit-stream)
  (= (slot-value bit-stream 'bits-left) 0))

(defgeneric wrap-in-bit-stream (object &key byte-order)
  (:documentation "Creates a BIT-STREAM that can read one bit at a time from the OBJECT. The BIT-STREAM
can be discarded if BYTE-ALIGNED-P returns T."))

(defmethod wrap-in-bit-stream ((object stream) &key (byte-order :little-endian))
  (make-instance 'bit-stream :real-stream object :byte-order byte-order))

(defmacro with-wrapped-in-bit-stream ((var non-bitstream &key (byte-order :little-endian)
					   close-when-done) &body body)
  `(let ((,var (wrap-in-bit-stream ,non-bitstream :byte-order ,byte-order)))
     (unwind-protect
	  (progn
	    ,@body)
       (finish-output ,var)
       ,@(if close-when-done
	     `((if ,close-when-done
		   (close ,var)))))))
       

(declaim (inline init-read init-write reset-op))

(defun reset-op (stream op)
  (setf (slot-value stream 'last-op) op)
  (setf (slot-value stream 'last-byte) 0)
  (setf (slot-value stream 'bits-left) 0))
	 
(defun init-read (stream)
  (unless (eq (slot-value stream 'last-op) :read)
    (reset-op stream :read)))

(defun init-write (stream)
  (unless (eq (slot-value stream 'last-op) :write)
    (reset-op stream :write)))

(defun read-partial-byte/big-endian (bits stream)
    (cond
      ((= (slot-value stream 'bits-left) 0)
       (setf (slot-value stream 'last-byte)
	     (read-byte (slot-value stream 'real-stream)))
       (setf (slot-value stream 'bits-left)
	     (slot-value stream 'element-bits))
       (read-partial-byte/big-endian bits stream))
      ((>= (slot-value stream 'bits-left) bits)
	   (prog1
	       (pop-bits bits (slot-value stream 'bits-left)
			 (slot-value stream 'last-byte))
	     (decf (slot-value stream 'bits-left) bits)))
      ((< (slot-value stream 'bits-left) bits)
       (let* ((bits-left (slot-value stream 'bits-left))
	      (remaining-bits (pop-bits (slot-value stream 'bits-left)
					bits-left
					(slot-value stream 'last-byte))))
	 (setf (slot-value stream 'bits-left) 0)
	 (logior
	  (ash remaining-bits (- bits bits-left))
	  (read-partial-byte/big-endian (- bits bits-left) stream))))))

(defun read-partial-byte/little-endian (bits stream)
    (cond
      ((= (slot-value stream 'bits-left) 0)
       (setf (slot-value stream 'last-byte)
	     (read-byte (slot-value stream 'real-stream)))
       (setf (slot-value stream 'bits-left)
	     (slot-value stream 'element-bits))
       (read-partial-byte/little-endian bits stream))
      ((>= (slot-value stream 'bits-left) bits)
	   (prog1
	       (pop-bits/le bits (slot-value stream 'last-byte))
	     (decf (slot-value stream 'bits-left) bits)))
      ((< (slot-value stream 'bits-left) bits)
       (let* ((bits-left (slot-value stream 'bits-left))
	      (remaining-bits (pop-bits/le (slot-value stream 'bits-left)					
					   (slot-value stream 'last-byte))))
	 (setf (slot-value stream 'bits-left) 0)
	 (logior
	  remaining-bits
	  (ash (read-partial-byte/little-endian (- bits remaining-bits) stream)
	       bits-left))))
      (t (error "BUG: This should never happen!"))))

(defmethod stream-finish-output ((stream bit-stream))
  (unless (or (not (eq (slot-value stream 'last-op) :write))
	      (= (slot-value stream 'bits-left) 0))
    (write-byte (ecase (slot-value stream 'byte-order)
		  (:little-endian (slot-value stream 'last-byte))
		  (:big-endian (ash (slot-value stream 'last-byte)
				    (- 8 (slot-value stream 'bits-left)))))
		(slot-value stream 'real-stream))
    (finish-output (slot-value stream 'real-stream))))

(defmethod stream-force-output ((stream bit-stream))
  (stream-finish-output stream))

(defmethod close ((stream bit-stream) &key abort)
  (declare (ignore abort))
  (stream-finish-output stream)
  (close (slot-value stream 'real-stream)))

(defmethod stream-read-byte ((stream bit-stream))
  (init-read stream)
  (cond ((= (slot-value stream 'bits-left) 0)
	 (read-byte stream (slot-value stream 'real-stream)))
	((= (slot-value stream 'bits-left)
	    (slot-value stream 'element-bits))
	 (prog1
	     (slot-value stream 'last-byte)
	   (setf (slot-value stream 'last-byte) 0
		 (slot-value stream 'bits-left) 0)))
	((< (slot-value stream 'bits-left)
	    (slot-value stream 'element-bits))
	 (ecase (slot-value stream 'byte-order)
	   (:little-endian
	    (let ((bits-left (slot-value stream 'bits-left))
		  (last-byte (slot-value stream 'last-byte))
		  (next-bits nil))
	      (setf (slot-value stream 'bits-left) 0)
	      (setf (slot-value stream 'last-byte) 0)
	      (setf next-bits (read-partial-byte/little-endian (- (slot-value stream 'element-bits)
								  bits-left)
							       stream))
	      (logior last-byte
		      (ash next-bits bits-left))))
	   (:big-endian
	    (logior (ash (slot-value stream 'last-byte)
			 (- (slot-value stream 'element-bits)
			    (slot-value stream 'bits-left)))
		    (let ((bits-to-read (- (slot-value stream 'element-bits)
					   (slot-value stream 'bits-left))))
		      (setf (slot-value stream 'last-byte) 0
			    (slot-value stream 'bits-left) 0)
		      (read-partial-byte/big-endian bits-to-read stream))))))
	((> (slot-value stream 'bits-left)
	    (slot-value stream 'element-bits))
	 (ecase (slot-value stream 'byte-order)
	   (:little-endian
	    (prog1 (logand (1- (expt 2 (slot-value stream 'element-bits)))
			   (slot-value stream 'last-byte))
	      (decf (slot-value stream 'bits-left)
		    (slot-value stream 'element-bits))
	      (setf (slot-value stream 'last-byte)
		    (ash (slot-value stream 'last-byte)
			 (- (slot-value stream 'element-bits))))))
	   (:big-endian
	    (error "Not implemented!"))))))

(defmethod stream-write-byte ((stream bit-stream) integer)
  (init-write stream)
  (cond ((= (slot-value stream 'bits-left) 0)
	 (write-byte integer (slot-value stream 'real-stream)))
	(t (let ((total-bits-left (+ (slot-value stream 'element-bits)
				     (slot-value stream 'bits-left))))
	     (multiple-value-bind (byte-to-write new-last-byte)
		 (ecase (slot-value stream 'byte-order)
		   (:little-endian
		    (push-bits integer (slot-value stream 'bits-left)
			       (slot-value stream 'last-byte))
		    (values (pop-bits/le (slot-value stream 'element-bits)
					 (slot-value stream 'last-byte))
			    (slot-value stream 'last-byte)))
		   (:big-endian
		    (push-bits/le integer (slot-value stream 'element-bits)
				  (slot-value stream 'last-byte))
		    (values (pop-bits (slot-value stream 'element-bits)
				      total-bits-left
				      (slot-value stream 'last-byte))
			    (slot-value stream 'last-byte))))
	       (setf (slot-value stream 'last-byte) new-last-byte)
	       (write-byte byte-to-write (slot-value stream 'real-stream)))))))

(defun %stream-write-sequence (stream sequence start end)
  (unless (>= end start)
    (return-from %stream-write-sequence sequence))
  (cond ((and (equal (slot-value stream 'bits-left) 0)
	      (streamp (slot-value stream 'real-stream)))
	 (write-sequence sequence (slot-value stream 'real-stream) :start start :end end))
	(t (loop for ix from start to end
	      do (write-byte (elt sequence ix) stream))
	   sequence)))

#-sbcl
(defmethod stream-write-sequence ((stream bit-stream) sequence start end &key &allow-other-keys)
  (%stream-write-sequence stream sequence (or start 0) (or end (1- (length sequence)))))

#+ccl
(defmethod ccl:stream-write-vector ((stream bit-stream) vector start end)
  (%stream-write-sequence stream vector (or start 0) (or end (1- (length vector)))))

#+sbcl
(defmethod sb-gray:stream-write-sequence ((stream bit-stream) seq &optional start end)
  (%stream-write-sequence stream seq (or start 0) (or end (length seq))))

(defun %stream-read-sequence (stream sequence start end)
  (declare (optimize (speed 0) (debug 3)))
  (unless (> end start)
    (return-from %stream-read-sequence sequence))
  (init-read stream)
  (cond ((and (equal (slot-value stream 'bits-left) 0)
	      (streamp (slot-value stream 'real-stream)))
	 (read-sequence sequence (slot-value stream 'real-stream) :start start :end end))
	(t
	 (loop for ix from start below end
	    do (setf (elt sequence ix) (read-byte stream))
	      count t))))

#-sbcl
(defmethod #+ccl ccl:stream-read-vector #-ccl stream-read-sequence  ((stream bit-stream) sequence start end
                                                                     #-ccl &key #-ccl &allow-other-keys)
  (%stream-read-sequence stream sequence start end))

#+sbcl
(defmethod sb-gray:stream-read-sequence ((stream bit-stream) (sequence array) &optional start end)
  (%stream-read-sequence stream sequence (or start 0) (or end (length sequence))))

(defmacro read-bytes-with-partial/macro (stream* bits byte-order &key adjustable)
  (alexandria:with-gensyms (whole-bytes remaining-bits element-bits buffer
					stream)
    `(let* ((,stream ,stream*)
	    (,element-bits (slot-value ,stream 'element-bits)))
       (multiple-value-bind (,whole-bytes ,remaining-bits)
	   (floor ,bits ,element-bits)
	 (let ((,buffer (make-array ,whole-bytes
				    :element-type (list 'unsigned-byte ,element-bits)
				    :adjustable ,adjustable
				    :fill-pointer ,adjustable)))
	   (when (> ,whole-bytes 0)
	     (read-sequence ,buffer ,stream))
	   (values ,buffer ,(ecase byte-order
				   (:little-endian
				    `(read-partial-byte/little-endian ,remaining-bits ,stream))
				   (:big-endian
				    `(read-partial-byte/big-endian ,remaining-bits, stream)))
		   ,remaining-bits))))))

(defun read-bytes-with-partial (stream bits)
  "Reads BITS bits from the STREAM, where BITS is expected to be more than a byte's worth
of bits. Returns three values:

   1. A buffer containing as many whole bytes as possible. This buffer
      is always read first, regardless of whether the bitstream is byte-aligned.
   2. The partial byte.
   3. The number of bits that were read for the partial byte.

The byte order is determined from the STREAM object, which must be a SIMPLE-BIT-STREAM:BIT-STREAM."  
  (ecase (slot-value stream 'byte-order)
    (:big-endian
     (init-read stream)
     (read-bytes-with-partial/macro stream bits :big-endian :adjustable t))
    (:little-endian
     (init-read stream)
     (read-bytes-with-partial/macro stream bits :little-endian :adjustable t))))

(defun read-bits/big-endian (bits stream)
  (cond ((< bits (slot-value stream 'element-bits))
	 (read-partial-byte/big-endian bits stream))
	((= bits (slot-value stream 'element-bits))
	 (read-byte stream))
	(t
	 (let ((result 0)
	       (element-bits (slot-value stream 'element-bits)))
	   (multiple-value-bind (buffer partial-byte remaining-bits)
	       (read-bytes-with-partial/macro stream bits :big-endian)
	     (loop for byte across buffer
		for bit-shift from bits downto remaining-bits by element-bits
		do (incf result (ash byte bit-shift)))
	     (logior result partial-byte))))))

(defun read-bits/little-endian (bits stream)
  (cond ((< bits (slot-value stream 'element-bits))
	 (read-partial-byte/little-endian bits stream))
	((= bits (slot-value stream 'element-bits))
	 (read-byte stream))
	(t
	 (let ((result 0)
	       (element-bits (slot-value stream 'element-bits)))
	   (multiple-value-bind (buffer partial-byte remaining-bits)
	       (read-bytes-with-partial/macro stream bits :little-endian)
	     (loop for byte across buffer
		for bit-shift from 0 by element-bits
		do (incf result (ash byte bit-shift)))
	     (logior result
		     (ash partial-byte
			  (- bits remaining-bits))))))))

(defun read-bits (bits stream)
  "Reads BITS bits from STREAM. If the STREAM is big-endian, the most
significant BITS bits will be read, otherwise, the least significant BITS bits
will be. The result is an integer of BITS bits."
  (ecase (slot-value stream 'byte-order)
    (:little-endian
     (init-read stream)
     (read-bits/little-endian bits stream))
    (:big-endian
     (init-read stream)
     (read-bits/big-endian bits stream))))

(defun write-bits (n n-bits stream)
  (when (= n-bits 0)
    (return-from write-bits (values)))
  (ecase (slot-value stream 'byte-order)
    (:little-endian
     (init-write stream)
     (push-bits n (slot-value stream 'bits-left)
		(slot-value stream 'last-byte))
     (incf (slot-value stream 'bits-left) n-bits)
     (loop while (>= (slot-value stream 'bits-left)
		     (slot-value stream 'element-bits))
	do
	  (write-byte (pop-bits/le (slot-value stream 'element-bits)
				   (slot-value stream 'last-byte))
		      (slot-value stream 'real-stream))
	  (decf (slot-value stream 'bits-left)
		(slot-value stream 'element-bits))))
    (:big-endian
     (init-write stream)
     (push-bits/le n n-bits
		   (slot-value stream 'last-byte))
     (incf (slot-value stream 'bits-left) n-bits)
     (loop while (>= (slot-value stream 'bits-left)
		     (slot-value stream 'element-bits))
	do
	  (write-byte (pop-bits (slot-value stream 'element-bits)
				(slot-value stream 'bits-left)
				(slot-value stream 'last-byte))
		      (slot-value stream 'real-stream))
	  (decf (slot-value stream 'bits-left)
		(slot-value stream 'element-bits))))))

#-sbcl
(defmethod stream-file-position ((stream bit-stream))
  (cond ((slot-value stream 'real-stream)
	 (file-position (slot-value stream 'real-stream)))
	(t (error "Not implemented for POSIX/Win32 descriptors."))))

#-sbcl
(defmethod (setf stream-file-position) (position-spec (stream bit-stream))
  (declare (ignore position-spec))
  (setf (slot-value stream 'bits-left) 0)
  (setf (slot-value stream 'last-byte) nil))
  
#+sbcl
(defmethod sb-gray:stream-file-position  ((stream bit-stream) &optional position-spec)
  (when position-spec
    (setf (slot-value stream 'bits-left) 0)
    (setf (slot-value stream 'last-byte) nil))
  (file-position (slot-value stream 'real-stream) position-spec))

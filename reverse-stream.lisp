(defpackage :reverse-stream
  (:use :common-lisp :trivial-gray-streams :lisp-binary/integer :lisp-binary-utils)
  (:export :wrap-in-reverse-stream :with-wrapped-in-reverse-stream :reverse-stream)
  (:documentation "A stream that reads from another stream and reverses the bit order
of each byte so that the low-order bit becomes the high-order bit and vice versa.

This functionality is called for by the TIFF file format, because \"It is easy and
inexpensive for writers to reverse bit order by using a 256-byte lookup table.\" It
is devillishly tricky to include this functionality directly in the DEFBINARY macro,
however, when the macro was written without gratuitous bit-reversal in mind.

The REVERSE-STREAM does not keep track of any file positioning information. That means
it can coexist with its client stream, and you can mix reads and/or writes between
the two.

REVERSE-STREAM is not limited to 8-bit bytes. It can handle any byte size that the
underlying Lisp implementation supports. On PC hardware, some Lisps can read byte
sizes that are multiples of 8 bits, such as (UNSIGNED-BYTE 24)."))

(in-package :reverse-stream)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compute-reversed-byte (n bits)
    (let ((result 0))
      (loop repeat bits
	 for bits-left downfrom bits
	   do (push-bits (pop-bits 1 bits-left n) (- bits bits-left) result))
      result)))

(defun make-lookup-table (bits)
  (make-array (list (expt 2 bits))
	      :element-type `(unsigned-byte ,bits)
	      :initial-contents
	      (loop for n from 0 below (expt 2 bits)
		   collect (compute-reversed-byte n bits))))

(defvar *8-bit-lookup-table* (make-lookup-table 8))

(defclass reverse-stream (fundamental-binary-stream)
  ((element-bits :type fixnum :initform 8 :initarg :element-bits)
   (lookup-table :initform nil :initarg :lookup-table)
   (real-stream :type stream :initarg :real-stream)))
       		

(defgeneric wrap-in-reverse-stream (object)
  (:documentation "Creates a REVERSE-STREAM that can read one bit at a time from the OBJECT. The REVERSE-STREAM
can be discarded if BYTE-ALIGNED-P returns T."))

(defmethod wrap-in-reverse-stream ((object stream))
  (let ((element-type (stream-element-type object)))
    (assert (and (listp element-type)
		 (= (length element-type) 2)
		 (eq (car element-type) 'unsigned-byte)))
    (let ((bits (cadr element-type)))
      (make-instance 'reverse-stream :real-stream object
		     :element-bits bits
		     :lookup-table (if (= bits 8)
					*8-bit-lookup-table*
					(make-lookup-table bits))))))
		   

(defmacro with-wrapped-in-reverse-stream ((var non-bitstream &key  close-when-done) &body body)
  `(let ((,var (wrap-in-reverse-stream ,non-bitstream)))
     (unwind-protect
	  (progn
	    ,@body)
       (finish-output ,var)
       ,@(if close-when-done
	     `((if ,close-when-done
		   (close ,var)))))))
       

(defmethod stream-finish-output ((stream reverse-stream))
  (finish-output (slot-value stream 'real-stream)))

(defmethod stream-force-output ((stream reverse-stream))
  (force-output (slot-value stream 'real-stream)))

(defmethod close ((stream reverse-stream) &key abort)
  (apply #'close (list* (slot-value stream 'real-stream)
			(if abort
			    (list :abort abort)))))

(defun reverse-byte (byte lookup-table)
  (aref lookup-table byte))

(declaim (inline reverse-byte))

(defmethod stream-read-byte ((stream reverse-stream))
  (reverse-byte
   (read-byte (slot-value stream 'real-stream))
   (slot-value stream 'lookup-table)))

(defmethod stream-write-byte ((stream reverse-stream) integer)
  (write-byte (reverse-byte integer
			    (slot-value stream 'lookup-table))
	      stream))

(defun %stream-write-sequence (stream sequence start end)
  (let ((reversed (mapseq (lambda (element)
			    (reverse-byte element (slot-value stream 'lookup-table)))
			  sequence)))
    (write-sequence reversed stream :start start :end end)))


#-sbcl
(defmethod stream-write-sequence ((stream reverse-stream) sequence start end &key &allow-other-keys)
  (%stream-write-sequence stream sequence (or start 0) (or end (1- (length sequence)))))

#+sbcl
(defmethod sb-gray:stream-write-sequence ((stream reverse-stream) seq &optional start end)
  (%stream-write-sequence stream seq (or start 0) (or end (length seq))))

(defun %stream-read-sequence (stream sequence start end)
  (prog1 (read-sequence sequence (slot-value stream 'real-stream) :start start :end end)
    (if (listp sequence)
	(loop for hd on sequence
	   while hd
	   do (rplaca hd (reverse-byte (car hd)
				       (slot-value stream 'lookup-table))))
	(loop for ix from 0 below (length sequence)
	   do (setf (aref sequence ix)
		    (reverse-byte (aref sequence ix)
				  (slot-value stream 'lookup-table)))))))


#-sbcl
(defmethod stream-read-sequence ((stream reverse-stream) sequence start end &key &allow-other-keys)
  (%stream-read-sequence stream sequence start end))

#+sbcl
(defmethod sb-gray:stream-read-sequence ((stream reverse-stream) (sequence array) &optional start end)
  (%stream-read-sequence stream sequence (or start 0) (or end (length sequence))))


#-sbcl
(defmethod stream-file-position ((stream reverse-stream))
  (file-position (slot-value stream 'real-stream)))

#+sbcl
(defmethod sb-gray:stream-file-position  ((stream reverse-stream) &optional position-spec)
  (if position-spec
      (file-position (slot-value stream 'real-stream) position-spec)
      (file-position (slot-value stream 'real-stream))))

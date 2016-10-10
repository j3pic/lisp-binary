(defpackage :lisp-binary/buffer-streams
  (:use :common-lisp :trivial-gray-streams))

(in-package :lisp-binary/buffer-streams)

(defclass buffer-stream (fundamental-binary-stream)
  ((buffer :initarg :buffer)
   (read-pointer :initform 0)))

(defun make-buffer-stream (element-type)
  (make-instance 'buffer-stream :buffer (make-array 0 :element-type element-type :adjustable t :fill-pointer t)))

(defmethod stream-write-byte ((stream buffer-stream) byte)
  (vector-push-extend byte (slot-value stream 'buffer)))

(defmethod stream-read-byte ((stream buffer-stream))
  (handler-case
      (aref
       (slot-value stream 'buffer)
       (prog1 (slot-value stream 'read-pointer)
	 (incf (slot-value stream 'read-pointer))))
    (t ()
      :eof)))

;; TRIVIAL-GRAY-STREAMS seems to be broken on SBCL.

(defmethod #+sbcl sb-gray:stream-write-sequence
    #-sbcl stream-write-sequence ((stream buffer-stream) sequence #+sbcl &optional start end #-sbcl &key)
  (loop for ix from start to (or end (1- (length sequence)))
       do (write-byte (aref sequence ix) stream)))

(defmacro with-output-to-buffer ((var &key (element-type ''(unsigned-byte 8))) &body body)
  `(let ((,var (make-buffer-stream ,element-type)))
     ,@body
     (slot-value ,var 'buffer)))
       
      

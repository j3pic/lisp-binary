(defpackage :exif
  (:use :common-lisp :lisp-binary :lisp-binary-utils))

(in-package :exif)

(define-enum tiff-type 2 (:byte-order :dynamic)
  (:unsigned-byte 1)
  :ascii
  :unsigned-short
  :unsigned-long
  :unsigned-rational ;; Two unsigned-longs
  :signed-byte
  :undefined
  :signed-short
  :signed-long
  :signed-rational
  :single-float
  :double-float)

(define-enum tiff-byte-order 2 (:byte-order :little-endian)
  (:little-endian #x4949)
  (:big-endian #x4d4d))

(defvar *pending-buffers* nil)
(defvar *tiff-start* nil)

(defun no-writer (obj stream)
  (declare (ignore obj stream))
  0)

(defun read-rational (type stream)
  (let-values* ((signed (ecase type
			  (:unsigned-rational nil)
			  (:signed-rational t)))
		((numerator numerator-bytes) (read-integer 4 stream :byte-order *byte-order*
							   :signed signed))
		((denominator denominator-bytes) (read-integer 4 stream :byte-order *byte-order*
							      :signed signed)))
    (values (/ numerator denominator)
	    (+ numerator-bytes denominator-bytes))))

(defun write-rational (value type stream)
  (let* ((signed (ecase type
		   (:unsigned-rational nil)
		   (:signed-rational t))))
    (+
     (write-integer (numerator value) 4 stream :byte-order *byte-order*
		    :signed signed)
     (write-integer (denominator value) 4 stream :byte-order *byte-order*
		    :signed signed))))  

(defun tiff-type->defbinary-type (type)
  (ecase type
    ((:unsigned-long :undefined) '(unsigned-byte 32))
    (:signed-long '(signed-byte 32))
    (:double-float 'double-float)
    (:single-float 'single-float)
    (:ascii '(terminated-string 1))
    ((:signed-rational :unsigned-rational)
     `(custom :reader (lambda (stream)
			(read-rational ,type stream))
	      :writer (lambda (obj stream)
			(write-rational obj ,type stream))))
    (:signed-byte '(signed-byte 8))
    (:unsigned-byte '(unsigned-byte 8))
    (:signed-short '(signed-byte 16))
    (:unsigned-short '(unsigned-byte 16))))

(defbinary directory-entry (:byte-order :dynamic)
  (tag 0 :type (unsigned-byte 16))
  (type 0 :type tiff-type)
  (count 0 :type (unsigned-byte 32) :writer (lambda (obj stream)
					      (declare (ignore obj))
					      (setf count (length value))
					      (write-integer count 4 stream :byte-order *byte-order*)))
  (value 0 :type (eval
		  (cond ((> count 1)
			 `(pointer :pointer-type (unsigned-byte 32)
				   :data-type ,(tiff-type->defbinary-type type)
				   :base-pointer-name 'tiff-base-pointer
				   :region-tag 'tiff-region))
			((member tag '(34665 ;; EXIF
				       34853 ;; GPS
				       40965)) ;; Interoperability
			 `(pointer :pointer-type (unsigned-byte 32)
				   :data-type tiff-image-file-directory
				   :base-pointer-name 'tiff-base-pointer
				   :region-tag 'tiff-region))
			(t
			 (case type
			   ((:undefined)
			    '(unsigned-byte 32))
			   ((:double-float :ascii :signed-rational :unsigned-rational)
			    `(pointer :pointer-type (unsigned-byte 32)
				      :data-type ,(tiff-type->defbinary-type type)
				      :base-pointer-name 'tiff-base-pointer
				      :region-tag 'tiff-region))
			   (otherwise (tiff-type->defbinary-type type)))))))
  (padding 0 :type (eval (if (> count 1)
			     'null
			     (ecase type
			       ((:unsigned-long :unsigned-rational :signed-rational :double-float :ascii :signed-long
						:single-float :undefined)
				'null)
			       ((:signed-byte :unsigned-byte)
				'(unsigned-byte 24))
			       ((:signed-short :unsigned-short)
				'(unsigned-byte 16)))))))

(defbinary tiff-image-file-directory
    (:align 2 :byte-order :dynamic)
  (directory-entries #() :type (counted-array 2 directory-entry))
  (next-directory-offset 0 :type (unsigned-byte 32)))


(defbinary tiff (:byte-order :dynamic :preserve-*byte-order* nil)
  (tiff-base-pointer 0 :type base-pointer)
  (byte-order 0 :type tiff-byte-order :reader (lambda (stream)
						      (values
						       (setf *byte-order* (read-enum 'tiff-byte-order stream))
						       2)))
  (magic 42 :type (magic :actual-type (unsigned-byte 16)
			 :value 42))
  (first-image-file-directory-offset 0 :type (unsigned-byte 32))
  (offset-ptr 0 :type (unsigned-byte 32)
	      :reader (lambda (stream)
			(declare (optimize (debug 3) (speed 0)))
			(values
			 (- (file-position stream) 4)
			 0))
	      :writer (lambda (obj stream)
			(declare (ignore obj))
			(setf offset-ptr (- (file-position stream) 4))
			0))
  (image-directories nil :type list
		     :reader (lambda (stream)
			       (let* ((next-directory nil)
				      (byte-count 0)
				      (directories
				       (loop for offset = first-image-file-directory-offset
					  then (slot-value next-directory 'next-directory-offset)
					  until (= offset 0)
					  collect (progn
						    (file-position stream (+ offset tiff-base-pointer))
						    (setf next-directory
							  (multiple-value-bind (dir bytes)
							      (read-binary 'tiff-image-file-directory stream)
							    (incf byte-count bytes)
							    dir))))))
				 (values directories byte-count)))
		     :writer (lambda (obj stream)
			       (declare (ignore obj))
			       (force-output stream)
			       (let ((real-offset (file-length stream)))
				 (file-position stream offset-ptr)
				 (write-integer (- real-offset tiff-base-pointer) 4 stream :byte-order *byte-order*)
				 (setf first-image-file-directory-offset real-offset)
				 (file-position stream real-offset)
				 (loop for (dir . more-dirs) on image-directories sum
				      (let ((bytes (write-binary dir stream))
					    (new-eof (file-position stream)))
					(force-output stream)
					(file-position stream (- new-eof 4))
					(write-integer (if more-dirs new-eof 0) 4 stream :byte-order *byte-order*)
					bytes)))))
  (tiff-region 0 :type (region-tag :base-pointer-name 'tiff-base-pointer)))
				    

(defbinary jpeg-generic-tag (:byte-order :big-endian)
  (offset 0 :type file-position)
  (magic #xff :type (magic :actual-type (unsigned-byte 8)
			   :value #xff))
  (code 0 :type (unsigned-byte 8))
  (length-offset 0 :type file-position)
  (length 0 :type (eval
		   (case code
		     ((#xd8 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7
			    #xd9 #xdd)
		      'null)
		     (otherwise
		      '(unsigned-byte 16)))))
  (restart-interval nil :type (eval
			       (if (= code #xdd)
				   '(unsigned-byte 32)
				   'null))))

(defbinary jpeg-app1-body (:byte-order :big-endian)
  (exif-header 0 :type (unsigned-byte 48))
  (body nil :type (eval (if (= exif-header #x457869660000)
			    'tiff
			    ;; FIXME!!! We need the LENGTH found just
			    ;; before this structure to create a type
			    ;; that reads the whole thing!
			    `(unsigned-byte 8)))))

(defun jpeg-tag-no-length-p (tag)
  (or (member (slot-value tag 'code)
	      '(#xd8 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7
		#xd9 #xdd))
      (not (slot-value tag 'length))))

(defbinary jpeg-generic-segment (:byte-order :big-endian)
  (tag nil :type jpeg-generic-tag)
  (contents nil :type (eval
		       (cond ((= (slot-value tag 'code) 225)
			      'jpeg-app1-body)
			     ((null (slot-value tag 'length))
			      'null)
			     (t `(simple-array (unsigned-byte 8) (,(- (slot-value tag 'length) 2)))))))
  (file-positioner nil :type (custom :reader
				     (lambda (stream)
				       (values
					(and (not (jpeg-tag-no-length-p tag))
					     (file-position stream
							    (+ (slot-value tag 'length-offset)
							       (slot-value tag 'length))))
					0))
				     :writer
				     (lambda (obj stream)
				       (declare (ignore obj))
					 (if (jpeg-tag-no-length-p tag)
					     (let ((end-position (file-position stream)))
					       (with-file-position ((slot-value tag 'length-offset) stream)
						 (write-integer (- end-position (slot-value tag 'length-offset))
								2 stream :byte-order :big-endian)))
					     0)))))
							    

(defun read-rest-of-stream (stream)
  (read-bytes (- (file-length stream)
		 (file-position stream))
	      stream))

(defparameter *segments* nil)

(defun remove-exif-data (input-file output-file)
  (with-open-binary-file (in input-file)
    (let ((jpeg-segments (loop for segment = (read-binary 'jpeg-generic-segment in)
			    until (= (slot-value
				      (slot-value segment 'tag) 'code) 225)
			    collect segment))
	  (rest-of-image (read-rest-of-stream in)))
      (setf *segments* jpeg-segments)
      (with-open-binary-file (out output-file
				  :direction :io
				  :if-exists :supersede
				  :if-does-not-exist :create)
	(loop for segment in jpeg-segments
	   do (write-binary segment out))
	(write-bytes rest-of-image out)))))

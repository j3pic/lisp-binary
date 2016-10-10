(defpackage :exif
  (:use :common-lisp :lisp-binary))

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

(defbinary directory-entry (:byte-order :dynamic)
  (tag 0 :type (unsigned-byte 16))
  (type 0 :type tiff-type)
  (count 0 :type (unsigned-byte 32))
  (value/offset 0 :type (eval
			 (if (> count 1)
			     '(unsigned-byte 32)
			     (ecase type
			       ((:unsigned-long :unsigned-rational :signed-rational :double-float :ascii)
				'(unsigned-byte 32))
			       (:signed-long '(signed-byte 32))
			       (:single-float 'single-float)
			       (:unsigned-byte '(unsigned-byte 8))
			       (:signed-byte '(signed-byte 8))
			       (:signed-short '(signed-byte 16))
			       (:unsigned-short '(unsigned-byte 16))))))
  (padding 0 :type (eval (if (> count 1)
			     'null
			     (ecase type
			       ((:unsigned-long :unsigned-rational :signed-rational :double-float :ascii :signed-long
						:single-float)
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
  (initial-offset 0 :type integer
		  :reader (lambda (stream)
			    (values (file-position stream)
				    0))
		  :writer (lambda (obj stream)
			    (declare (ignore obj))
			    (setf initial-offset (file-position stream))
			    0))
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
						    (file-position stream (+ offset initial-offset))
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
				 (write-integer (- real-offset initial-offset) 4 stream :byte-order *byte-order*)
				 (setf first-image-file-directory-offset real-offset)
				 (file-position stream real-offset)
				 (loop for (dir . more-dirs) on image-directories sum
				      (let ((bytes (write-binary dir stream))
					    (new-eof (file-position stream)))
					(force-output stream)
					(file-position stream (- new-eof 4))
					(write-integer (if more-dirs new-eof 0) 4 stream :byte-order *byte-order*)
					bytes))))))


(defun no-writer (obj stream)
  (declare (ignore obj stream))
  0)

(defbinary jpeg-generic-tag (:byte-order :big-endian)
  (offset 0 :type integer :reader (lambda (stream)
				    (values (file-position stream)
					    0))
	  :writer #'no-writer)
  (magic #xff :type (magic :actual-type (unsigned-byte 8)
			   :value #xff))
  (code 0 :type (unsigned-byte 8))
  (length 0 :type (eval
		   (case code
		     ((#xd8 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7
			    #xd9 #xdd)
		      'null)
		     (otherwise '(unsigned-byte 16)))))
  (restart-interval nil :type (eval
			       (if (= code #xdd)
				   '(unsigned-byte 32)
				   'null))))

(defbinary jpeg-app0-segment (:byte-order :big-endian)
  (soi #xffd8 :type (magic :actual-type (unsigned-byte 16)
			   :value #xffd8))
  (app0 #xffe0 :type (magic :actual-type (unsigned-byte 16)
			    :value #xffe0))
  (length-setter nil :type null
		 :writer (lambda (obj stream)
			   (declare (ignore obj stream))
			   (setf length (+ (length buffer) 7))))
  (length 0 :type (unsigned-byte 16))
  (identifier "JFIF" :type (magic :actual-type (terminated-string 1
								  :terminator 0)
				  :value "JFIF"))
  (buffer #() :type (simple-array (unsigned-byte 8)
				  ((- length 7)))))


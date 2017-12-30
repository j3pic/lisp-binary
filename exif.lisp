(defpackage :exif
  (:use :common-lisp :lisp-binary :lisp-binary-utils))

(in-package :exif)

(declaim (optimize (debug 3) (speed 0)))

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

(defun slot-path (object &rest slots)
  (loop for slot in slots do
       (setf object (slot-value object slot)))
  object)

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

(defun ensure-non-null-pointer (pointer-value)
  (assert (/= 0 pointer-value)))

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
  (file-position 0 :type file-position)
  (tag 0 :type (unsigned-byte 16))
  (type 0 :type tiff-type)
  (count 0 :type (unsigned-byte 32)
	 ;; This custom writer ensures that the COUNT remains
	 ;; synced with the length of the VALUE below.
	 :writer (lambda (obj stream)
		   (declare (ignore obj))
		   (setf count (if (eq type :ascii)
				   (1+ (length value))
				   (if (or (listp value)
					   (vectorp value))
				       (length value)
				       1)))
		   (write-integer count 4 stream :byte-order *byte-order*)))
  ;; The VALUE depends on both the TYPE and the COUNT. The total number of
  ;; bytes needed to store the VALUE is given by (size of the type) * (count).
  ;; If this size is > 4 bytes, then the type of the VALUE must resolve to:
  ;;
  ;;   (pointer :pointer-type (unsigned-byte 32)
  ;;            :data-type the-actual-type-of-the-value
  ;;            :base-pointer 'tiff-base-pointer
  ;;            :region-tag 'tiff-region)
  ;;
  ;; The BASE-POINTER is declared at the beginning of the TIFF type definition,
  ;; and tells the LISP-BINARY library that the pointer is an offset that
  ;; begins at the BASE-POINTER.
  ;;
  ;; The REGION-TAG is used in writing the pointer. The region tag named
  ;; TIFF-REGION is also declared in the TIFF type definition, and determines
  ;; where the data being pointed to will end up going. The value is merely
  ;; STORED when the pointer is written, and it gets written to disk when the
  ;; REGION-TAG is reached.
  (value 0 :type (eval
		  (cond
		    ;; This tells LISP-BINARY that strings with a count <= 4
		    ;; (which includes the terminating NUL) will fit inside
		    ;; the VALUE, so we don't need a pointer.
		    ((and (eq type :ascii)
			  (<= count 4))
		     (tiff-type->defbinary-type :ascii))
		    ;; This tells LISP-BINARY that byte arrays with
		    ;; count <= 4 and short arrays with count <= 2
		    ;; will fit within the VALUE.
		    ((or (and (member type '(:signed-byte :unsigned-byte))
			      (<= count 4)
			      (> count 1))
			 (and (member type '(:signed-short :unsigned-short))
			      (= count 2)))
		     `(simple-array ,(tiff-type->defbinary-type type)
				    (,count)))
		    ;; This makes any other array type into a pointer.
		    ((> count 1)
		     `(pointer :pointer-type (unsigned-byte 32)
			       :data-type ,(if (eq type :ascii)
					       (tiff-type->defbinary-type :ascii)
					       `(simple-array ,(tiff-type->defbinary-type type) (,count)))
			       :base-pointer-name 'tiff-base-pointer
			       :validator #'ensure-non-null-pointer
			       :region-tag 'tiff-region))
		    ;; Certain tags mark their value as being an
		    ;; unsigned long, but they should really be treated
		    ;; as pointers to Image File Directories.
		    ((member tag '(34665       ;; EXIF
				   34853       ;; GPS
				   40965))     ;; Interoperability
		     `(pointer :pointer-type (unsigned-byte 32)
			       :data-type tiff-image-file-directory
			       :base-pointer-name 'tiff-base-pointer
			       :validator #'ensure-non-null-pointer
			       :region-tag 'tiff-region))
		    (t
		     (case type
		       ((:undefined)
			'(unsigned-byte 32))
		       ;; Some types are just plain too big to fit in the VALUE.
		       ;; Generate pointers to them.
		       ((:double-float :signed-rational :unsigned-rational :ascii)
			`(pointer :pointer-type (unsigned-byte 32)
				  :data-type ,(tiff-type->defbinary-type type)
				  :base-pointer-name 'tiff-base-pointer
				  :validator #'ensure-non-null-pointer
				  :region-tag 'tiff-region))
		       ;; Some types will fit just fine.
		       (otherwise (tiff-type->defbinary-type type)))))))
  ;; If the VALUE doesn't use 32 bits, then it must be padded. The PADDING
  ;; is an unsigned integer that fills in the space not used by the VALUE.
  (padding 0 :type (eval (if (> count 1)
			     'null
			     (ecase type
			       (:ascii
				(if (>= count 4)
				    'null
				    `(unsigned-byte ,(* 8 (- 4 count)))))
			       ((:unsigned-long :unsigned-rational :signed-rational :double-float :signed-long
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

(defun write-integer/debug (&rest arguments)
  (format t "Calling ~S~%" (cons 'write-integer arguments))
  (apply #'write-integer arguments))

(defbinary tiff (:byte-order :dynamic :preserve-*byte-order* nil)
  (tiff-base-pointer 0 :type base-pointer)
  (byte-order 0 :type tiff-byte-order :reader (lambda (stream)
						      (values
						       (setf *byte-order* (read-binary-type 'tiff-byte-order stream))
						       2)))
  (magic 42 :type (magic :actual-type (unsigned-byte 16)
			 :value 42))
  (offset-ptr 0 :type file-position)
  (first-image-file-directory-offset 0 :type (unsigned-byte 32))
  (image-directories nil :type (custom
				:reader (lambda (stream)
					  (let* ((next-directory nil)
						 (byte-count 0)
						 (directories
						  (with-file-position (0 stream)
						    (loop for offset = first-image-file-directory-offset
						       then (slot-value next-directory 'next-directory-offset)
						       until (= offset 0)
						       collect (progn
								 (file-position stream (+ offset tiff-base-pointer))
								 (setf next-directory
								       (multiple-value-bind (dir bytes)
									   (read-binary 'tiff-image-file-directory stream)
									 (incf byte-count bytes)
									 dir)))))))
					    (values directories byte-count)))
				:writer (lambda (obj stream)
					  (declare (ignore obj))
					  (force-output stream)
					  (let ((real-offset (file-length stream)))
					    (with-file-position (offset-ptr stream)
					      (write-integer (- real-offset tiff-base-pointer) 4 stream :byte-order *byte-order*)
					      (setf first-image-file-directory-offset (- real-offset tiff-base-pointer))
					      (file-position stream real-offset)
					      (loop for (dir . more-dirs) on image-directories sum
						   (let ((bytes (write-binary dir stream))
							 (new-eof (file-position stream)))
						     (force-output stream)
						     (file-position stream (- new-eof 4))
						     (write-integer (if more-dirs
									(- new-eof tiff-base-pointer)
									0) 4 stream :byte-order *byte-order*)
						     bytes)))))))
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

(defvar *exif-header* #x457869660000)

(defbinary jpeg-app1-body (:byte-order :big-endian)
  (exif-header 0 :type (unsigned-byte 48))
  (body nil :type (eval (if (= exif-header *exif-header*)
			    'tiff
			    'null))))

(defun jpeg-tag-no-length-p (tag)
  (or (member (slot-value tag 'code)
	      '(#xd8 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7
		#xd9 #xdd))
      (not (slot-value tag 'length))))


(defbinary jpeg-generic-segment (:byte-order :big-endian)
  (file-position 0 :type file-position)
  (tag nil :type jpeg-generic-tag)
  (contents nil :type (eval
		       (cond ((= (slot-value tag 'code) 225)
			      'jpeg-app1-body)
			     ((null (slot-value tag 'length))
			      'null)
			     (t `(simple-array (unsigned-byte 8) (,(- (slot-value tag 'length) 2)))))))
  (buffer nil :type (eval
		     (if (or (and (jpeg-app1-body-p contents)
				  (tiff-p (slot-value contents 'body)))
			     (jpeg-tag-no-length-p tag))
			 'null
			 `(simple-array (unsigned-byte 8) (,(- (slot-value tag 'length) 8))))))
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


(defun remove-exif-data (input-file output-file)
  (with-open-binary-file (in input-file)
    (let ((jpeg-segments (loop for segment = (read-binary 'jpeg-generic-segment in)
			    until (= (slot-value
				      (slot-value segment 'tag) 'code) 225)
			    collect segment))
	  (rest-of-image (read-rest-of-stream in)))
      (with-open-binary-file (out output-file
				  :direction :io
				  :if-exists :supersede
				  :if-does-not-exist :create)
	(loop for segment in jpeg-segments
	   do (write-binary segment out))
	(write-bytes rest-of-image out)))))

(defun read/write-test (pathname object type &key (base-pointer 0) tags)
  (with-local-pointer-resolving-context
    (with-open-binary-file (out pathname
				:direction :io
				:if-exists :supersede
				:if-does-not-exist :create)
      (write-binary-type object
			 type out)
      (loop for tag in tags
	 do (lisp-binary::dump-tag tag base-pointer out)))
    (with-open-binary-file (in pathname
			       :direction :input)
      (read-binary-type type in))))

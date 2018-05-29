(defpackage :tiff-format
  (:use :lisp-binary
	:common-lisp
	:lisp-binary-utils))

(in-package :tiff-format)

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

(define-enum photometric-interpretation 2 (:byte-order :dynamic)
    :low
    :high
    :rgb)

(define-enum fill-order 2 (:byte-order :dynamic)
	     (:reversed-bits 1)
	     (:normal-bits 2))

(defbinary new-subfile-type (:byte-order :dynamic)
  (reserved 0 :type (unsigned-byte 29))
  (transparency-mask-p 0 :type (unsigned-byte 1))
  (one-of-many-pages-p 0 :type (unsigned-byte 1))
  (reduced-resolution-p 0 :type (unsigned-byte 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Finish this. The number of possible tags is huge, and
  ;;       many of them require complicated handlers to be written
  ;;       for them. I could be at this for months!
  (defparameter *tag-info*
    `((:new-subfile-type 254
       :type new-subfile-type
       :n 1
       :documentation "A few flags.")
      (:image-width 256
       :type (or :unsigned-short :unsigned-long)
       :n 1)
      (:image-length 257
       :type (or :unsigned-short :unsigned-long)
       :n 1)
      (:bits-per-sample 258
       :type :unsigned-short
       :default 1
       :n :samples-per-pixel
       :documentation "In an RGB file, there must be a separate
one of these for each of the components.")     
		       
      (:compression 259
       :type :unsigned-short
       :n 1
       :default 1
       :documentation "1 = No compression.
2 = CCITT Group Modified Huffman RLE. :BITS-PER-SAMPLE must be 1.
32773 = PackBits compression.")
      (:cell-width 264
       :type :unsigned-short
       :n 1
       :documentation "The width of a dithering or halftoning matrix")
      (:cell-length 265
       :type :unsigned-short
       :n 1
       :documentation "The length of a dithering or halftoning matrix. Should only
exist if :THRESHOLDING = 2")
      (:photometric-interpretation 262)
      (:fill-order 266
       :type fill-order
       :default :reversed-bits
       :n 1
       :documentation "If equal to :REVERSED-BITS, then the high-order bit in each
byte of a pixel is actually the low-order bit, and vice versa. If equal to :NORMAL,
then the bytes can be interpreted as they are found on disk. Adobe recommends :REVERSED-BITS,
which is the default, and :NORMAL isn't required to be supported by
TIFF readers.")
      (:image-description 270
       :type :ascii
       :documentation "Some text to describe the image.")
      (:scanner-make 271
       :type :ascii
       :documentation "The manufacturer of the scanner that scanned this image.")
      (:scanner-model 272
       :type :ascii
       :documentation "The model of the scanner that scanned the image.")
      (:strip-offsets 273)
      (:samples-per-pixel 277)
      (:strip-byte-counts 279)
      (:rows-per-strip 278)
      (:min-sample-value 280
       :type :unsigned-short
       :default 0
       :n :samples-per-pixel
       :default 0)
      (:max-sample-value 281
       :type :unsigned-short
       :n :samples-per-pixel
       :default (1- (expt 2 :bits-per-sample))
       :documentation "The maximum sample value that appears in the image.")
      (:x-resolution 282)
      (:y-resolution 283)
      (:free-offsets 288
       :type :unsigned-long
       :documentation "Not recommended for general interchange")
      (:free-byte-counts 289
       :type :unsigned-long
       :documentation "Not recommended for general interchange.")
      (:gray-response-unit 290
       :type :unsigned-short
       :n 1
       :default 2
       :documentation "The absolute value of the negative exponent of 10 that will be
multiplied with the :GRAY-RESPONSE-CURVE to obtain the actual value.")
      (:gray-response-curve 291
       :type :unsigned-short
       :n (expt 2 :bits-per-sample)
       :documentation "For grayscale data, the optical density of each pixel.")
      (:resolution-unit 296)
      (:software 305)
      (:datetime 306
       :type :ascii
       :n 20
       :documentation "YYYY:MM:DD HH:MM:SS")
      (:artist 315
       :type :ascii
       :documentation "Who created this? Sometimes contains copyright notice.")
      (:host-computer 316
       :type :ascii
       :documentation "Gratuitous information to compromise the security of the person who created the TIFF file.
Adobe recommends that it contain \"the computer and/or operating system\" of the author.")
      (:color-map 320
       :type :unsigned-short
       :n (* 3 (expt 2 :bits-per-sample))
       :documentation "A color map for palletted images.
The first 2^BITS-PER-SAMPLE integers are the reds, the second
set are the greens, and finally the blues. Each integer represents
an intensity of the color whose section of the array it occupies.")
      (:extra-samples 338
       :type :unsigned-short
       :n ":samples-per-pixel - (the number of samples expected given the :PHOTOMETRIC-INTERPRETATION)"
       :documentation		     
       "If the :SAMPLES-PER-PIXEL are greater than the number
of components needed for the :PHOTOMETRIC-INTERPRETATION,  then this field
defines what each of the extra components is for. This array will contain one element
for each extra component-per-pixel, and each element will have one of the following values:

0 = Unspecified
1 = Associated Alpha with pre-multiplied color
2 = Unassociated alpha"
       (:copyright 33432 :type :ascii)))))

(define-enum tag 2 (:byte-order :dynamic)
  (:new-subfile-type 254)
  (:image-width 256)
  (:image-length 257)
  (:bits-per-sample 258)
  (:compression 259)
  (:photometric-interpretation 262)
  (:strip-offsets 273)
  (:samples-per-pixel 277)
  (:strip-byte-counts 279)
  (:rows-per-strip 278)
  (:x-resolution 282)
  (:y-resolution 283)  
  (:resolution-unit 296)
  (:software 305)
  (:datetime 306)
  (:artist 315)
  (:color-map 320))
  

(define-enum tiff-byte-order 2 (:byte-order :little-endian)
  (:little-endian #x4949)
  (:big-endian #x4d4d))

(defbinary directory-entry (:byte-order :dynamic)
  (tag 0 :type tag)
  (type 0 :type tiff-type)
  (count 0 :type (unsigned-byte 32))
  (value/offset 0 :type (eval
			 (if (> count 1)
			     '(unsigned-byte 32)
			     (ecase type
			       ((:unsigned-long :unsigned-rational :signed-rational :double-float :ascii :undefined)
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
						:single-float :undefined)
				'null)
			       ((:signed-byte :unsigned-byte)
				'(unsigned-byte 24))
			       ((:signed-short :unsigned-short)
				'(unsigned-byte 16)))))))
			   
(defbinary image-file-directory
    (:align 2 :byte-order :dynamic)
  (directory-entries #() :type (counted-array 2 directory-entry))
  (next-directory-offset 0 :type (unsigned-byte 32)))

(defbinary tiff (:byte-order :dynamic :preserve-*byte-order* nil)
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
						    (file-position stream offset)
						    (setf next-directory
							  (multiple-value-bind (dir bytes)
							      (read-binary 'image-file-directory stream)
							    (incf byte-count bytes)
							    dir))))))
				 (values directories byte-count)))
		     :writer (lambda (obj stream)
			       (declare (ignore obj))
			       (force-output stream)
			       (let ((real-offset (file-length stream)))
				 (file-position stream offset-ptr)
				 (write-integer real-offset 4 stream :byte-order *byte-order*)
				 (setf first-image-file-directory-offset real-offset)
				 (file-position stream real-offset)
				 (loop for (dir . more-dirs) on image-directories sum
				      (let ((bytes (write-binary dir stream))
					    (new-eof (file-position stream)))
					(force-output stream)
					(file-position stream (- new-eof 4))
					(write-integer (if more-dirs new-eof 0) 4 stream :byte-order *byte-order*)
					bytes))))))
	      

(defun read-tiff-file (filename)
  (handler-bind ((bad-enum-value
		  (lambda (exn)
		    (unless (eq (slot-value exn 'enum-name) 'tiff-byte-order)
		      (cond ((slot-value exn 'integer-value)
			      (invoke-restart 'use-value (make-symbol
							 (format nil "UNKNOWN-~a" (slot-value exn 'integer-value))))))))))
    (with-open-binary-file (in filename)
      (read-binary 'tiff in))))

(defun passing-test ()
  (with-open-binary-file (in #P"/usr/share/apps/quanta/templates/images/others/demo.tif")
    (let ((tiff (read-binary 'tiff in)))
      (with-open-binary-file (out #P"/tmp/test.tif"
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	(write-binary tiff out))
      (with-open-binary-file (in* #P"/tmp/test.tif")
	(read-binary 'tiff in*)))))

(defun create-sample-tif (filename)
  (with-open-binary-file (out filename
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
    (write-integer #x4d4d002a00000014 8 out :byte-order :big-endian)
    (loop for (integer size) in
	 '((0 12)
	   (#xc 2)
	   (#x00fe00040000000100000000 12)
	   (#x0100000400000001000007d0 12)
	   (#x010100040000000100000bb8 12)
	   (#x010300030000000180050000 12)
	   (#x010600030000000100010000 12)
	   (#x01110004000000bc000000b6 12)
	   (#x011600040000000100000010 12)
	   (#x01170003000000bc000003a6 12)
	   (#x011a00050000000100000696 12)
	   (#x011b0005000000010000069e 12)
	   (#x013100020000000e000006a6 12)
	   (#x0132000200000014000006b6 12)
	   (0 4))
       do (write-integer integer size out :byte-order :big-endian))))

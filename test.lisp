(defpackage :lisp-binary-test
  (:use :common-lisp :lisp-binary))

(in-package :lisp-binary-test)

(defbinary index-test ()
  (pointers #() :type (counted-array 2 (unsigned-byte 32)))
  (data #() :type (simple-array (unsigned-byte 32) ((length pointers)))))

(defparameter test-data
  (make-index-test :pointers (make-array 3 :element-type '(unsigned-byte 32) :initial-contents '(0 0 0))
		   :data (make-array 3 :element-type '(unsigned-byte 32)
				     :initial-contents '(12 13 14))))

(defmacro with-output-file ((var filename) &body body)
  `(with-open-file (,var ,filename
		       :direction :io
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :element-type '(unsigned-byte 8))
    ,@body))

(defmacro with-input-file ((var filename) &body body)
  `(with-open-file (,var ,filename
			 :direction :input
			 :element-type '(unsigned-byte 8))
     ,@body))

(defun write-test-data (filename)
  (with-output-file (out filename)
    (write-binary test-data out)))

(defbinary just-a-pointer-array ()
  (arr nil :type (counted-array 2 (unsigned-byte 32))))

(defun write-pointer-array (arr stream)
  (let ((wrapped (make-just-a-pointer-array :arr arr)))
    (write-binary wrapped stream)))

(defun write-test-data/pointers-as-list (filename)
  "This function implements the 'write the pointers as zero then
collect a list of their addresses and go back and fix the zeroes' approach
to resolving pointers at write-time.

Three major flaws:

   1. The array of pointers gets written twice.
   2. The list of pointers must be a literal raw list of pointers.
   3. The list of pointers will take up a lot of memory, especially for
      a large binary file.

Unlike the approach described in the comments in binary.lisp, we
don't create an association between the values in POINTERS and those
in DATA. Instead, we assume that the elements of DATA will be in the
same order as those in the POINTERS."
  (with-output-file (out filename)
    (write-pointer-array (slot-value test-data 'pointers) out)
    (let ((pointer-addresses
	   (loop for value across (slot-value test-data 'data)
	      collect (prog1 (file-position out)
			(write-integer value 4 out)))))
      (file-position out 0)
      (loop for addr in pointer-addresses
	 for ix from 0
	 do (setf (aref (slot-value test-data 'pointers) ix) addr))
      (write-pointer-array (slot-value test-data 'pointers) out))))

(defun read-test-data (filename)
  (with-input-file (in filename)
    (read-binary 'index-test in)))

(defbinary employee ()
  (name "" :type (counted-string 1))
  (age 0 :type (unsigned-byte 8))
  (job-title "" :type (counted-string 1))
  (salary 0 :type (unsigned-byte 64)))

(defmacro fill-array-with (array size value-form)
  (let ((ix (gensym)))
    `(loop for ,ix from 0
	repeat ,size
	  do (setf (aref ,array ,ix) ,value-form))))

(defmacro make-array-filled-with (size element-type value-form)
  (let ((result (gensym))
	(size* (gensym)))
    `(let* ((,size* ,size)
	    (,result (make-array (list ,size*) :element-type ,element-type)))
       (fill-array-with ,result ,size* ,value-form)
       ,result)))

(defun read-counted-array (type count-size stream)
  (let ((count (read-integer count-size stream)))
    (make-array-filled-with count t (read-binary type stream))))

(defun write-counted-array (array count-size stream)
  (write-integer (length array) count-size stream)
  (loop for object across array
       do (write-binary array stream)))

(defun read-integer-counted-array (int-size/bytes count-size stream)
  (make-array-filled-with (read-integer count-size stream) t
			  (read-integer int-size/bytes stream)))

(defun write-integer-counted-array (array int-size count-size stream)
  (write-integer (length array) count-size stream)
  (loop for integer across array
       do (write-integer integer int-size stream)))

(defbinary department ()
  (name "" :type (counted-string 1))
  ;; The T type allows the HEAD to exist in memory as
  ;; an EMPLOYEE object, even though it has to be converted
  ;; to an integer before writing it to disk.
  (head nil :type t
	:reader (lambda (stream)
		  (read-integer 4 stream))
	:writer (lambda (obj stream)
		  (write-integer obj 4 stream)))
  (peons nil :type t
	 :reader (lambda (stream)
		   (read-integer-counted-array 4 4 stream))
	 :writer (lambda (obj stream)
		   (write-integer-counted-array obj 4 4 stream))))

;; A test struct that contains values that have
;; pointers back into the parent object.
(defbinary company ()
  (ceo nil :type employee)
  (cfo nil :type employee)
  (coo nil :type employee)
  (departments nil :type (counted-array 4 department))
  (peons nil :type (counted-array 4 employee)))

(defmacro draw-random (place)
  (let ((n (gensym))
	(head (gensym))
	(pick (gensym))
	(car (gensym))
	(cdr (gensym)))
    `(let ((,n (random (length ,place))))
       (prog1 (nth ,n ,place)
	 (setf ,place
	       (loop for item in ,place
		  for nn from 0
		  unless (= nn ,n)
		    collect item))))))

(defparameter *first-names*
  '("Jeremy" "Amine" "Gary" "Amy" "Donna" "Debi" "Sarah"
    "Mike" "Randy" "Matt" "Brian" "Mick" "Nick" "Rudy"
    "Bruce" "Joel" "Isabella" "Austin" "Doctor"))

(defparameter *surnames*
  '("Phelps" "Bitar" "Zeidenstein" "Brugel" "O'Connor" "Striblen"
    "Stinson" "Fatter" "Penis" "Feller" "Powers" "Evil" "Gonzales"
    "Weiner" "Scarborough"))

(defun pick (list)
  (nth (random (length list)) list))

(defun invent-name ()
  (format nil "~a ~a" (pick *first-names*) (pick *surnames*)))

(defun random-between (low high)
  (+ (random (- high low)) low))

(defun invent-employee (&key (salary 1) (job-title (pick '("Knob Polisher" "Ass Wiper" "Sack Scratcher"))))
  (make-employee :name (invent-name)
			     :age (random-between 18 64)
			     :job-title job-title
			     :salary salary))

(defun promote (employee new-job-title new-salary)
  (setf (slot-value employee 'job-title) new-job-title)
  (setf (slot-value employee 'salary) new-salary)
  employee)

(defun make-employees (count)
  (let ((employees
	 (loop repeat count collect
	      (invent-employee))))
    (make-array (length employees) :element-type 'employee
		:initial-contents employees)))

(defparameter *employee-array* (make-employees 1000))
(defparameter *employee-pool* (coerce *employee-array* 'list))
(defparameter *department-pool* '("Accounts Receivable" "Accounts Payable" "Receiving" "Shipping"
				  "Quality Assurance" "Security" "Insecurity" "Energy" "Deli"
				  "Weapons" "Operations"))

(defun invent-department (max-size)
  (let ((department-name (draw-random *department-pool*)))
    (make-department :name department-name
		     :head (promote (draw-random *employee-pool*)
				    (format nil "Director of ~a" department-name)
				    50000)
		     :peons (remove nil
				    (make-array-filled-with (random max-size) t
							    (draw-random *employee-pool*))))))
					  

(defparameter *company*
  (make-company :ceo (invent-employee :job-title "CEO"
				      :salary 1000000)
		:cfo (invent-employee :job-title "CFO"
				      :salary 500000)
		:coo (invent-employee :job-title "COO"
				      :salary 250000)
		:departments (make-array-filled-with (length *department-pool*)
						     'department
						     (invent-department 67))
		:peons *employee-array*))


(defbinary test-1 (:byte-order :dynamic)
  ((a b c) nil :type (bit-field
		      :raw-type (unsigned-byte 24)
		      :member-types ((unsigned-byte 8)
				     (unsigned-byte 8)
				     (unsigned-byte 8)))))

(defbinary test-2 (:byte-order :little-endian)
  (a #xabc :type (unsigned-byte 12))
  (b #xdef :type 12))

(in-package :lisp-binary)

(define-lisp-binary-type type-info (type)
  (if (gethash type *enum-definitions*)
      (values 'symbol
	      `(read-enum ',type ,stream-symbol)
	      `(write-enum ',type ,name ,stream-symbol))
      (values type
	      `(read-binary ',type ,stream-symbol)
	      `(write-binary ,name ,stream-symbol))))

(define-lisp-binary-type type-info (type &key reader writer (lisp-type t))
  :where (eq type 'custom)
  (documentation
   (custom
    "(CUSTOM &key reader writer (lisp-type t))

Specifies a slot of type LISP-TYPE that will be read by the provided
READER function and written with the provided WRITER function

The READER function must accept the lambda-list (STREAM), and its
argument will be the stream currently being read.

The WRITER function must accept the lambda-list (OBJECT STREAM), and
it is generally expected to write the OBJECT to the STREAM.

If these functions are specified as LAMBDA forms, then they will
be closures. The READER can expect every field that has been read
so far to be bound to their names, while the WRITER can expect
to be able to see all the slots in the struct being written.

Both functions are optional."))
  (values lisp-type
	  (if reader
	      `(funcall ,reader ,stream-symbol)
	      '(values nil 0))
	  (if writer
	      `(funcall ,writer ,name ,stream-symbol)
	      '(progn 0))))

(define-lisp-binary-type type-info (type &key raw-type member-types)
  :where (eq type 'bit-field)
  (documentation
   (bit-field
    "(BIT-FIELD &key raw-type member-types)

NOTE: Direct use of this type by programs is deprecated. Instead,
just declare clusters of fields with (UNSIGNED-BYTE n) or (SIGNED-BYTE n)
without worrying if n is a whole number of bytes. The DEFBINARY macro
will combine them into BIT-FIELD fields as long as there is a combination of
fields that add up to a whole number of bytes. 

Specifies that multiple values are to be OR'd into a single integer for serialization
purposes. The name of a slot of this type must be specified as a list of names,
one for each value in the bit field. :RAW-TYPE specifies the type of the single integer
into which everything is being stored, and must meet the following requirements:

1. Be of the form (UNSIGNED-BYTE n)
2. Where N is divisible by 8.

The :MEMBER-TYPES is an unevaluated list of types that must consist entirely of
(UNSIGNED-BYTE b) or (SIGNED-BYTE b) types. The Bs must add up to N above.

READ-BINARY will automatically separate the values in the bit field into their
slots, and WRITE-BINARY will automatically OR them back together.

The default value you specify for this field should be given as a list
of default values for each of the subfields."))


  (let ((reader* nil)
	(writer* nil))
    (letf (((slot-value type-info 'type) raw-type))
      (multiple-value-bind (real-raw-type reader writer)
	  (expand-defbinary-type-field type-info)
	(declare (ignore writer))
	(destructuring-case real-raw-type
	  ((kwtype (type bits)) :where (and (eq type 'unsigned-byte)
					    (integerp bits)
					    (eq kwtype :type))
	   (let ((temp-var (gensym "TEMP-VAR-"))
		 (bytes-read (gensym "BYTES-READ-"))
		 (signedness nil)
		 (field-sizes nil))
	     (loop for (member-type bits) in member-types
		do (push (eq member-type 'signed-byte)
			 signedness)
		  (push bits field-sizes))
	     (setf signedness (reverse signedness))
	     (setf field-sizes (reverse field-sizes))
	     (unless (= (apply #'+ field-sizes)
			bits)
	       (error "Member types ~S don't add up to ~S bits~%"
		      member-types bits))		     
	     (setf reader*
		   ;; FIXME: READER might not produce a BYTES-READ value if
		   ;;        it is supplied by the user via the :READER argument.
		   ;;        But it's unlikely that anyone will combine :READER
		   ;;        with BIT-FIELD.
		   `(multiple-value-bind (,temp-var ,bytes-read)
			,reader
		      (incf ,byte-count-name ,bytes-read)
		      (split-bit-field ,temp-var (list ,@field-sizes)
				       ',signedness)))
	     (setf writer*
		   `(write-integer (join-field-bits (list ,@field-sizes)
						    (list ,@signedness)
						    (list ,@name))
				   ,(/ (apply #'+ field-sizes) 8)
				   ,stream-symbol
				   :byte-order ,byte-order))
	     (cond ((eq byte-order '*byte-order*)
		    (setf reader* `(ecase *byte-order*
				     (:big-endian
				      (multiple-value-bind (,temp-var ,bytes-read)
					  ,reader
					(incf ,byte-count-name ,bytes-read)
					(split-bit-field ,temp-var (list ,@field-sizes)
							 ',signedness)))
				     (:little-endian
				      (multiple-value-bind (,temp-var ,bytes-read)
					  ,reader
					(incf ,byte-count-name ,bytes-read)
					(split-bit-field ,temp-var (list ,@(reverse field-sizes))
							 ',(reverse signedness))))))
		    (setf writer* `(ecase *byte-order*
				     (:little-endian
				      (write-integer
				       (join-field-bits (list ,@(reverse field-sizes))
							(list ,@(reverse signedness))
							(list ,@(reverse name)))
				       ,(/ (apply #'+ field-sizes) 8)
				       ,stream-symbol
				       :byte-order ,byte-order))
				     (:big-endian ,writer*))))
		   ((eq byte-order :little-endian)
		    (setf reader*
			  `(multiple-value-bind (,temp-var ,bytes-read)
			       ,reader
			     (incf ,byte-count-name ,bytes-read)
			     (split-bit-field ,temp-var (list ,@(reverse field-sizes))
					      ',(reverse signedness))))
		    (setf writer*
			  `(write-integer
			    (join-field-bits (list ,@(reverse field-sizes))
					     (list ,@(reverse signedness))
					     (list ,@(reverse name)))
			    ,(/ (apply #'+ field-sizes) 8)
			    ,stream-symbol
			    :byte-order ,byte-order))))
	     (values member-types
		     reader* writer*)))
	  (otherwise
	   (error "Invalid BIT-FIELD :RAW-TYPE value: ~S" raw-type)))))))

(define-lisp-binary-type type-info (type)
  :where (eq type 'base-pointer)
  (documentation
   (base-pointer "Instead of reading or writing this field, CL:FILE-POSITION will be called
on the current stream, and the address returned will be stored under a tag
with the same name as this slot. The tag can then be used to calculate
file positions and offsets. See the POINTER type for an example."))
  (values t
	  `(let ((file-position (file-position ,stream-symbol)))
	     (add-base-pointer-tag ',name file-position)
	     (values file-position 0))
	  `(progn
	     (setf ,name (file-position ,stream-symbol))
	     (add-base-pointer-tag ',name ,name)
	     0)))

(define-lisp-binary-type type-info (type)
  :where (eq type 'file-position)
  (documentation
   (file-position "FILE-POSITION

Like BASE-POINTER, but no global tag is stored. The slot will contain the
address in the file of the next thing to be read. No actual reading or
writing is triggered by a slot of this type.
"))
  (values 'integer
	  `(values (file-position ,stream-symbol)
		   0)
	  `(progn (setf ,name (file-position ,stream-symbol))
		  0)))

(define-lisp-binary-type type-info (type &key base-pointer-name)
  :where (eq type 'region-tag)
  (documentation
   (region-tag "(REGION-TAG &key base-pointer-name)

Instead of writing the value of this slot, all POINTERs that have the same
REGION-TAG name as this slot will be written out here, and the corresponding
offsets will be updated. The file being written must be opened with
:DIRECTION :IO. The POINTERs themselves will be written as offsets from
whatever object has the BASE-POINTER named BASE-POINTER-NAME."))
  (push name *ignore-on-write*)
  (values t
	  `(values nil 0)
	  `(dump-tag ',name ,(if base-pointer-name
				 `(get-base-pointer-tag ',base-pointer-name)
				 0)
		     ,stream-symbol)))

(define-lisp-binary-type type-info (type &key pointer-type data-type base-pointer-name region-tag validator)
  :where (eq type 'pointer)
  (documentation
   (pointer "(POINTER &key pointer-type data-type base-pointer-name region-tag)

Specifies that the value is really a pointer to another value somewhere else
in the file. When reading, if a BASE-POINTER-NAME is supplied and a base-pointer
tag has been created, then the pointer will be treated as an offset from that
base-pointer. If no BASE-POINTER-NAME is provided, then the pointer is treated
as being an absolute file-position.

The :POINTER-TYPE key specifies the type of the pointer itself, and must be some kind
of integer.

The :DATA-TYPE specifies the data that is being pointed to.

The :REGION-TAG is used when writing. When WRITE-BINARY writes this field, what
it really does is just write a zero pointer (since the object being pointed to
proably occurs later in the file, so we don't know what the address is going to
be yet). Then WRITE-BINARY stores the address OF THE POINTER, along with a
serialized representation  of the data to be written.  

When any WRITE-BINARY method gets to a REGION-TAG field, it writes out all the data
that has been stored under that tag's name, and goes back to update the pointers.

POINTERs cannot be automatically written if they point to an earlier part of the file
than they themselves occur (no backwards-pointing pointers).

Because it must go back and change what it has previously written, the stream must
be opened with :DIRECTION :IO.

All I/O involving POINTERs, REGION-TAGs, or BASE-POINTERs should be performed
within a WITH-LOCAL-POINTER-RESOLVING-CONTEXT block.

Example:

    (defbinary bar ()
      (pointer-1 nil :type (pointer :pointer-type (unsigned-byte 16)
    				:data-type  (terminated-string 1)
    				:base-pointer-name foo-base
    				:region-tag foo-region))
      (pointer-2 0   :type (pointer :pointer-type (unsigned-byte 16)
    				:data-type quadruple-float
    				:base-pointer-name foo-base
    				:region-tag foo-region)))
    
    
    (defbinary foo ()
      (foo-base 0 :type base-pointer)
      (bar nil :type bar)
      ;; POINTER-1 and POINTER-2 will point to this:
      (foo-region nil :type (region-tag :base-pointer-name foo-base)))
    
    
    (with-local-pointer-resolving-context
     (let ((input (with-open-binary-file (in \"foo.bin\")
    				     (read-binary 'foo in))))
       (with-open-binary-file (out \"bar.bin\"
    			       :direction :io)
    			  (write-binary input stream))))
"))
  (block nil
    (letf (((slot-value type-info 'type) pointer-type))
      (multiple-value-bind (pointer-defstruct-type pointer-reader pointer-writer)
	  (expand-defbinary-type-field type-info)
	(declare (ignore pointer-defstruct-type)
		 (optimize (speed 0) (debug 3)))
	(letf (((slot-value type-info 'type) data-type))
	  (multiple-value-bind (defstruct-type data-reader data-writer)
	      (expand-defbinary-type-field type-info)
	    (values (getf defstruct-type :type)
		    (alexandria:with-gensyms (pointer-value base-pointer-address pointer-bytes-read
							    pbr2 pv2)
		      `(let* ((,pointer-bytes-read nil)
			      (,base-pointer-address ,(if base-pointer-name
							  `(get-base-pointer-tag ',base-pointer-name)
							  0))
			      (,pointer-value (+ ,base-pointer-address
						 (multiple-value-bind (,pv2 ,pbr2)
						     ,pointer-reader
						   (setf ,pointer-bytes-read ,pbr2)
						   ,pv2))))
			 (restart-case
			     (progn
			       ,@(if validator
				     `((funcall ,validator ,pointer-value)))
			       (with-file-position (,pointer-value ,stream-symbol)
				 (values ,data-reader ,pointer-bytes-read)))
			   (use-value (value)
			     :report ,(format nil "Provide a value of type ~S" data-type)
			     :interactive (lambda ()
					    (format t "Enter a value of type ~S: " ',data-type)
					    (force-output)
					    (list (eval (read))))
			     (values value 0)))))
		    (alexandria:with-gensyms (write-data write-pointer pointer-position)
		      `(let ((,name ,name))
                         (flet ((,write-data (,stream-symbol) ,data-writer)
                                (,write-pointer (,name ,stream-symbol) ,pointer-writer))
                           (queue-write-pointer ',region-tag
                                                (let ((,pointer-position (file-position ,stream-symbol)))
                                                  (lambda (,name ,stream-symbol)
                                                    (with-file-position (,pointer-position ,stream-symbol)
                                                      (,write-pointer ,name ,stream-symbol))))
                                                #',write-data)
                           (,write-pointer 0 ,stream-symbol)))))))))))

(define-lisp-binary-type type-info (type &key (actual-type '(unsigned-byte 16)) (value 0))
  :where (eq type 'magic)
  (documentation
   (magic "(MAGIC &key actual-type value)

Specifies that a magic value will be read and written. The value will be
read as type ACTUAL-TYPE.

If the value read is not CL:EQUAL to the VALUE given, then a condition of type
BAD-MAGIC-VALUE will be raised.

A BAD-MAGIC-VALUE object contains the slots BAD-VALUE and REQUIRED-VALUE.

The error can be ignored by invoking the CL:CONTINUE restart."))
  (if (and (listp actual-type)
	   (eq (car actual-type) 'quote))
      (restart-case
	  (error ":ACTUAL-TYPE ~S should not be quoted" actual-type)
	(unquote-it ()
	  :report "Well, unquote it, then!"
	  (setf actual-type (cadr actual-type)))))
  (letf (((slot-value type-info 'type)
	  actual-type))	      
    (multiple-value-bind (defstruct-type reader writer)
	(expand-defbinary-type-field type-info)
      (values
       (getf defstruct-type :type)
       (let ((v (gensym "READER-"))
	     (bytes-read (gensym "BYTES-READ-"))
	     (required-value (gensym "REQUIRED-VALUE-")))
	 `(let ((,required-value ,value))
	    (multiple-value-bind (,v ,bytes-read) ,reader
	      (unless (equal ,v ,required-value)
		(restart-case
		    (error 'bad-magic-value :bad-value ,v
			   :required-value ,required-value
			   :format-control
			   "Invalid magic number: ~a (expected: ~a)~%"
			   :format-arguments (list ,v ,required-value))
		  (continue ()
		    :report "Ignore the error and continue loading"
		    nil)))
	      (values ,v ,bytes-read))))
      `(progn
	 (setf ,name ,value)
	 ,writer)))))

(define-lisp-binary-type type-info (type length &key (external-format :latin1) (padding-character #\Nul))
  :where (member type '(fixed-length-string fixed-string))
  (documentation
   ((fixed-length-string fixed-string)
    "(FIXED-LENGTH-STRING length &key (external-format :latin1) (padding-character #\Nul))

Specifies a string of fixed length. When writing, any excess space
in the string will be padded with the PADDING-CHARACTER. The LENGTH is the
number of bytes desired after encoding.

If the input string is longer than the provided LENGTH, a condition of type
LISP-BINARY:INPUT-STRING-TOO-LONG will be raised. Invoke the restart CL:TRUNCATE
to trim enough excess characters from the string to make it equal to the LENGTH."))
  (values 'string
	  (let ((bytes (gensym "BYTES-"))
		(bytes* (gensym "BYTES*-"))
		(buffer (gensym "BUFFER-")))
		    
	    `(let ((,bytes nil))
	       (values
		(string-right-trim
                 (list ,padding-character)
                 (octets-to-string
                  (multiple-value-bind (,buffer ,bytes*)
                      (read-bytes ,length ,stream-symbol)
                    (setf ,bytes ,bytes*)
                    ,buffer)
                  :external-format ,external-format))
		,bytes)))
	  `(write-bytes
	    (make-fixed-length-string ,name ,length ,external-format ,padding-character)
	    ,stream-symbol)))

(define-lisp-binary-type type-info (type count-size &key (external-format :latin1))
  :where (member type '(counted-string counted-buffer))
  (documentation
   ((counted-string counted-buffer)
    "(COUNTED-STRING count-size-in-bytes &key (EXTERNAL-FORMAT :latin1))
(COUNTED-BUFFER count-size-in-bytes)

These are like COUNTED-ARRAYS, but their elements are one byte long. Furthermore, a
COUNTED-STRING will be encoded or decoded into a Lisp string according to its EXTERNAL-FORMAT.

The encoding/decoding is done using the FLEXI-STREAMS library, and valid EXTERNAL-FORMATs are those
that are accepted by FLEXI-STREAMS:OCTETS-TO-STRING.

Example:

    (defbinary foobar ()
      (str \"\" :type (counted-string 2 :external-format :utf8)))"))
  (values (ecase type
	    ((counted-string) 'string)
	    ((counted-buffer) '(simple-array (unsigned-byte 8))))
	  (let ((read-form `(read-counted-string ,count-size ,stream-symbol
						 :byte-order ,byte-order)))
	    (ecase type
	      ((counted-string) (alexandria:with-gensyms (value bytes)
				  `(multiple-value-bind (,value ,bytes) ,read-form
				     (values
				      (octets-to-string ,value :external-format ,external-format)
				      ,bytes))))
	      ((counted-buffer) read-form)))
	  `(write-counted-string ,count-size ,stream-symbol
				 ,(ecase type
				    ((counted-string)
				     `(string-to-octets ,name :external-format ,external-format))
				    ((counted-buffer)
				     name))
				 :byte-order ,byte-order)))

(define-lisp-binary-type type-info (counted-array count-size element-type &key bind-index-to)
  :where (eq counted-array 'counted-array)
  (documentation
   (counted-array
    "(COUNTED-ARRAY count-size-in-bytes element-type &key bind-index-to)

This is a SIMPLE-ARRAY preceded by an integer specifying how many
elements are in it.

Example:

    (read-binary-type '(counted-array 2 (unsigned-byte 8)) stream)

The COUNT-SIZE-IN-BYTES does not have to be an integer. It can also be a
fraction, which will trigger non-byte-aligned I/O. (example, if the size is 1/2, then the count
is 4 bits wide, and the first element begins halfway into the same byte as the count) The
ELEMENT-TYPE can also be non-byte-aligned. Such a type can only be read from or written
to a BIT-STREAM.

See also: WITH-WRAPPED-IN-BIT-STREAM"))
  (let ((count-size* (gensym "COUNT-SIZE-"))
	(reader-value  (gensym "READER-VALUE-"))
	(reader-byte-count (gensym "READER-BYTE-COUNT-")))
    (letf (((slot-value type-info 'type)
	    `(simple-array ,element-type ((read-integer ,count-size* ,stream-symbol :byte-order ,byte-order))
			   ,@(if bind-index-to					 
				 `(:bind-index-to ,bind-index-to)))))
      (multiple-value-bind (defstruct-type reader writer)
	  (expand-defbinary-type-field type-info)
	(setf writer `(let ((,count-size* ,count-size))
			(+
			 (write-integer (length ,name) ,count-size* ,stream-symbol :byte-order ,byte-order)
			 ,writer)))
	(setf reader `(let ((,count-size* ,count-size))
			(multiple-value-bind (,reader-value ,reader-byte-count) ,reader
			  (values ,reader-value (+ ,count-size* ,reader-byte-count)))))
	(values (getf defstruct-type :type) reader writer)))))

(define-lisp-binary-type type-info (simple-array type lengths &key bind-index-to)
  :where (member simple-array '(simple-array))
  (documentation
   (simple-array "(SIMPLE-ARRAY element-type (size))

Example:

    (defbinary foobar ()
      (size 0 :type (unsigned-byte 16))
      (arr #() :type (simple-array (unsigned-byte 8) (size))))

For the element type, any real or virtual type supported by DEFBINARY is allowed.
The SIZE is a Lisp expression that will be evaluated in an environment where all
previous members of the struct are bound to their names.

DEFBINARY will read and write all other CL objects using their READ-BINARY and
WRITE-BINARY methods, if defined."))
  (let ((array-count-size nil)
	(reader* nil)
	(writer* nil))
    (unless (listp lengths)
      (restart-case
	  (error "Invalid simple-array type (~a ~a ~a): ~a should be a list."
		 simple-array type lengths lengths)
	(dwim ()
	  :report "Assume a one-dimensional array."
	  (setf lengths (list lengths)))))
    (unless (= (length lengths) 1)
      (error "Invalid simple-array type (SIMPLE-ARRAY ~a ~a): DEFBINARY only supports 1-dimensional arrays."
	     type lengths))
    (let ((length (car lengths))
	  (name-one (gensym "NAME-ONE-"))
	  (buffer (gensym "BUFFER-"))
	  (next-value (gensym "NEXT-VALUE-"))
	  (bytes (gensym "BYTES-"))
	  (local-byte-count (gensym "LOCAL-BYTE-COUNT-"))
	  (ix (gensym "IX-")))
      (flet ((maybe-add-align (form readp)
	       (let ((move-op (if readp
				  '#'read-bytes
				  '(lambda (bytes stream)
				    (loop repeat bytes
				       do (write-byte 0 stream))))))
		 (if element-align
		     `(progn (incf ,local-byte-count (align-to-boundary (+ ,byte-count-name ,local-byte-count)
									,element-align ,move-op ,stream-symbol))
			     ,form)
		     form)))
	     (maybe-bind-index (form)
	       (if bind-index-to
		   `(let ((,bind-index-to ,ix))
		      (declare (ignorable ,bind-index-to))
		      ,form)
		   form)))
	;; Turn alignment off for subtypes. That way if we're reading an
	;; array of array, our alignment doesn't spread to the next level down.
	;; For example, if ELEMENT-ALIGN is 64 right now and we're reading an
	;; array of strings, that means that each string is meant to be 64-byte
	;; aligned. If the alignment is passed further down, each CHARACTER will
	;; be 64-byte aligned.
	(letf (((slot-value type-info 'align) nil)
	       ((slot-value type-info 'element-align) nil)
	       ((slot-value type-info 'type) type)
	       ((slot-value type-info 'byte-count-name) local-byte-count)
	       ((slot-value type-info 'name) name-one))
	  (multiple-value-bind (defstruct-type read-one write-one)
	      (expand-defbinary-type-field type-info)
	    (setf reader*
		  `(let ((,buffer (make-array ,length :element-type ',(cadr defstruct-type)))
			 (,local-byte-count 0))
		     ;; FIXME: This code needs to be adapted to support
		     ;;        terminated arrays. The only difference
		     ;;        in the terminated case is that the loop
		     ;;        termination condition would be based on
		     ;;        a predicate instead of the length of the
		     ;;        buffer. The problem is there's no good
		     ;;        way to tell this code to expand the
		     ;;        terminated case instead of the fixed-length
		     ;;        case.
		     ;;
		     ;;  It would also be useful if this code could have a
		     ;;  third case in which it somehow produced a lazy
		     ;;  sequence instead of an array. Specifically, this
		     ;;  would be applicable to cases where data should
		     ;;  be handled in a streaming manner. Then, client
		     ;;  code would only need to force each next element
		     ;;  when needed, and it would be read at that time.
		     ;;
		     ;;  Ideally, the decision about whether to read in
		     ;;  a streaming fashion or all at once could be made
		     ;;  at runtime. That would require a totally different
		     ;;  expansion here.
		     ;;
		     ;; NOTE: Do not use CLAZY to implement lazy lists.
		     ;;       This library is incredibly SLOW! Perhaps
		     ;;       this is an inherent property of the whole
		     ;;       delay/force paradigm.
		     ;;
		     ;;       In fact, ANY SRFI-41-like stream facility
		     ;;       in which promises are implemented as
		     ;;       closures will have the same performance
		     ;;       problems in SBCL, which is supposed to be
		     ;;       the fastest Lisp.
		     ;;
		     ;;       Better to implement the stream as a closure
		     ;;       that simply closes over the file stream,
		     ;;       and reads one element each time it's called.
		     ;;
		     ;; NOTE2: Racket's implementation of SRFI-41 streams
		     ;;        is very fast. I wonder what the difference is?
		     ;;
		     ;; NOTE3: My CL implementation of SRFI-41 ported to
		     ;;        Racket is very fast, even though I copy
		     ;;        the use of closures to implement promises!
		     ;;        WTF?! The difference is entirely in GC time.
		     ;;
		     ;; NOTE4: Creating a simple one-closure counter is
		     ;;        far more efficient than creating a lazy
		     ;;        infinite sequence that creates a new closure with
		     ;;        each new number generated.
		     ;;
		     (loop for ,ix from 0 below ,(if array-count-size
						     `(multiple-value-bind (array-size bytes-read)
							  (read-integer ,array-count-size ,stream-symbol
									:byte-order ,byte-order)
							(incf ,local-byte-count bytes-read)
							array-size)
						     `(length ,buffer)) do				  
			  (multiple-value-bind (,next-value ,bytes)
			      (restart-case
				  ,(maybe-bind-index (maybe-add-align read-one t))
				(use-value (val)
				  :report ,(format nil "Enter a new value for the next element of ~a" name)
				  :interactive (lambda ()
						 (format t "Enter a new value of type ~a (evaluated): " ',(cadr defstruct-type))
						 (list (eval (read))))
				  (values val 1)))
			    (setf (aref ,buffer ,ix) ,next-value)
			    (incf ,local-byte-count ,bytes)))
		     (values ,buffer ,local-byte-count)))
	    (setf writer*
		  `(let ((,local-byte-count 0))
		     (loop for ,ix from 0 below (length ,name)
			do (incf ,local-byte-count ,(maybe-bind-index
						     (maybe-add-align
                                                      `(symbol-macrolet ((,name-one (aref ,name ,ix))) ,write-one) nil))))
		     ,local-byte-count))
	    (values `(simple-array ,(cadr defstruct-type))
		    reader* writer*)))))))

(define-lisp-binary-type type-info (type-name type-generating-form)
  :where (eq type-name 'eval)
  (documentation
   (eval "(EVAL unquoted-type-expression)

The EVAL type specifier causes the type to be computed at runtime. The
UNQUOTED-TYPE-EXPRESSION will be evaluated just before attempting to read
the field of this type in an environment where all the previously-defined
fields are bound to their names.

Example:

    (defbinary foobar ()
      (type-tag 0 :type (unsigned-byte 32))
      (data nil :type (eval
		       (case type-tag
			 (1 '(unsigned-byte 16))
			 (2 '(counted-string 1 :external-format :utf-8))))))

In the above example, READ-BINARY will first read the TYPE-TAG as an
(UNSIGNED-BYTE 32), then it will evaluate the CASE expression in order to
get the type of the DATA. Then, because of a special optimization that has
been applied to this type when a CASE form is presented to it, the bodies
of the cases are evaluated at macro-expansion time, and their values
used to generate the code to read and write the corresponding types. A
similar case form is compiled into the READ-BINARY and WRITE-BINARY
methods, with the types being replaced by reader or writer code. If this
mechanism fails, a fallback mechanism, described below, is used.

The following forms support this optimization:

   case ecase typecase etypecase cond

If any other form is provided, then the EVAL expander resorts to its
default behavior, which is to pass whatever type is produced by the form
to an internal function in order to generate the code that will be used
to perform the reading or writing, and then to EVAL the resulting form.

The same thing happens if any of the bodies of the cases signals a
condition at macro expansion time, which could be an indication that
the case bodies require information that is only available at runtime.

The CASE expression is not actually evaluated with a runtime call
to EVAL. Instead, it is embedded directly in the source code of the
generated READ-BINARY and WRITE-BINARY methods.

If the UNQUOTED-TYPE-EXPRESSION evaluates to another EVAL type specifier,
then that specifier will be expanded once again. The EVAL expander
tries to be as smart as possible to avoid actually calling EVAL at
runtime, but sometimes it's unavoidable.
"))
  (let ((case-template nil)
	(readers nil)
	(writers nil))
    ;; The following implements an optimization for a common case:
    ;; If the EVAL expression is a CASE, ECASE, TYPECASE, or ETYPECASE,
    ;; it may be possible to replace the types that these forms return
    ;; with the reader/writer code that each type would expand to. For
    ;; example, if you specify this type:
    ;;
    ;; (eval (case foo
    ;;         (1 'counted-string)
    ;;         (2 '(unsigned-byte 16))))
    ;;
    ;; ...without the optimization, it would expand to:
    ;;
    ;; Reader: (read-binary-type (case foo ...))
    ;; Writer: (write-binary-type (case foo ...))
    ;;
    ;; ...and then at runtime, READ-BINARY-TYPE/WRITE-BINARY-TYPE will
    ;;    create a reader or writer form from the type and EVAL it.
    ;;
    ;; But with the optimization, the CASE form expands to:
    ;;
    ;; Reader: (case foo
    ;;            (1 (read-binary 'counted-string #:stream-symbol))
    ;;            (2 (read-integer 2 #:stream-symbol :byte-order :little-endian
    ;;                    :signed nil :signed-representation :twos-complement)))
    ;; Writer: (case foo
    ;;            (1 (write-binary bar #:stream-symbol))
    ;;            (2 (write-integer bar 2 #:stream-symbol :byte-order ...)))
    ;;
    ;; ...which skips the runtime EVAL and is therefore more efficient.
    
    (setf case-template
	  (block make-case-template
	    (if (member (car type-generating-form) '(case ecase typecase etypecase cond))
		`(,(first type-generating-form)
		   ,(second type-generating-form)
		   ,@(loop for (case . body) in (cddr type-generating-form)
			collect (handler-case
				    (letf (((slot-value type-info 'type)
					    (eval `(progn ,@body))))
				      (multiple-value-bind (type reader writer)
					  (expand-defbinary-type-field type-info)
					(declare (ignore type))
					(let ((placeholder (gensym)))
					  (push (cons placeholder reader) readers)
					  (push (cons placeholder writer) writers)
					  (list case placeholder))))
				  (t ()
				    ;; Most likely error: The form in question has something in
				    ;; it that would only work with an EVAL at read or write time,
				    ;; not at compile time. So we just return the unoptimized FORM,
				    ;; then.
				    (return-from make-case-template nil))))))))
    (labels ((fill-template (expansions)
	       (list* (first case-template)
		      (second case-template)
		      (loop for (case placeholder) in (cddr case-template)
			 collect (list case (cdr (assoc placeholder expansions)))))))
      (if case-template
	  (values t (fill-template readers) (fill-template writers))
	  
	  (values
	   t
	   `(read-binary-type ,type-generating-form ,stream-symbol :byte-order ,byte-order
			      :align ,align :element-align ,element-align)
	   `(write-binary-type ,name ,type-generating-form ,stream-symbol
			       :byte-order ,byte-order :align ,align
			       :element-align ,element-align))))))

(define-lisp-binary-type type-info (byte-type bits &key (signed-representation :twos-complement))
  :where (member byte-type '(unsigned-byte signed-byte))
  (documentation
   ((unsigned-byte signed-byte)
    "(UNSIGNED-BYTE n) and (SIGNED-BYTE n &key (signed-representation :twos-complement), 
where N is the number of bits. Since these types are used so frequently in DEFBINARY
structs, there is a shorthand for them: You can simply use the number of bits as the
type. Positive for unsigned, and negative for signed (two's complement only).

Example:

    (defbinary foobar ()
      (x 0 :type 16)  ;; 16-bit unsigned
      (y 1 :type -16)) ;; 16-bit signed

If you need to read one's complement, it must be written out:

    (defbinary foobar ()
      (x 0 :type (signed-byte 16 :signed-representation :ones-complement)))"))
  (values `(,byte-type ,bits)
	  `(read-integer ,(/ bits 8) ,stream-symbol
			 :byte-order ,byte-order
			 :signed ,(eq byte-type 'signed-byte)
			 :signed-representation ,signed-representation)
	  `(write-integer ,name ,(/ bits 8) ,stream-symbol
			  :byte-order ,byte-order
			  :signed-representation ,signed-representation
			  :signed ,(eq byte-type 'signed-byte))))

(define-lisp-binary-type type-info (float-type &key (byte-order byte-order))
  :where (member float-type '(float single-float half-float double-float quadruple-float quad-float
			      octuple-float octo-float))
  (documentation
   ((float single-float)
    "IEEE Single Precision")
   (double-float "IEEE Double Precision")
   (half-float "Read and written as IEEE half-precision, stored in memory as single-precision")
   ((quadruple-float quad-float)
    "Read and written as IEEE quadruple precision, stored in memory as CL:RATIONAL
to avoid loss of prcision.")
   ((octuple-float octo-float)
    "Read and written as IEEE octuple precision, stored in memory as CL:RATIONAL
to avoid loss of precision."))
  (let ((float-format (case float-type
			(half-float :half)
			((float single-float) :single)
			(double-float :double)
			((quadruple-float quad-float) :quadruple)
			((octuple-float octo-float) :octuple)))
	(float-type (case float-type
		      ((float single-float double-float)
		       float-type)
		      (half 'single-float)
		      #+clisp ((quadruple-float quad-float
						octuple-float octo-float) 'long-float)
		      (otherwise 'number))))
    (values float-type
	    `(read-float ,float-format :stream ,stream-symbol :byte-order ,byte-order
			 :result-type ',float-type)
	    `(write-float ,float-format ,name :stream ,stream-symbol
			  :byte-order ,byte-order))))

(define-lisp-binary-type type-info (null-type)
  :where (eq null-type 'null)
  (documentation
   (null "Reading and writing will be a no-op. The value of a field of type NULL will always read
as NIL and be ignored when writing."))
  (values 'null
	  `(progn (values nil 0))
	  `(progn 0)))

(define-lisp-binary-type type-info (type termination-length &key (external-format :latin1) (terminator 0))
  :where (member type '(terminated-string terminated-buffer))
  (documentation
   ((terminated-string terminated-buffer)
    "(TERMINATED-STRING termination-length &key (terminator 0) (extenal-format :latin1))
(TERMINATED-BUFFER termination-length &key (terminator 0))

Specifies a C-style terminated string. The TERMINATOR is an integer that will be en/decoded
according to the field's BYTE-ORDER. As such, it is capable of being more than one byte long,
so it can be used to specify multi-character terminators such as CRLF."))
  (let ((real-terminator `(ecase ,byte-order
			    ((:little-endian) (encode-lsb (or ,terminator 0) ,termination-length))
			    ((:big-endian) (encode-msb (or ,terminator 0) ,termination-length)))))
    (let ((reader `(read-terminated-string ,stream-symbol :terminator ,real-terminator))
	  (name (ecase type
		  ((terminated-string) `(string-to-octets ,name :external-format ,external-format))
		  ((terminated-buffer) name))))
      (if (eq type 'terminated-string)
	  (setf reader `(read-octets-to-string ,reader :external-format ,external-format)))
      (values 
       (ecase type
	 ((terminated-string) 'string)
	 ((terminated-buffer) '(simple-array (unsigned-byte 8))))
       reader
       `(write-terminated-string ,name ,stream-symbol :terminator ,real-terminator)))))

(define-lisp-binary-type type-info (type)
  :where (integerp type)
  ;; Let a positive numeric type n be shorthand for (unsigned-byte n),
  ;; and a negative one be shorthand for (signed-byte -n).
  (setf (slot-value type-info 'type)
	(cond ((> type 0)
	       `(unsigned-byte ,type))
	      ((< type 0)
	       `(signed-byte ,(- type)))
	      (t (restart-case
		     (error "0 is not a valid type.")
		   (unsigned-byte-8 ()
		     :report "Use (UNSIGNED-BYTE 8)"
		     '(unsigned-byte 8))
		   (signed-byte-8 ()
		     :report "Use (SIGNED-BYTE 8)"
		     '(signed-byte 8))
		   (specify (new-type)
		     :report "Specify a type to use."
		     :interactive (lambda ()
				    (format t "Specify a type to use (unevaluated): ")
				    (force-output)
				    (list (read)))
		     new-type)))))
  (multiple-value-bind (defstruct-type reader writer)
      (expand-defbinary-type-field type-info)
    (values (getf defstruct-type :type) reader writer)))


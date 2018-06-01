;; -*- Syntax: Common-Lisp; Package: Lisp-Binary -*-

(cl:defpackage lisp-binary
  (:use :closer-common-lisp :flexi-streams :quasiquote-2.0 :lisp-binary-utils :lisp-binary/float :lisp-binary/integer
	:simple-bit-stream)
  (:documentation "Read binary data directly into structs, and write it back out again. Also provides a lower-level API for
reading and writing integers and floating-point numbers. Also provides a bit-stream API.")
  (:export :get-lsb-byte :encode-lsb :decode-lsb :decode-msb :encode-msb :decode-ip-addr
	   :read-bytes :write-bytes
	   :read-integer :write-integer :read-counted-string :read-file :write-counted-string
	   :integer-value :symbol-value :enum-name :bad-enum-value
	   :read-bits
	   :write-bits
	   :read-float
	   :write-float
	   
	   :defbinary

	   :*byte-order*
	   :base-pointer
	   :region-tag
	   :pointer
	   :with-local-pointer-resolving-context
	   :custom
	   :read-binary :write-binary

	   :read-binary-type
	   :write-binary-type

	   :wrap-in-bit-stream
	   :with-wrapped-in-bit-stream
	   :with-buffered-output

	   :pad-fixed-length-string
	   :input-string-too-long
	   :read-terminated-string :write-terminated-string :buffer :terminated-string :terminated-buffer
	 :counted-string :counted-buffer :counted-array :define-enum :read-enum :write-enum :magic :bad-magic-value
	 :bad-value :required-value :fixed-length-string :fixed-string :bit-field :open-binary :with-open-binary-file :use-string-value
	 :half-float
	 :single-float
	 :double-float
	 :quadruple-float
	 :octuple-float))
	 
(in-package :lisp-binary)

;; (declaim (optimize (debug 0) (speed 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-quasiquote-2.0)
  (defvar *known-defbinary-types* nil
    "An alist whose keys are the names of DEFBINARY structs and
whose values are the field-descriptions of those structs. Needed
by TYPE-SIZE")
  (defvar *ignore-on-write* nil)
  (defun get-type-fields (type-name)
    (cdr (assoc type-name *known-defbinary-types*))))

(enable-quasiquote-2.0)

(defun open-binary (pathname &key (direction :input)
			       if-exists if-does-not-exist)
  "Like OPEN, but always opens a binary stream suitable for use with the DEFBINARY library."
  (apply #'open `(,pathname :direction ,direction :element-type (unsigned-byte 8)
			   ,@(if if-exists
				 `(:if-exists ,if-exists))
			   ,@(if if-does-not-exist
				 `(:if-does-not-exist ,if-does-not-exist)))))

(defmethod read-bytes (n (stream bit-stream) &key (element-type
						   `(unsigned-byte ,(slot-value stream 'simple-bit-stream::element-bits))))
  (declare (optimize (speed 0) (debug 3)))
  (cond ((and (integerp n)
	      (= (slot-value stream 'simple-bit-stream::bits-left) 0))
	 (read-bytes n (slot-value stream 'simple-bit-stream::real-stream) :element-type element-type))
	((and (listp element-type)
	      (= (length element-type) 2)
	      (numberp (second element-type)))
	 (let ((bits-per-byte (second element-type)))
	   (letf (((slot-value stream 'simple-bit-stream::element-bits)
		   bits-per-byte))
	     (multiple-value-bind (buffer partial-byte partial-bits)
		 (simple-bit-stream:read-bytes-with-partial stream (* n bits-per-byte))
	       (let ((original-length (length buffer)))
		 (when (> partial-bits 0)
		   (vector-push-extend partial-byte buffer))
		 (values buffer  (+ original-length (/ partial-bits bits-per-byte))))))))
	(t (error "Can't read ~a bytes of type ~S from a ~S" n element-type 'bit-stream))))

(defmethod write-bytes (buffer (stream bit-stream) &optional bytes)
  (setf bytes (or bytes (length buffer)))
  (if (and (integerp bytes)
	   (byte-aligned-p stream))
      (write-bytes buffer (slot-value stream 'simple-bit-stream::real-stream)
		   bytes)
      (multiple-value-bind (whole-bytes
			    partial-byte) (floor bytes)
	(let ((partial-bits (* partial-byte 8)))
	  (when (> whole-bytes 0)
	      (write-sequence buffer stream :end (1- whole-bytes)))
	  (when (> partial-bits 0)
	    (write-bits (aref buffer (1- (length buffer)))
			partial-bits stream))
	  bytes))))

;; TODO: Test and debug various combinations of :byte-order between
;;       WRITE-INTEGER and WITH-WRAPPED-IN-BIT-STREAM, which supports
;;       its own :BYTE-ORDER argument, which gets stored in the bitstream.


(defmacro with-open-binary-file ((stream filespec &rest options
					 &key (direction :input) if-exists if-does-not-exist)
				 &body body)
  "Like WITH-OPEN-FILE, but always opens the file as a binary file."
  `(with-open-file (,stream ,filespec ,@options :direction ,direction
			    :element-type '(unsigned-byte 8)
			    ,@(if if-exists
				  `(:if-exists ,if-exists))
			    ,@(if if-does-not-exist
				  `(:if-does-not-exist ,if-does-not-exist)))
     ,@body))

(defmacro with-buffered-output ((var stream) &body body)
  "Creates buffered output stream VAR. Data written to VAR will be written to STREAM after the BODY returns.
This makes it possible to write binaries that require streams opened in :DIRECTION :IO to streams that are
open in :DIRECTION :OUT"
  (let ((buffer (gensym "buffer-")))
    `(let ((,buffer (flexi-streams:with-output-to-sequence (,var :element-type '(unsigned-byte 8))
		      ,@body)))
       (write-bytes ,buffer ,stream))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *enum-definitions* (make-hash-table :test 'eq))
  
  (defstruct enum-definition
    (name nil :type symbol)
    (size 0 :type number)
    (signed nil :type boolean)
    (byte-order :little-endian :type keyword)
    (variables nil :type list)))

(defun get-enum-definition (symbol)
  (gethash symbol *enum-definitions* nil))


(defun enump (symbol)
  (and (get-enum-definition symbol)
       t))

(defmacro define-enum (name size (&key signed (byte-order :little-endian)) &rest values)
  "Define an enum type. What this does is allow LISP-BINARY to automatically map between
keywords and integer values that are expected to be found in a binary file. The SIZE is in bytes.

Example:

    (define-enum speeds 2 ()
       slow                 ;; Implicitly 0
       light-speed          ;; Implicitly 1
       (ridiculous-speed 5) ;; Explicit value
       ludicrous-speed)     ;; Implicitly 6
"

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((definition (make-enum-definition :size ,size
					     :name ',name
					     :byte-order ,byte-order
					     :signed ,signed
					     :variables ,(let ((counter -1))
							      `(list ,@(loop for val in values
									  if (listp val)
									  do (setf counter (- (second val) 1))
									     (setf val (car val))
									  collect `(cons ',val ,(incf counter))))))))
       (setf (gethash ',name *enum-definitions*) definition)
       (deftype ,name () 'symbol))))

(simple-define-condition bad-enum-value (simple-error) (integer-value symbol-value enum-name))
(simple-define-condition bad-magic-value (simple-error) (bad-value required-value))

(defun get-enum-value (enum symbol)
  (let ((definition (if (enum-definition-p enum)
			enum
			(gethash enum *enum-definitions*))))    
    (cdr (assoc symbol (slot-value definition 'variables)))))

(defun get-enum-name (enum value)
  "Returns the name that corresponds to the VALUE in the given ENUM."
  (let ((definition (if (enum-definition-p enum)
			enum
			(gethash enum *enum-definitions*))))
    (car (assoc-cdr value (slot-value definition 'variables)))))

(defparameter *byte-order* :little-endian)

(defun write-enum (enum symbol stream)
  (let ((enum-def (if (enum-definition-p enum)
		       enum
		       (gethash enum *enum-definitions*))))
    (write-integer (get-enum-value enum-def symbol)
		   (slot-value enum-def 'size)
		   stream
		   :byte-order (let ((byte-order (slot-value enum-def 'byte-order)))
				 (if (eq byte-order :dynamic)
				     *byte-order*
				     byte-order))
		   :signed (slot-value enum-def 'signed))))

(defun read-enum (enum stream)
  (let ((enum-def (if (enum-definition-p enum)
		      enum
		      (gethash enum *enum-definitions*))))
    (multiple-value-bind (raw-value bytes-read)
	(read-integer (slot-value enum-def 'size) stream
		      :byte-order (let ((byte-order (slot-value enum-def 'byte-order)))
				    (if (eq byte-order :dynamic)
					*byte-order*
					byte-order))
		      :signed (slot-value enum-def 'signed))
      (values
       (or (get-enum-name enum-def raw-value)
	   (error 'bad-enum-value
		  :integer-value raw-value
		  :symbol-value nil
		  :enum-name (slot-value enum-def 'name)
		  :format-control "Value ~a is not a value in enum ~a"
		  :format-arguments (list raw-value (slot-value enum-def 'name))))
       bytes-read))))

(defun decode-ip-addr (raw-msb)
  (declare (type (array (unsigned-byte 8) (4)) raw-msb))
  (with-output-to-string (*standard-output*)
    (format t "~{~a~^.~}" (loop for byte across raw-msb collect byte))))


(defun buffer (&rest elements)
  (make-array (length elements) :element-type '(unsigned-byte 8)
	      :initial-contents elements))

(defmacro read-octets-to-string (read-form &rest options)
  (let ((read-result (gensym "READ-RESULT"))
	(bytes-read (gensym "BYTES-READ")))
    `(multiple-value-bind (,read-result ,bytes-read) ,read-form
       (values (octets-to-string ,read-result ,@options)
	       ,bytes-read))))


(defun read-counted-string (size-len stream &key (byte-order :little-endian))
  "Reads an unsigned integer of SIZE-LEN bytes in the specified BYTE-ORDER,
then reads a byte vector of SIZE-LEN bytes. Returns two values:

1. The byte vector
2. The total number of bytes that were read."


  (multiple-value-bind (strlen bytes-read) (read-integer size-len stream :byte-order byte-order)
    (multiple-value-bind (string bytes-read-2) (read-bytes strlen stream)
      (values string (+ bytes-read bytes-read-2)))))

(defun write-counted-string (size-len stream buffer &key (byte-order :little-endian))
  "Writes the length of the BUFFER on the STREAM as an unsigned integer of SIZE-LEN bytes in the
specified BYTE-ORDER, then writes out the BUFFER."
  (declare (type integer size-len)
	   (type (simple-array (unsigned-byte 8)) buffer))
  (+ (write-integer (length buffer) size-len stream :byte-order byte-order)
     (write-bytes buffer stream)))

(defun simple-array-p (obj)
  (typep obj 'simple-array))

(defun array-pop (arr)
  (aref arr (decf (fill-pointer arr))))

(defun make-simple-array (complex-arr element-type)
  (make-array (length complex-arr)
	      :element-type element-type
	      :initial-contents complex-arr))

(defun read-terminated-string (stream &key (terminator (buffer 0)))
  "Reads a string ending in the byte sequence specified by TERMINATOR. The TERMINATOR is
not included in the resulting buffer. The default is to read C-style strings, terminated
by a zero."
  (declare (type (simple-array (unsigned-byte 8) (*)) terminator))
  (restart-case
      (let ((term-ix 0)
	    (bytes-read 0)
	    (result (make-array 0 :element-type '(unsigned-byte 8)
				:fill-pointer t :adjustable t)))
	(loop for byte = (prog1 (read-byte stream)
			   (incf bytes-read))
	   do (vector-push-extend byte result)
	     (when (eq byte (aref terminator term-ix))
	       (incf term-ix)
	       (when (eq term-ix (length terminator))
		 (loop repeat (length terminator) do (array-pop result))
		 (return-from read-terminated-string (values (make-simple-array result '(unsigned-byte 8))
							     bytes-read))))))
    (use-string-value (value)
      :report "Provide a string to use instead."
      :interactive (lambda ()
		     (format t "Enter a value of type STRING (evaluated): ")
		     (list (eval (read))))
      (values (flexi-streams:string-to-octets value) 0))))

(define-condition input-string-too-long (simple-error) ((input-string :initarg :input-string)))

(defun right-pad (string pad-length padding-character)
  "Pads the STRING with PAD-LENGTH copies of the PADDING-CHARACTER. If PAD-LENGTH is negative,
removes characters from the right end of the string instead."
  (cond ((> pad-length 0)
	 (concatenate 'string
		      string (make-string pad-length :initial-element padding-character)))
	((= pad-length 0)
	 string)
	((< pad-length 0)
	 (subseq string 0 (+ (length string) pad-length)))))

(defun make-truncated-fixed-length-string  (normal-string required-length external-format)
  (loop with pad-length = (- required-length (length (flexi-streams:string-to-octets normal-string :external-format external-format)))
     with min = pad-length
     with max = 0
     for encoded-string = (flexi-streams:string-to-octets (right-pad normal-string pad-length nil) :external-format external-format)
     until (= (length encoded-string) required-length)
     do (cond ((> (length encoded-string) required-length)
	       (setf max pad-length))
	      ((< (length encoded-string) required-length)
	       (setf min pad-length)))
       (setf pad-length (+ min (floor (- max min) 2)))
     finally (return encoded-string)))

(defun make-fixed-length-string (normal-string required-length external-format &optional (padding-character #\Nul))
  "Creates a FIXED-LENGTH-STRING and encodes it for writing. The REQUIRED-LENGTH is the length in bytes of the string
after encoding. The EXTERNAL-FORMAT is any value accepted by the FLEXI-STREAMS library as an external-format.

If the NORMAL-STRING is longer than the REQUIRED-LENGTH after encoding without any padding, then a condition of type
INPUT-STRING-TOO-LONG is raised. The restart CL:TRUNCATE tells this function to truncate the string to the required
length.

FIXME:

There is still a potential problem here. Suppose that getting to the REQUIRED-LENGTH requires adding an odd number
of bytes, but the PADDING-CHARACTER is encoded as an even number of bytes. Then this function would loop forever.

Alternately, suppose that the input is too long, and the TRUNCATE restart is chosen. If the input is one byte longer
than the REQUIRED-LENGTH, but the last character in the string is encoded as two bytes, then MAKE-TRUNCATED-FIXED-LENGTH-STRING
will never find the right number of characters to trim (the answer is to trim the two-byte character and then pad with a one-byte
character). I need to find concrete examples of this. These examples are likely to be found in the UTF-8 encoding. "
  (let ((initial-encoded-string (flexi-streams:string-to-octets normal-string :external-format external-format)))
    (cond ((= (length initial-encoded-string) required-length)
	   initial-encoded-string)
	  ((> (length initial-encoded-string) required-length)
	   (restart-case
	       (error 'input-string-too-long :input-string normal-string)
	     (truncate ()
	       :report "Truncate the string"
	       (make-truncated-fixed-length-string normal-string required-length external-format))))
	  (t
	   (loop with pad-length = (- required-length (length normal-string))
	      with max = pad-length
	      with min = 0
	      for encoded-string = (flexi-streams:string-to-octets (right-pad normal-string pad-length padding-character) :external-format external-format)
	      until (= (length encoded-string) required-length)
	      do (cond ((> (length encoded-string) required-length)
			(setf max pad-length))
		       ((< (length encoded-string) required-length)
			(setf min pad-length)))
		(setf pad-length (+ min (floor (- max min) 2)))
	      finally (return encoded-string))))))
	      

(defgeneric read-binary (type stream))
(defgeneric write-binary (obj stream))

(defmethod read-binary ((type (eql 'terminated-string)) stream)
  (read-terminated-string stream))

(defun write-terminated-string (string stream &key (terminator (buffer 0)))
    (+ (write-bytes string stream)
       (write-bytes terminator stream)))

(defun read-file (filename &key (element-type '(unsigned-byte 8)))
  (with-open-file (in filename :element-type element-type)
    (read-bytes (file-length in) in :element-type element-type)))

(defstruct binary-field
  name defstruct-field read-form write-form type bit-stream-id)

(define-condition unknown-type-size (simple-error) ())

(declaim (notinline type-size make-bit-field combine-field-descriptions
		    externally-byte-aligned-p))

(defvar *ignore-eval-type-bitstream-issue* t
  "If a DEFBINARY struct contains a field of type (EVAL ...),
then the macro cannot statically determine whether the struct can
be read without using a BIT-STREAM. If this is set to NIL, then
a condition is raised every time an (EVAL ...) type is encountered,
with restarts available to tell the macro whether a bitstream is
required for that particular field. 

Typically, the error would reach EMACS, and the programmer can then
pick a restart from the menu.

However, this doesn't work if you're using COMPILE-FILE, because COMPILE-FILE
catches exceptions, so you don't see the error until it has already been
caught, so you will not be presented with the restarts that I have set up.

For most programs, just ignoring the issue is good enough. Setting this to T (the default)
causes the expander to ignore this problem and not raise a condition.")

(defun type-size (type-spec)
  "Determines the size in bits of a DEFBINARY type, for the purposes
of determining whether reading the field requires doing non-byte-aligned I/O.

The types accepted are those handled by EXPAND-DEFBINARY-TYPE-FIELD.

It relies on a few simplifications when it comes to arrays and strings, which allow
it to not need to know the exact size of the array, only the count size and the size
of the element-type.

Returns three values:

   - The (approximate) number of bits that the field will take up.
   - T if the field could be treated as part of a BIT-FIELD, or
     NIL if it can only be read using a BIT-STREAM. BIT-FIELD support
     is limited to UNSIGNED-BYTEs and SIGNED-BYTEs.
   - :BIT-STREAM-ONLY if the field generates so much alignment uncertainty that
     the whole structure and everything thereafter must be read
     from a BIT-STREAM, and :NORMAL-STREAM if it's okay to read
     byte-aligned parts of the struct from a regular stream."
  (unless (listp type-spec)
    (setf type-spec (list type-spec)))
  (destructuring-case type-spec
    ((type &rest _)
     :where (eq type 'custom)
     (values 8 nil :normal-stream))
    ((type &rest _)
     :where (member type '(base-pointer file-position region-tag))
     (values 0 nil :normal-stream))
    ((type &rest irrelevant)
     :where (member type '(bit-field pointer))
     (declare (ignore irrelevant))
     ;; BIT-FIELDs are required to have a total size that is
     ;; measured in whole bytes, so we can just assume 8 bits.
     (values 8 t :normal-stream))
    ((type &rest crap &key (actual-type '(unsigned-byte 16)) &allow-other-keys)
     :where (eq type 'magic)
     (declare (ignore crap))
     (type-size actual-type))
    ((type &rest _ &key pointer-type &allow-other-keys)
     :where (eq type 'pointer)
     (multiple-value-bind (size can-be-in-bitstream stream-type)
	 (type-size pointer-type)
       (declare (ignore can-be-in-bitstream))
       (values size nil stream-type)))
    ((type length &rest who-cares)
     :where (member type '(fixed-length-string fixed-string))
     (declare (ignore who-cares length))
     (values 8 nil :normal-stream))
    ((type count-size &rest crap)
     :where (member type '(counted-string counted-buffer))
     (declare (ignore crap))
     (values (* 8 count-size) nil :normal-stream))
    ((counted-array count-size element-type &rest garbage)
     :where (eq counted-array 'counted-array)
     (declare (ignore garbage))
     (multiple-value-bind (element-size can-be-part-of-bit-field
					stream-type)
	 (type-size element-type)
       (declare (ignore can-be-part-of-bit-field))
       (if (and (divisiblep element-size 8)
		(integerp count-size)
		(eq stream-type :normal-stream))
	   (values 8 nil :normal-stream)
	   (values 1 nil :bit-stream-only))))
    ((simple-array type (array-size) &rest fuck-all)
     :where (eq simple-array 'simple-array)
     (declare (ignore fuck-all))
     (multiple-value-bind (type-size ign stream-type)
	 (type-size type)
       (declare (ignore ign))
       (cond ((and (divisiblep type-size 8)
		   (eq stream-type :normal-stream))
	      (values 8 t :normal-stream))
	     ((numberp array-size)
	      (values (* array-size type-size) nil stream-type))
	     (t (values 1 nil :bit-stream-only)))))
    ((type-name &rest args)
     :where (eq 'type-name 'eval)
     ;; FIXME: EVAL is the ultimate uncertain-size type, since
     ;; the actual type in question isn't even known
     ;; until runtime. But we don't want to impose
     ;; the performance overhead of a BIT-STREAM
     ;; every single time the EVAL type is used!
     ;;
     ;; The EVAL type must be extended with an option
     ;; to allow the programmer to specify whether
     ;; a bit stream is required.
     ;;
     ;; For now, we're using a RESTART-CASE to let
     ;; the programmer choose at macroexpansion time.
     ;;
     (restart-case
	 (error 'unknown-type-size :format-control "Unable to determine if a bit stream is needed to read/write type ~S"
		:format-arguments (list (cons type-name args)))
       (use-bit-stream ()
	 :report "Reading and writing this type involves reading less than 8 bits. After reading it, non-byte-aligned I/O may be needed."
	 (return-from type-size
	   (values 1 nil :bit-stream-only)))
       (use-bit-stream-field-only ()
	 :report "Reading and writing this type requires a bit-stream, but after reading it, further I/O will be byte-aligned."
	 (return-from type-size
	   (values 8 nil :bit-stream-this-field)))
       (no-bit-stream ()
	 :report "This type consists of whole bytes."
	 (return-from type-size
	   (values 8 t :normal-stream)))))
    ((byte-type bits) :where (member byte-type '(unsigned-byte signed-byte))
     (values bits t :normal-stream))
    ((float-type &rest _)
     :where (member float-type '(float single-float half-float double-float quadruple-float quad-float
				 octuple-float octo-float))
     (values (case float-type
	       ((float single-float) 32)
	       (half-float 16)
	       (double-float 64)
	       ((quadruple-float quad-float) 128)
	       ((octuple-float octo-float) 256))
	     t :normal-stream))
    ((null-type) :where (eq null-type 'null)
     (values 0 t :normal-stream))
    ((type termination-length &rest _)
     :where (member type '(terminated-string terminated-buffer))
     (values (+ 8 (* termination-length 8))
	     nil (if (integerp termination-length)
		     :normal-stream
		     :bit-stream-only)))
    ((type) :where (integerp type)
     (type-size `(,(if (> type 0)
		       'unsigned-byte
		       'signed-byte)
		   ,(abs type))))
    ((type) :where (enump type)
     (values
      (* (slot-value (get-enum-definition type) 'size) 8)
      nil :normal-stream))
    ((type) :where (get-type-fields type)
     (let ((stream-type :normal-stream))
       (values
	(loop for field in (get-type-fields type)
	   for type = (field-description-type field)
	   sum (multiple-value-bind (bits can-use-bit-field
					  stream-type*)
		   (type-size type)
		 (declare (ignore can-use-bit-field))
		 (unless (eq stream-type* :normal-stream)
		   (setf stream-type stream-type*))
		 bits))
	nil
	stream-type)))
    (otherwise
     (if *ignore-eval-type-bitstream-issue*
	 (values 8 nil :normal-stream)
	 (restart-case
	     (error 'unknown-type-size
		    :format-control "Cannot determine if type ~S requires a bit stream for I/O."
		    :format-arguments (list type-spec))
	   (use-bit-stream ()
	     :report "Require a bit-stream to read this type and anything containing it."
	     (return-from type-size
	       (values 1 nil :bit-stream-only)))
	   (bit-stream-field-only ()
	     :report "Create a bit stream to read this field, but use a normal stream for the rest of the structure."
	     (return-from type-size
	       (values 8 nil :bit-stream-this-field)))
	   (no-bit-stream ()
	     :report "Just use a normal stream to read this field."
	     (return-from type-size
	       (values 8 nil :normal-stream))))))))
    

(define-condition unspecified-type-error (simple-condition) ())

(defun expand-previous-defs (symbol value form)
  (subst* `((,symbol ,value)) form))

(defun align-to-boundary (byte-count boundary move-op stream)
  (if (divisiblep byte-count boundary)
      0
      (let ((to-next-boundary (- boundary (mod byte-count boundary))))
	(funcall move-op to-next-boundary stream)
	to-next-boundary)))

(defvar *debug* nil)

(defun runtime-reader/writer-form (reader-or-writer byte-count-name byte-count stream-symbol stream let-defs)
  "Wraps READER-OR-WRITER in the lexical context required for it to work. READER-OR-WRITER is a reader or writer
form generated by EXPAND-DEFBINARY-TYPE-FIELD at runtime as a result of an EVAL type specifier being used.
This form assumes that it will be spliced into the body of a READ-BINARY or WRITE-BINARY method, where certain
variables are bound. Since the EVAL type specifier produces code that will use this form in an EVAL, the values
that would normally be bound must be added with a LET form."
  `(let ((,byte-count-name ,byte-count)
	 (,stream-symbol ,stream))
     (declare (ignorable ,stream-symbol ,byte-count-name))
     (let ,let-defs
       (declare (ignorable ,@(loop for (var val) in let-defs collect var)))
       ,reader-or-writer)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (struct-like-defclass defbinary-type ()
			name type
			(byte-order :little-endian :type symbol)
		        reader writer stream-symbol
			previous-defs-symbol byte-count-name align element-align bind-index-to)
  (struct-like-defclass pointer ()
			(offset 0 :type integer)
			(type nil :type symbol)
			(base-pointer :beginning :type keyword)
			(stream *standard-input* :type keyword)))

(defstruct out-pointer
  (offset-position 0 :type number)
  (offset-size/bytes 0 :type number)
  (offset-byte-order :little-endian :type keyword)
  (offset-signedness nil :type boolean)
  (data-to-store nil)
  (closure #'identity :type function))

(defvar *queued-pointers* nil)

(defun push-to-tag (obj tag)
  "Add the OBJ to the queue for TAG.

This is the shittiest implementation ever. It functionally rebuilds the entire
tag list each time it runs, sometimes discarding this rebuild and going with
the original version. 

There are several reasons for this. First, the tag system can tolerate a
direct SETF of *QUEUED-POINTERS*, but it cannot tolerate mutation of the
underlying data structure. That is because I'm attempting to rely on the
way special variables work in multithreaded Lisps as a source of thread
safety. Mutating *QUEUED-POINTERS* with RPLACA or RPLACD could be seen
in another thread, but rebuilding the list and SETFing it will not affect
other threads.

FIXME: This approach doesn't provide thread safety unless every
thread binds *QUEUED-POINTERS* with a LET or LAMBDA form. The
WRITE-BINARY method can't do this because it must be able to
push items onto the tag that can be seen by other implementations
of WRITE-BINARY that might have the corresponding DUMP-TAG call."
  (let* ((found-tag nil)
	 (result (loop for node in *queued-pointers*
		     for (existing-tag . objects***) = node
		     if (eq existing-tag tag)
		     collect (prog1 (cons tag (cons obj objects***))
			       (setf found-tag t))
		     else collect node)))
    (setf *queued-pointers*
	  (if found-tag
	      result
	      (cons (list tag obj) *queued-pointers*)))))

(defun clear-tag (tag)
  (setf *queued-pointers*
	(remove tag *queued-pointers* :key #'car)))

(defun get-tag (tag)
  (cdr (assoc tag *queued-pointers*)))

(defun queue-write-pointer (tag offset-position offset-size/bytes offset-byte-order offset-signedness data-to-store closure)
  "Queue some data along with a closure to write it at a later time, and arrange for its address to be written
to the OFFSET-POSITION at that time."
  (push-to-tag (make-out-pointer
		:offset-position offset-position
		:offset-size/bytes offset-size/bytes
		:offset-byte-order offset-byte-order
		:offset-signedness offset-signedness
		:data-to-store data-to-store
		:closure closure)
	       tag))

(defun dump-tag (tag base-pointer stream &optional (previous-result 0))
  (let* ((tag-contents (prog1 (get-tag tag)
			 (clear-tag tag)))
	 (bytes-written (loop for out-pointer in tag-contents
			   for offset = (- (file-position stream) base-pointer)
			   sum (with-slots (offset-position offset-size/bytes offset-byte-order offset-signedness
							    data-to-store closure) out-pointer
				 (with-file-position (offset-position stream)
				   (write-integer offset offset-size/bytes stream :byte-order offset-byte-order
						  :signed offset-signedness))
				 (funcall closure data-to-store stream)))))
    (if (get-tag tag)
	(dump-tag tag base-pointer stream (+ bytes-written previous-result))
	(+ previous-result bytes-written))))

(defvar *base-pointer-tags* nil)

(defmacro with-local-pointer-resolving-context
    (&body body)
  "Deal with POINTERs in a thread-safe manner. Generated READ-BINARY and
WRITE-BINARY methods rely on special variables to store information to
make sure that offsets are calculated correctly. This macro creates local
bindings of all the relevant special variables."
  `(let ((*queued-pointers* *queued-pointers*)
	 (*base-pointer-tags* *base-pointer-tags*))
     ,@body))

(defun add-base-pointer-tag (tag pointer)
  (push (cons tag pointer) *base-pointer-tags*))

(defun get-base-pointer-tag (tag)
  (or
   (cdr (assoc tag *base-pointer-tags*)) 0))

;; One difficult thing to do is handle offsets within files. Some
;; file formats specify entire tables of offsets in the middle of
;; a structure. It is desireable to be able to resolve these offsets
;; into the objects they point to, and then convert them back into
;; offsets when the object is written back to disk.
;;
;; But the offsets have a base-pointer, which can be some completely
;; arbitrary value, but is often either:
;;
;;  The beginning of the file.
;;
;;  The beginning or end of a structure header.
;;
;;  The beginning or end of the offset itself.
;;
;;  The beginning of some arbitrary field in the structure.
;;
;; In the latter two cases, we can't just go ahead and read the pointer,
;; because the base-pointer isn't known until the entire header or even
;; a large portion of the file has been read.
;;
;; This can be solved by waiting until the entire header is read,
;; and only then converting the offsets into the data they point to.
;; This requires a placeholder to be put in the slot representing
;; the offset. This placeholder serves two functions: First, it
;; contains the actual offset that was read from the file.
;; Second, it remembers the type of object that is to be read.
;;
;; The macroexpander must remember the name of each field that is expected
;; to contain such a placeholder, and insert code to read the pointer and
;; replace the field. 
;;
;; The other problem is more difficult: An offset can be part of
;; a substructure embedded in a larger header, and so will be read
;; in a recursive call to READ-BINARY. The entire header
;; isn't read until the *top* level instance of `read-binary'
;; is done reading data. Therefore, `read-binary' must be given
;; an option to tell it not to resolve pointers, which must
;; be propagated recursively, so that recursive calls to READ-BINARY
;; do not attempt to resolve the offsets they encounter. This could
;; probably be handled with a special variable.
;;
;; Worse yet, an offset found in a structure can have a base
;; pointer relative to a field in a substructure.
;;
;; Even more difficult is writing offsets back to disk. Again,
;; writing of pointers cannot begin until the entire header
;; has been written to disk. That means that every time we
;; encounter a field that must be written as an offset, we
;; must remember the offset *of the offset*, and come back to
;; it when the address is known, which happens after the
;; header has been written. In the meantime, the offset must
;; be written as zero, or another nonce value. This calls for
;; yet another special variable, which stores a map of Lisp objects
;; waiting to be written to disk to the offsets of the offsets
;; that will point to them.
;;
;; Every time a value that is to be written as an offset is encountered,
;; a cons `(,object . ,(file-position)) is pushed onto the table. When
;; the toplevel READ-BINARY is reached, it pops each object, appends
;; it to the file, and writes its base-pointer-corrected offset to the
;; stored file-position.
;;
;; The only question is what to do if the file is divided into sections,
;; each with its own type of data.

(defun load-offset (offset)
  (declare (type pointer offset))
  (let* ((stream (slot-value offset 'stream))
	 (starting-position (file-position stream)))
    (file-position (slot-value offset 'stream)
		   (+ (ecase (slot-value offset 'base-pointer)
			(:beginning 0)
			(:end-of-header starting-position))
		      (slot-value offset 'offset)))
    (prog1
	(read-binary (slot-value offset 'type) (slot-value offset 'stream))
      (file-position stream starting-position))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-read/write-binary-type-body (field-name value stream-name stream read/write-form)
    `(let ((,field-name ,value)
	   (,stream-name ,stream))
       (declare (ignorable ,field-name ,stream-name))
       ,read/write-form)))

(defmacro read/write-binary-type (read-or-write type stream &key value (byte-order :little-endian) align element-align)
  `(alexandria:with-gensyms (stream-name struct-name field-name)
     (multiple-value-bind (defstruct-type runtime-reader runtime-writer)
       (expand-defbinary-type-field struct-name
			       (make-instance 'defbinary-type
					      :name field-name
					      :type ,type
					      :byte-order ,byte-order
					      :stream-symbol stream-name
					      :previous-defs-symbol nil
					      :align ,align
					      :element-align ,element-align))
       (declare (ignore defstruct-type ,(ecase read-or-write
				(:read 'runtime-writer)
				(:write 'runtime-reader))))
       (eval ,(subst 'dig 'quote
		     `',(expand-read/write-binary-type-body '(inject field-name) (list 'inject value) '(inject stream-name) (list 'inject stream)
							    (ecase read-or-write
							      (:read '(inject runtime-reader))
							      (:write '(inject runtime-writer)))))))))
       


(defun read-binary-type (type stream &key (byte-order :little-endian) align element-align)
  "Read a value of type TYPE from the STREAM. TYPE is any type supported by the DEFBINARY macro."
  (read/write-binary-type :read type stream :byte-order byte-order
			  :align align :element-align element-align))

(defun write-binary-type (value type stream &key (byte-order :little-endian) align element-align)
  "Write the VALUE, interpreting it as type TYPE, to the STREAM. The TYPE is any type supported by the
DEFBINARY macro."
  (read/write-binary-type :write type stream :byte-order byte-order
			  :value value
			  :align align :element-align element-align))


(declaim (inline read-binary-type write-binary-type))

(defparameter debug-data nil)

;; FIXME: It turns out that RESTART-CASE is actually kind of expensive, and DEFBINARY
;;        uses it absolutely everywhere. Perhaps the restarts should be ripped out
;;        entirely, or if possible, made optional.

(defun remove-all-restart-case (form)
  "Strips out all RESTART-CASE forms from the FORM, replacing them with their value form.
Improves performance at the cost of easy error recovery."
  (recursive-mapcar
   (lambda (node)
     (destructuring-case node
       ((form-name value-form &rest cases) :where (eq form-name 'restart-case)
	(declare (ignore cases))
	value-form)
       (otherwise node)))
   form t))

(defparameter *always-produce-byte-count*
  '(read-integer read-float read-counted-string read-terminated-string)
  "These functions are known to always produce a byte count, so DEFBINARY doesn't
have to generate code to verify that they did.")

(defun remove-double-multiple-value-bind (form)
  (recursive-mapcar
   (lambda (node)
     (destructuring-case node
       ((mvb-1 (gs1 gs2)
	       (mvb-2 (gs3 gs4) value-form
		      body-form)
	       &rest outer-body)
	:where (and (eq mvb-1 'multiple-value-bind)
		    (eq mvb-2 'multiple-value-bind))
	`(multiple-value-bind (,gs1 ,gs2) ,value-form
	     ,(subst* `((,gs3 ,gs1)
			(,gs4 ,gs2))
		      body-form)
	     ,@outer-body))
       (otherwise node)))
   form))

(defvar *virtual-types* nil)

(defstruct virtual-type
  name
  lambda-list
  reader
  writer
  lisp-type
  (estimated-total-bits 8)
  (stream-required :normal-stream)
  can-be-in-bit-field)

(defmacro define-virtual-type (name lambda-list reader writer lisp-type &key (estimated-total-bits 8) (stream-required :normal-stream)
									  can-be-in-bit-field)
  "Define a new Virtual Type.

A Virtual Type is a rule that tells the LISP-BINARY how to read a particular kind of
value from a stream, and how to write it back. The result of reading a Virtually-Typed object
can be an object of any Lisp type. Which Lisp type the Virtual Type corresponds to must
be specified in the LISP-TYPE argument.

The same LISP-TYPE can be produced by many different Virtual Types. As a result, the LISP-BINARY
library must always be told which type it is reading or writing. This is usually done at compile-time
through the DEFBINARY macro.

The READER and WRITER must be functions.

The READER accepts a STREAM argument and returns two values:

   1. The fully-processed value that was read.
   2. The number of bytes that were read from the STREAM. If the STREAM is a BIT-STREAM,
      then the number of bytes can be fractional, to indicate how much was read down to
      the bit. 1/8 of a byte = 1 bit.

The WRITER accepts the lambda list (OBJ STREAM), where OBJ is the value to be written. It
returns the number of bytes that were written.


"
  (pushover (make-virtual-type :name name :lambda-list lambda-list
			       :reader reader :writer writer :lisp-type lisp-type
			       :estimated-total-bits estimated-total-bits
			       :stream-required stream-required
			       :can-be-in-bit-field can-be-in-bit-field)
	    *virtual-types* :key #'virtual-type-name)
  `',name)

(defun build-virtual-type-reader/writer-expander (expression-form value-name stream-symbol)
  (alexandria:with-gensyms (type-name)
  `(destructuring-case ,expression-form
     ,@(loop for type in *virtual-types*
	  collect `((,type-name ,@(virtual-type-lambda-list type))
		    :where (eq ,type-name ,(virtual-type-name type))
		    (values `(funcall ,,(virtual-type-reader type) ,,stream-symbol)
			    `(funcall ,,(virtual-type-writer type) ,,value-name ,,stream-symbol)
			    `(:type ,,(virtual-type-lisp-type type)))))
     (otherwise (error "Unknown virtual type: ~S" ',expression-form)))))

(defun remove-impossible-error-checks (form)
  (recursive-mapcar
   (lambda (node)
     (destructuring-case node
       ((restart-case
	    (mvb bindings (function-name &rest function-arguments)
		 (unless variable
		   (error &rest stuff))
		 &rest body)
	  &rest restarts)
	:where (and (eq mvb 'multiple-value-bind)
		    (eq restart-case 'restart-case)
		    (eq unless 'unless)
		    (eq error 'error)
		    (member function-name *always-produce-byte-count*))
	(declare (ignore restarts variable stuff))
	`(multiple-value-bind ,bindings (,function-name ,@function-arguments)
	   ,@body))
       (otherwise node))) form))

(defvar *outer-stream-file-position* 0)

(defvar *type-info-objects* nil)

(defun expand-defbinary-type-field (struct-name type-info)
  "Expands the :TYPE field of a DEFBINARY form. Returns three values:
     1. A :TYPE specifier that can be spliced into a slot definition in a DEFSTRUCT form.

     2. A READER-FORM that can be spliced into another function to read a datum of the specified
        type. The READER-FORM will assume that it will be spliced into a scope where there's
        a readable stream. The name of this stream must be stored in (SLOT-VALUE TYPE-INFO 'STREAM-SYMBOL).

     3. A WRITER-FORM to write such a datum. It can be spliced into a scope similar to that of the READER-FORM.

TYPE-INFO is a DEFBINARY-TYPE that contains the following:

   NAME       - The name of the variable to be defined. The WRITER-FORM will be evaluated in a context
                where the NAME is bound to the object being written.
   TYPE       - The type specifier. Its value may specify a virtual type, such as (COUNTED-STRING 1) that this
                function will expand into something else.

   BYTE-ORDER - :LITTLE-ENDIAN or :BIG-ENDIAN.
   TERMINATOR - A SIMPLE-ARRAY that will be matched against the input to determine the end of a string.
                If the TYPE is itself a SIMPLE-ARRAY, TERMINATOR can be a function, which will be passed
                each element read. (TODO: Actually implement that function part)
   READER     - A user-specified function that overrides the reader that this function would otherwise
                generate.
   WRITER     - A user-specified writer function that overrides the default writer.
   STREAM-SYMBOL - The name that will be given to the input and output stream.

   PREVIOUS-DEFS-SYMBOL - A symbol that will be bound to a list of LET bindings at runtime. In the event of
                          an EVAL type, code is generated that will splice the LET bindings found here
                          into a runtime-constructed LET form that will then be EVAL'd.
   BYTE-COUNT-NAME      - The name of the variable that will be bound to the number of bytes read or written so far.
   ALIGN                - The alignment boundary
   ELEMENT-ALIGN        - The alignment boundary for individual elements in an array.
   BIND-INDEX-TO        - In array reads and writes, may specify a variable to bind the loop index to.
                          Or it can be NIL.

"
  (declare (type defbinary-type type-info)
	   (optimize (safety 3) (debug 3) (speed 0)))
  (bind-class-slots defbinary-type type-info
    (declare (ignore previous-defs-symbol bind-index-to)
	     (type symbol byte-count-name))
    (block this-function
      (let ((reader* nil)
	    (writer* nil)
	    (read-pointer-resolver nil)  ;; These are obsolescent values that were
	    (write-pointer-resolver nil) ;; supposed to be bubbled up the call stack.
	    (type (if (listp type)
		      type
		      (list type))))
	(values
	 ;; All supported types are represented as patterns in this
	 ;; DESTRUCTURING-CASE form. 
	 (destructuring-case type
	   ((type &key reader writer (lisp-type t))
	    :where (eq type 'custom)
	    (setf reader* (if reader
			      `(funcall ,reader ,stream-symbol)
			      `(values nil 0)))
	    (setf writer* (if writer			      
			      `(funcall ,writer ,name ,stream-symbol)
			      '(progn 0)))
	    `(:type ,lisp-type))
	   ((type &key raw-type member-types)
	    :where (eq type 'bit-field)	    
	    (letf (((slot-value type-info 'type) raw-type))
	      (multiple-value-bind (real-raw-type reader writer)
		  (expand-defbinary-type-field struct-name type-info)
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
		     (list :type member-types)))
		  (otherwise
		   (error "Invalid BIT-FIELD :RAW-TYPE value: ~S" raw-type))))))
	   ((type)
	    :where (eq type 'base-pointer)
	    (setf reader* `(let ((file-position (file-position ,stream-symbol)))
			     (add-base-pointer-tag ',name file-position)
			     (values file-position 0)))
	    (setf writer* `(progn
			     (setf ,name (file-position ,stream-symbol))
			     (add-base-pointer-tag ',name ,name)
			     0))
	    '(:type t))
	   ((type)
	    :where (eq type 'file-position)
	    (setf reader* `(values (file-position ,stream-symbol)
				   0))
	    (setf writer* `(progn (setf ,name (file-position ,stream-symbol))
				  0))
	    '(:type integer))
	   ((type &key base-pointer-name)
	    :where (eq type 'region-tag)
	    (setf reader* `(values ,base-pointer-name 0))
	    (setf writer* `(dump-tag ',name (if ,base-pointer-name
						(get-base-pointer-tag ,base-pointer-name)
						0) ,stream-symbol))
	    (push name *ignore-on-write*)
	    '(:type t))
	   ((type &key pointer-type data-type base-pointer-name region-tag validator)
	    :where (eq type 'pointer)
	    (letf (((slot-value type-info 'type) pointer-type))
	      (multiple-value-bind (pointer-defstruct-type pointer-reader pointer-writer)
		  (expand-defbinary-type-field struct-name type-info)
		(declare (ignore pointer-defstruct-type)
			 (optimize (speed 0) (debug 3)))
		(letf (((slot-value type-info 'type) data-type))
		  (multiple-value-bind (defstruct-type data-reader data-writer)
		      (expand-defbinary-type-field struct-name type-info)
		    (setf reader* (alexandria:with-gensyms (pointer-value base-pointer-address pointer-bytes-read
									  pbr2 pv2)
				    `(let* ((,pointer-bytes-read nil)
					    (,base-pointer-address ,(if base-pointer-name
									`(get-base-pointer-tag ,base-pointer-name)
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
					   (values value 0))))))
		    (setf writer* (alexandria:with-gensyms (closure)
				    `(let ((,closure (lambda (,name ,stream-symbol)
						       ,data-writer)))
				       (queue-write-pointer ,region-tag (file-position ,stream-symbol)
							    ;; Trying to figure out the size, byte order, and
							    ;; signedness of the pointer by analyzing the code
							    ;; that was generated to write it.
							    ,@(destructuring-case (recursive-find-sublist '(write-integer) pointer-writer)
										  ((write-integer number size stream &key (byte-order :little-endian) signed)
										   (declare (ignore write-integer number stream))
										   (list size byte-order signed))
										  (otherwise
										   (restart-case
										       (error "Can't determine the format of a pointer of type ~a" pointer-type)
										     (use-type (new-pointer-type)
										       :report "Enter a different pointer type to use"
										       :interactive (lambda ()
												      (format t "Enter the new type: ")
												      (list (read)))
										       (letf (((slot-value type-info 'type)
											       `(pointer :pointer-type ,new-pointer-type
													:data-type ,data-type
													:base-pointer-name ,base-pointer-name
													:region-tag ,region-tag)))
											 (return-from expand-defbinary-type-field
											   (expand-defbinary-type-field struct-name type-info))))
										     (enter-parameters (size byte-order signedness)
										       :report "Enter the pointer parameters manually"
										       :interactive (lambda ()
												      (list (progn
													      (format t "Enter the size in bytes of the pointer: ")
													      (eval (read)))
													    (progn
													      (format t "Enter the byte order of the pointer (:LITTLE-ENDIAN or :BIG-ENDIAN): ")
													      (eval (read)))
													    (progn
													      (format t "Is it signed? (Y/n): ")
													      (if (find #\n (read-line)
															:test #'equalp)
														  nil
														  t))))
										       (list size byte-order signedness)))))
							    ,name ,closure)
				       (let ((,name 0))
					 ,pointer-writer))))
		    defstruct-type)))))
	   ((type &key (actual-type '(unsigned-byte 16)) (value 0))
	    :where (eq type 'magic)
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
		  (expand-defbinary-type-field struct-name type-info)
		(setf reader (let ((v (gensym "READER-"))
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
				    (values ,v ,bytes-read)))))
		(setf writer `(progn
				(setf ,name ,value)
				,writer))
		(return-from this-function
		  (values defstruct-type reader writer)))))
	   ((type length &key (external-format :latin1) (padding-character #\Nul))
	    :where (member type '(fixed-length-string fixed-string))
	    (setf reader*
		  (let ((bytes (gensym "BYTES-"))
			(bytes* (gensym "BYTES*-"))
			(buffer (gensym "BUFFER-")))
		    
		    `(let ((,bytes nil))
		       (values
			(octets-to-string
			 (multiple-value-bind (,buffer ,bytes*)
			     (read-bytes ,length ,stream-symbol)
			   (setf ,bytes ,bytes*)
			   ,buffer)
			 :external-format ,external-format)
			,bytes))))
	    (setf writer*
		  `(write-bytes
		    (make-fixed-length-string ,name ,length ,external-format ,padding-character)
		    ,stream-symbol))
	    '(:type string))
	   ((type count-size &key (external-format :latin1))
	    :where (member type '(counted-string counted-buffer))
	      (setf reader* 
		    (let ((read-form `(read-counted-string ,count-size ,stream-symbol
							   :byte-order ,byte-order)))
		      (ecase type
			((counted-string) (alexandria:with-gensyms (value bytes)
					    `(multiple-value-bind (,value ,bytes) ,read-form
					       (values
						(octets-to-string ,value :external-format ,external-format)
						,bytes))))
			((counted-buffer) read-form))))
	      (setf writer* `(write-counted-string ,count-size ,stream-symbol
						   ,(ecase type
							   ((counted-string)
							    `(string-to-octets ,name :external-format ,external-format))
							   ((counted-buffer)
							    name))
						   :byte-order ,byte-order))
	      `(:type
		,(ecase type
			((counted-string) 'string)
			((counted-buffer) '(simple-array (unsigned-byte 8))))))
	   ((counted-array count-size element-type &key bind-index-to)
	    :where (eq counted-array 'counted-array)
	    (let ((count-size* (gensym "COUNT-SIZE-"))
		  (reader-value  (gensym "READER-VALUE-"))
		  (reader-byte-count (gensym "READER-BYTE-COUNT-")))
	      (letf (((slot-value type-info 'type)
		      `(simple-array ,element-type ((read-integer ,count-size* ,stream-symbol :byte-order ,byte-order))
				     ,@(if bind-index-to					 
					   `(:bind-index-to ,bind-index-to)))))
		(multiple-value-bind (defstruct-type reader writer)
		    (expand-defbinary-type-field struct-name type-info)
		  (setf writer `(let ((,count-size* ,count-size))
				  (+
				   (write-integer (length ,name) ,count-size* ,stream-symbol)
				   ,writer)))
		  (setf reader `(let ((,count-size* ,count-size))
				  (multiple-value-bind (,reader-value ,reader-byte-count) ,reader
				    (values ,reader-value (+ ,count-size* ,reader-byte-count)))))
		  (return-from this-function
		    (values defstruct-type reader writer))))))
	   ((simple-array type lengths &key bind-index-to)
	    :where (member simple-array '(simple-array))
	    (let ((array-count-size nil))	      
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
			(expand-defbinary-type-field struct-name type-info)
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
								(subst* `((,name-one (aref ,name ,ix))) write-one) nil))))
			       ,local-byte-count))
		      `(:type (simple-array ,(cadr defstruct-type)))))))))
	   ((type-name type-generating-form) :where (eq type-name 'eval)
	    (setf reader* `(read-binary-type ,type-generating-form ,stream-symbol :byte-order ,byte-order
					     :align ,align :element-align ,element-align))
	    (setf writer* `(write-binary-type ,name ,type-generating-form ,stream-symbol
					      :byte-order ,byte-order :align ,align
					      :element-align ,element-align))
	    '(:type t))
	   ((byte-type bits) :where (member byte-type '(unsigned-byte signed-byte))
	      (setf reader* `(read-integer ,(/ bits 8) ,stream-symbol
					   :byte-order ,byte-order
					   :signed ,(eq byte-type 'signed-byte)))
	      (setf writer* `(write-integer ,name ,(/ bits 8) ,stream-symbol
					    :byte-order ,byte-order
					    :signed ,(eq byte-type 'signed-byte)))
	    `(:type (,byte-type ,bits)))
	   ((float-type &key (byte-order byte-order))
	    :where (member float-type '(float single-float half-float double-float quadruple-float quad-float
					octuple-float octo-float))
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
	      (setf reader* `(read-float ,float-format :stream ,stream-symbol
					 :result-type ',float-type :byte-order ,byte-order))
	      (setf writer* `(write-float ,float-format ,name :stream ,stream-symbol
					  :byte-order ,byte-order))
	      `(:type ,float-type)))	   
	   ((null-type) :where (eq null-type 'null)
	    (setf reader* `(progn (values nil 0)))
	    (setf writer* `(progn 0))
	    `(:type t))
	   (nil
	    (let ((new-type
		   (restart-case
		       (error (format nil "DEFBINARY error: No type specified for ~a. Reading and Writing won't work" name))
		     (unsigned-byte-8 () :report "Use (UNSIGNED-BYTE 8)" '(unsigned-byte 8))
		     (unsigned-byte-16 () :report "Use (UNSIGNED-BYTE 16)" '(unsigned-byte 16))
		     (unsigned-byte-32 () :report "Use (UNSIGNED-BYTE 32)" '(unsigned-byte 32))
		     (unsigned-byte-64 () :report "Use (UNSIGNED-BYTE 64)" '(unsigned-byte 64))
		     (signed-byte-8 () :report "Use (SIGNED-BYTE 8)" '(signed-byte 8))
		     (signed-byte-16 () :report "Use (SIGNED-BYTE 16)" '(signed-byte 16))
		     (signed-byte-32 () :report "Use (SIGNED-BYTE 32)" '(signed-byte 32))
		     (signed-byte-64 () :report "Use (SIGNED-BYTE 64)" '(signed-byte 64))
		     (other (new-type) :interactive (lambda ()
						      (format t "Enter the type to use (unevaluated): ")
						      (list (read)))
			    :report "Specify a type to use instead."
			    new-type)
		     (fuck-it () :report "Let binary reading and writing be broken for this binary struct." nil))))
	      (if new-type
		  (progn
		    (setf (slot-value type-info 'type) new-type)
		    (return-from this-function
		      (expand-defbinary-type-field struct-name type-info)))
		  nil)))
	   ((type termination-length &key (external-format :latin1) (terminator 0))
	    :where (member type '(terminated-string terminated-buffer))
	    (let ((real-terminator `(ecase ,byte-order
				      ((:little-endian) (encode-lsb (or ,terminator 0) ,termination-length))
				      ((:big-endian) (encode-msb (or ,terminator 0) ,termination-length)))))
	      (let ((reader `(read-terminated-string ,stream-symbol :terminator ,real-terminator))
		    (name (ecase type
			    ((terminated-string) `(string-to-octets ,name :external-format ,external-format))
			    ((terminated-buffer) name))))
		(if (eq type 'terminated-string)
		    (setf reader `(read-octets-to-string ,reader :external-format ,external-format)))
		(setf reader* reader)
		(setf writer* `(write-terminated-string ,name ,stream-symbol :terminator ,real-terminator))
		(ecase type
		  ((terminated-string) '(:type string))
		  ((terminated-buffer) '(:type (simple-array (unsigned-byte 8))))))))
	   ((type)
	    :where (integerp type)
	    ;; Let a positive numeric type n be shorthand for (unsigned-byte n),
	    ;; and a negative one be shorthand for (signed-byte -n).
	    (setf (slot-value type-info 'type) (cond ((> type 0)
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
	    (return-from this-function
	      (expand-defbinary-type-field struct-name type-info)))
	   ((type)
	    (when reader (setf reader* `(funcall ,reader ,stream-symbol)))
	    (when writer (setf writer* `(funcall ,writer ,name ,stream-symbol)))
	    (unless reader*
	      (setf reader* (if (gethash type *enum-definitions*)
				`(read-enum ',type ,stream-symbol)
				`(read-binary ',type ,stream-symbol))))
	    (unless writer*
	      (setf writer* (if (gethash type *enum-definitions*)
				`(write-enum ',type ,name ,stream-symbol)
				`(write-binary ,name ,stream-symbol))))
	    `(:type ,(if (gethash type *enum-definitions*)
			 'symbol
			 type)))
	   ((type &rest something)
	    (declare (ignore type something))
	    (error "Unable to generate a reader or writer for unknown type ~S" (slot-value type-info 'type))))
	 (if reader
	     `(funcall ,reader ,stream-symbol)
	     reader*)
	 (if writer
	     `(funcall ,writer ,name ,stream-symbol)
	     writer*)
	 read-pointer-resolver write-pointer-resolver)))))

(defun defbinary-constructor-name (name defstruct-options)
  (let ((con (getf defstruct-options :constructor)))
    (or (if (consp con)
	    (car con)
	    con)
	(intern (format nil "MAKE-~a" name) (symbol-package name)))))

(defun expand-defbinary-field (struct-name name default-value &rest other-keys &key type (byte-order :little-endian)
							      byte-count-name
							      align element-align bit-stream-id
							      reader writer stream-symbol previous-defs-symbol
								     bind-index-to &allow-other-keys)
  "Expands the field into three values: A slot-descriptor suitable for CL:DEFSTRUCT, a form that reads this slot
from a stream, and a form that writes it to a stream. The reader and writer must be evaluated in a context where
the NAME is bound to the slot's value."
  (declare (optimize (safety 3) (debug 3) (speed 0)))
  (assert byte-count-name)
  (setf other-keys (remove-plist-keys other-keys :type :byte-order :encoding :terminator :reader :writer :stream-symbol :previous-defs-symbol
				                 :byte-count-name :align :element-align :bind-index-to))
  (multiple-value-bind (real-type read-form write-form)
      (expand-defbinary-type-field
       struct-name
       (make-instance 'defbinary-type 
		      :name name 
		      :type type
		      :byte-order byte-order
		      :reader reader
		      :writer writer
		      :stream-symbol (or bit-stream-id stream-symbol)
		      :previous-defs-symbol previous-defs-symbol
		      :byte-count-name byte-count-name
		      :align align
		      :element-align element-align
		      :bind-index-to bind-index-to))
    (make-binary-field
     :type type
     :name name
     :defstruct-field `(,name ,default-value
			      ,@real-type ,@other-keys)
     :bit-stream-id bit-stream-id
     :read-form (if align 
		    `(progn
		       (incf ,byte-count-name (align-to-boundary ,byte-count-name
								 ,align #'read-bytes ,stream-symbol))
		       ,read-form)
		    read-form)
     :write-form (if align
		     `(progn
			(incf ,byte-count-name
			      (align-to-boundary ,byte-count-name
						 ,align (lambda (bytes stream)
							  (loop repeat bytes
							       do (write-byte 0 stream)))
						 ,stream-symbol))
			,write-form)
		     write-form))))

(defun bitfield-spec->defstruct-specs (name options)
  (check-type name list)
  (let ((type (getf options :type)))
    (check-type type list)
    (unless (= (length name)
	       (length type))
      (error "In bitfield: Number of values ~a ~S doesn't match number of types ~a ~S"
	     (length name) name (length type) type))
    (loop for real-name in name
       for real-type in type
       collect `(,real-name 0 :type ,real-type ,@(remove-plist-keys options :type)))))

(defun find-bit-field-groups (list &key (key #'identity))
  "Identifies those fields in the LIST of fields that must be
read/written either as a BIT-FIELD or through a BIT-STREAM wrapper.
A list of the original fields is returned, with those groups
that will be read/written in a non-byte-aligned mode grouped
in sublists.

See also: TYPE-SIZE"
  (named-let local-loop
      ((current-group nil)
       (result nil)
       (running-total 0)
       (list list))
    (let* ((field (car list))
	   (bits (and list
		      (funcall key field))))
      (cond ((null list)
	     (reverse
	      (if current-group
		  (cons (reverse current-group) result)
		  result)))
	    ((not (divisiblep (+ running-total bits) 8))
	     (local-loop (cons field current-group)
			 result
			 (+ running-total bits)
			 (cdr list)))
	    ((divisiblep (+ running-total bits) 8)
	     (local-loop nil
			 (cons (if current-group
				   (reverse (cons field current-group))
				   field)
			       result)
			 (+ running-total bits)
			 (cdr list)))
	    (t (error "Shouldn't ever be reached"))))))
	    

(defun field-description-plist (field-description)
  (cddr field-description))

(defun field-option (field-description option &optional default)
  (getf (field-description-plist field-description) option default))

(defun field-description-type (field-description)
  (getf (field-description-plist field-description)
	:type))

(defun combine-field-descriptions (field-descriptions)
  "Group the FIELD-DESCRIPTIONS according to how they should be grouped into bit-fields.

Returns two values:

  1. A list of fields very similar to the FIELD-DESCRIPTIONS, except that some elements of the
     list will be replaced by lists of fields that should be grouped together. Since a single
     field description is itself a list, use LIST-OF-FIELDS-P to tell the difference between
     one field and a list of fields.

  2. Either :BIT-STREAM-REQUIRED if the whole struct can only be read with a bit-stream,
     or no second value otherwise.
"

  (find-bit-field-groups field-descriptions
			 :key (lambda (field)
				(multiple-value-bind (bits can-use-bit-field contagious-bit-stream)
				    (type-size (field-description-type field))
				  (declare (ignore can-use-bit-field))
				  (cond
				    ((eq contagious-bit-stream :bit-stream-only)
				     (return-from combine-field-descriptions
				       (values field-descriptions :bit-stream-required)))
				    (t bits))))))

(defun bit-field-type-p (type)
  (destructuring-case type
    ((type-name size) :where (and (member type-name '(unsigned-byte signed-byte))
				  (integerp size))
     t)
    (otherwise nil)))

(defun list-of-fields-p (datum)
  (destructuring-case datum
    ((variables default-value &rest options &key type &allow-other-keys)
     :where (and (or (and (listp variables)
			  (listp type)
			  (eq (car type) 'bit-field))
		     (symbolp variables)))
     (declare (ignore options default-value))
     nil)
    (otherwise t)))
		 
(defun expand-byte-shorthand (n)
  (if (numberp n)
      (if (> n 0)
	  `(unsigned-byte ,n)
	  `(signed-byte ,(- n)))
      n))

(defun make-bit-field (source-fields)
  (let ((types (mapcar 'expand-byte-shorthand
		       (mapcar 'field-description-type source-fields))))
    (if (divisiblep (apply #'+ (mapcar 'type-size types)) 8)
	`(,(mapcar #'car source-fields)
	   0
	   :type (bit-field :member-types ,(loop for type in types
					      collect (if (bit-field-type-p type)
							  type
							  (invoke-restart 'cant-make-bit-field)))
			    :raw-type (unsigned-byte ,(loop for (nil bits) in types
							 sum bits))))
	(invoke-restart 'cant-make-bit-field))))

(defun add-bit-stream-id (field-descriptions)
  (loop for (name default-value . options) in field-descriptions
     with stream-id = (gensym "BITSTREAM-")
       collect (list* name default-value :bit-stream-id stream-id options)))

(defun externally-byte-aligned-p (field-descriptions)
  (divisiblep (apply #'+
		     (mapcar (lambda (f)
			       (type-size (field-description-type f)))
			     field-descriptions))
	      8))

(defun convert-to-bit-fields (field-descriptions)
  "Converts groups of non-byte-aligning field descriptions into bitfields where possible.
If they can't be read as bitfields, then a :BIT-STREAM-ID option is added to the field. "
  (cond ((externally-byte-aligned-p field-descriptions)
	 (multiple-value-bind (combined-field-descriptions message)
	     (combine-field-descriptions field-descriptions)
	   (cond ((eq message :bit-stream-required)
		  (values field-descriptions :bit-stream-required))
		 ;; I want to decide dynamically whether to COLLECT the
		 ;; result of MAKE-BIT-FIELD or APPEND the MAYBE-FIELD
		 ;; it tried to operate on, depending on whether a restart
		 ;; is invoked. LOOP doesn't allow this easily, so instead,
		 ;; I build a SCRIPT that consists of :APPEND and :COLLECT
		 ;; commands, and then interpret that script to get the
		 ;; final result.
		 (t (let ((script
			   (loop for maybe-field in combined-field-descriptions
			      append (if (list-of-fields-p maybe-field)
					 (restart-case
					     `(:collect ,(make-bit-field maybe-field))
					   (cant-make-bit-field ()
					     `(:append ,(add-bit-stream-id maybe-field))))
					 `(:collect ,maybe-field)))))
		      (loop for (command object) on script by #'cddr
			 if (eq command :collect)
			 collect object
			 else if (eq command :append)
			 append object
			 else do (error "Internal error: Unknown command ~S" command)))))))
	 (t (values field-descriptions :bit-stream-required))))

(defmacro old-make-reader-let-def (f)
  "Creates a single variable definition to go in the let-def form within READ-BINARY. F is a
BINARY-FIELD object that describes the field. Captures several local variables within DEFBINARY.
It's a macro because it became necessary to use it twice."
  `(let* ((f-name (slot-value ,f 'name))
	  (f-form
	   (if (listp f-name)
	       (slot-value ,f 'read-form)
	       `(multiple-value-bind (,form-value ,most-recent-byte-count)
		    ,(slot-value ,f 'read-form)
		  (cond ((not (numberp ,most-recent-byte-count))
			 (restart-case
			     (error (format nil "Evaluation of ~a did not produce a byte count as its second value"
					    (with-output-to-string (out)
					      (print ',(slot-value ,f 'read-form) out))))
			   (use-value (val) :report "Enter an alternate value, dropping whatever was read."
					:interactive (lambda ()
						       (format t "Enter a new value for ~a: " ',f-name)
						       (list (eval (read))))
					(setf ,form-value val)
					(setf ,most-recent-byte-count 0))
								
			   (enter-size (size) :report "Enter a byte count manually"
				       :interactive (lambda ()
						      (format t "Enter the byte count: ")
						      (force-output)
						      (list (eval (read))))
				       (setf ,most-recent-byte-count size))))
			(t
			 (incf ,byte-count-name ,most-recent-byte-count)
			 ,form-value)))))
	  (x-form (subst* `((,previous-defs-symbol ,(reverse previous-defs)))
			  f-form)))
     (if (listp f-name)
	 (loop for real-name in f-name
	    do (push (list real-name (list 'inject real-name)) previous-defs))
	 (push (list f-name (list 'inject f-name)) previous-defs))
     (list f-name x-form)))

(defparameter *last-f* nil)

(defun %make-reader-let-def (f form-value most-recent-byte-count previous-defs previous-defs-push previous-defs-symbol
			    byte-count-name byte-order)
  "Creates a single variable definition to go in the let-def form within READ-BINARY. F is a
BINARY-FIELD object that describes the field."
  (let* ((f-name (slot-value f 'name))
	  (f-form
	   (if (listp f-name)
	       (slot-value f 'read-form)
	       `(multiple-value-bind (,form-value ,most-recent-byte-count)
		    ,(slot-value f 'read-form)
		  (cond ((not (numberp ,most-recent-byte-count))
			 (restart-case
			     (error (format nil "Evaluation of ~a did not produce a byte count as its second value"
					    (with-output-to-string (out)
					      (print ',(slot-value f 'read-form) out))))
			   (enter-value (val) :report "Enter an alternate value, dropping whatever was read."
					:interactive (lambda ()
						       (format t "Enter a new value for ~a: " ',f-name)
						       (list (eval (read))))
					(setf ,form-value val)
					(setf ,most-recent-byte-count 0))
								
			   (enter-size (size) :report "Enter a byte count manually"
				       :interactive (lambda ()
						      (format t "Enter the byte count: ")
						      (force-output)
						      (list (eval (read))))
				       (setf ,most-recent-byte-count size))))
			(t
			 (incf ,byte-count-name ,most-recent-byte-count)
			 ,form-value)))))
	  (x-form (subst* `((,previous-defs-symbol ,(reverse previous-defs)))
			  f-form)))
    (when (listp f-name)
      (setf f-name (ecase byte-order
		     (:little-endian (reverse f-name))
		     (:big-endian f-name)))
      (loop for real-name in f-name
	 do (funcall previous-defs-push (list real-name (list 'inject real-name))))
	 (funcall previous-defs-push (list f-name (list 'inject f-name))))
     (list f-name x-form)))

(defmacro make-reader-let-def (f)
  `(%make-reader-let-def ,f form-value most-recent-byte-count previous-defs (lambda (new-def)
									      (push new-def previous-defs))
			 previous-defs-symbol
			    byte-count-name byte-order))

(defun var-bit-stream (var bit-stream-groups)
  (maphash (lambda (stream vars)
	     (let ((field-object (find-if (lambda (field-object)
					    (eq var (slot-value field-object 'name)))
					  vars)))
	       (if field-object
		   (return-from var-bit-stream (values stream field-object)))))
	   bit-stream-groups))

(defun reverse-bit-stream-groups (bit-stream-hash real-stream-symbol let-defs)
  (loop for group in (group let-defs
			    :key (lambda (def)
				   (var-bit-stream (car def) bit-stream-hash)))
     append (if (eq (var-bit-stream (caar group) bit-stream-hash)
		    real-stream-symbol)
		group
		(reverse group))))

(defun add-stream-definitions (bit-stream-groups stream-symbol byte-order let-defs)
  "If the LET-DEFS contain fields that must be read from a bit-stream, this function
adds the necessary BIT-STREAM type variables to them. BIT-STREAM-GROUPS is a hash table
that maps each bit-stream variable name to the BINARY-FIELD objects of the variables that
must be read from that bit-stream. The STREAM-SYMBOL is the name of the default stream,
and BYTE-ORDER is the :BYTE-ORDER option passed to the DEFBINARY macro (only :BIG-ENDIAN
and :LITTLE-ENDIAN are supported. Handling for :DYNAMIC byte order must happen elsewhere)."
  (assert (member byte-order '(:big-endian :little-endian)))
  (loop for stream being the hash-keys in bit-stream-groups
     for var = (slot-value (car (last (gethash stream  bit-stream-groups)))
			   'name)
     do (setf let-defs
	      (insert-before var
			     `(,stream (wrap-in-bit-stream ,stream-symbol :byte-order ,byte-order))
			     let-defs
			     :key #'car)))
  let-defs)

(defun add-bit-stream-vars (bit-stream-groups stream-symbol byte-order make-let-def let-defs)
  (loop for (var def) in (add-stream-definitions bit-stream-groups stream-symbol byte-order let-defs)
     collect (multiple-value-bind (stream field-object)
		 (var-bit-stream var bit-stream-groups)
	       (if stream
		   (funcall make-let-def field-object stream)
		   `(,var ,def)))))


(defun group-write-forms (stream-names write-forms)
  "Groups a list of WRITE-FORMS according to which stream they write to. The
streams are identified by matching them to their names, which must be given in
STREAM-NAMES."
  (labels ((stream-used-here (form)
	     (recursive-find-if
	      (lambda (node)
		(member node stream-names))
	      form)))
  (loop for write-form-group in (group write-forms
				       :key #'stream-used-here)
     for stream-name = (stream-used-here write-form-group)
       collect (cons stream-name write-form-group))))


(defmacro defbinary (name (&rest defstruct-options
				 &key (byte-order :little-endian)
				 (preserve-*byte-order* t)
				 align
                                 export (byte-count-name (gensym "BYTE-COUNT-")) &allow-other-keys) &rest field-descriptions)
  "Defines a struct that represents binary data, and also generates readers and writers.

Example:

   (defbinary simple-binary (:export t
                             :byte-order :little-endian)
       (magic 38284 :type (magic :actual-type (unsigned-byte 16)
                                 :value 38284))
       (size 0 :type (unsigned-byte 32))
       (oddball-value 0 :type (unsigned-byte 32)
                        :byte-order :big-endian)
       ((b g r) 0 :type (bit-field :raw-type (unsigned-byte 8)
                                   :member-types
                                        ((unsigned-byte 2)
                                         (unsigned-byte 3)
                                         (unsigned-byte 3))))
       (name \"\" :type (counted-string 1 :external-format :utf8))
       (alias #() :type (counted-buffer 4)
                  :byte-order :big-endian)
       (floating-point 0.0d0 :type double-float)
       (big-float 0 :type octuple-float)
       (odd-float 0 :type (double-float :byte-order :big-endian))
       (c-string \"\" :type (terminated-buffer 1 :terminator 0))
       (nothing nil :type null) ;; Reads and writes nothing.
       (other-struct nil :type other-binary 
                     :reader #'read-other-binary
                     :writer #'write-other-binary)
       (struct-array #() :type (counted-array 1 simple-binary))
       (blah-type 0 :type (unsigned-byte 32))
       (blah nil :type (eval (case oddball-value
                           ((1) '(unsigned-byte 32))
                           ((2) '(counted-string 2)))))
       (an-array #() :type (simple-array (unsigned-byte 32) ((length c-string))))
       (body #() :type (simple-array (unsigned-byte 8) (size))))

The above generates a DEFSTRUCT definition for SIMPLE-BINARY, along with
a definition for a READ-BINARY method and a WRITE-BINARY method.
`
The READ-BINARY method is EQL-specialized, and will construct the needed
object for you. It can be invoked like this:

    (read-binary 'simple-binary stream)

The WRITE-BINARY method is called like this:

    (write-binary object stream)

:BYTE-ORDER is either :little-endian or :big-endian to control the byte order that will
be used when reading, writing, or sending over a network. It is defined on a struct-wide
basis, and can be overridden on a per-slot basis.

:EXPORT determines whether the struct name, slot names, and generators will be exported
from the current package.

TYPES

    DEFBINARY supports two kinds of types: Ordinary Common Lisp types, and Virtual Types.

    Out of the Common Lisp types, DEFBINARY knows how to read:

        (UNSIGNED-BYTE n) and (SIGNED-BYTE n), where N is the number of bits.
        Since these types are used so frequently in DEFBINARY structs, there is a shorthand
        for them: You can simply use the number of bits as the type. Positive for unsigned,
        and negative for signed. Example:

            (defbinary foobar ()
              (x 0 :type 16)  ;; 16-bit unsigned
              (y 1 :type -16)) ;; 16-bit signed

        float, short-float, half-float, single-float, double-float, quadruple-float,
        and octuple-float.

           FLOAT SINGLE-FLOAT are treated as IEEE Single Precision,
           DOUBLE-FLOAT is treated as IEEE Double Precision, while the others are read
           in as IEEE Half Precision, Quadruple Precision, etc.

           HALF-FLOAT is stored in memory as a single-float, while floats larger than
           Double Precision are decoded into RATIONAL numbers to preserve their
           precision (they are encoded in their proper format on writing).

        (SIMPLE-ARRAY element-type (size))

            Example:

            (defbinary foobar ()
               (size 0 :type (unsigned-byte 16))
               (arr #() :type (simple-array (unsigned-byte 8) (size))))

            For the element type, any real or virtual type supported by DEFBINARY is allowed.
            The SIZE is a Lisp expression that will be evaluated in an environment where all
            previous members of the struct are bound to their names.

        DEFBINARY will read and write all other CL objects using their READ-BINARY and
        WRITE-BINARY methods, if defined.

    The virtual types are:

        (COUNTED-ARRAY count-size-in-bytes element-type)

            This is a SIMPLE-ARRAY preceded by an integer specifying how many
            elements are in it. The SIMPLE-ARRAY example above could be rewritten to use
            a COUNTED-ARRAY instead, like this:

           (defbinary foobar ()
               (arr #() :type (counted-array 2 (unsigned-byte 8))))

          Obscure fact: The COUNT-SIZE-IN-BYTES does not have to be an integer. It can also be a
          fraction, which will trigger non-byte-aligned I/O. (example, if the size is 1/2, then the count
          is 4 bits wide, and the first element begins halfway into the same byte as the count) The
          ELEMENT-TYPE can also be non-byte-aligned. Doing this can result in needing to use a BIT-STREAM
          to do I/O with the struct.

          The same obscure fact applies anywhere the library accepts a size in bytes.

        (COUNTED-STRING count-size-in-bytes &key (EXTERNAL-FORMAT :latin1))
        (COUNTED-BUFFER count-size-in-bytes)

           These are like COUNTED-ARRAYS, but their elements are one byte long. Furthermore, a
           COUNTED-STRING will be encoded or decoded into a Lisp string according to its EXTERNAL-FORMAT.

           The encoding/decoding is done using the FLEXI-STREAMS library, and valid EXTERNAL-FORMATs are those
           that are accepted by FLEXI-STREAMS:OCTETS-TO-STRING.
 
           Example:

           (defbinary foobar ()
              (str \"\" :type (counted-string 2 :external-format :utf8)))

        (TERMINATED-STRING termination-length &key (terminator 0) (extenal-format :latin1))
        (TERMINATED-BUFFER termination-length &key (terminator 0))

            Specifies a C-style terminated string. The TERMINATOR is an integer that will be en/decoded
            according to the field's BYTE-ORDER. As such, it is capable of being more than one byte long,
            so it can be used to specify multi-character terminators such as CRLF.

        (FIXED-LENGTH-STRING length &key (external-format :latin1) (padding-character #\Nul))

            Specifies a string of fixed length. When writing, any excess space
            in the string will be padded with the PADDING-CHARACTER. The LENGTH is the
            number of bytes desired after encoding.

            If the input string is longer than the provided LENGTH, a condition of type
            LISP-BINARY:INPUT-STRING-TOO-LONG will be raised. Invoke the restart CL:TRUNCATE
            to trim enough excess characters from the string to make it equal to the LENGTH.

        (MAGIC &key actual-type value)

            Specifies that a magic value will be read and written. The value will be
            read as type ACTUAL-TYPE.

            If the value read is not CL:EQUAL to the VALUE given, then a condition of type
            BAD-MAGIC-VALUE will be raised.

            A BAD-MAGIC-VALUE object contains the slots BAD-VALUE and REQUIRED-VALUE.

            The error can be ignored by invoking the CL:CONTINUE restart. 

        BASE-POINTER

            Instead of reading or writing this field, CL:FILE-POSITION will be called
            on the current stream, and the address returned will be stored under a tag
            with the same name as this slot. The tag can then be used to calculate
            file positions and offsets. See the POINTER type for an example.

        FILE-POSITION

            Like BASE-POINTER, but no global tag is stored. The slot will contain the
            address in the file of the next thing to be read. No actual reading or
            writing is triggered by a slot of this type.
        
        (REGION-TAG &key base-pointer-name)

            Instead of writing the value of this slot, all POINTERs that have the same
            REGION-TAG name as this slot will be written out here, and the corresponding
            offsets will be updated. The file being written must be opened with
            :DIRECTION :IO. The POINTERs themselves will be written as offsets from
            whatever object has the BASE-POINTER named BASE-POINTER-NAME.

        (POINTER &key pointer-type data-type base-pointer-name region-tag)

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

        (BIT-FIELD &key raw-type member-types)

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

        (CUSTOM &key reader writer (lisp-type t))

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

            Both functions are optional.

        NULL

            Reading and writing will be a no-op. The value of a field of type NULL will always read
            as NIL and be ignored when writing.

RUNTIME TYPE DETERMINATION 

    (EVAL unquoted-type-expression)

    The EVAL type specifier causes the type to be computed at runtime. The UNQUOTED-TYPE-EXPRESSION will be
    evaluated just before attempting to read the field of this type in an environment where all the
    previously-defined fields are bound to their names.

    Example:

        (defbinary foobar ()
           (type-tag 0 :type (unsigned-byte 32))
           (data nil :type (eval (case type-tag
                                       (1 '(unsigned-byte 16))
                                       (2 '(counted-string 1 :external-format :utf-8))))))

   In the above example, READ-BINARY will first read the TYPE-TAG as an (UNSIGNED-BYTE 32),
   then it will evaluate the CASE expression in order to get the type of the DATA. Then it
   will generate a reader and a writer the same way DEFBINARY normally does, except at runtime
   instead of compile time.

   The CASE expression is not actually evaluated with a runtime call to EVAL. Instead, it is
   embedded directly in the source code of the generated READ-BINARY and WRITE-BINARY methods.

   The reader/writer code derived from the resulting type expression, however, *is* evaluated with
   a runtime call to EVAL, and the part of the macroexpander that handles type fields gets called
   at runtime as well.

   If the UNQUOTED-TYPE-EXPRESSION evaluates to another EVAL type specifier, then that specifier
   will be expanded once again. 

DEFINING STRINGS

    :EXTERNAL-FORMAT is any value accepted by flexi-streams:octets-to-string, such as :latin1 or :utf8.

    String count and terminated-buffer terminator lengths are given in BYTES, unlike the
    Common Lisp type '(unsigned-byte n)' in which the length is given in bits.

    The types COUNTED-STRING and COUNTED-BUFFER are virtual types. When encountered, DEFBINARY will
    generate code that deals with counted strings and buffers, which are blocks of data that begin
    with a size. The number passed with this type is the size of the count. The difference between the
    two is that one of them is converted to and from a STRING while the other retains its raw binary
    representation and will be a SIMPLE-ARRAY of (UNSIGNED-BYTE 8).

    (TERMINATED-STRING terminator-size) and (TERMINATED-BUFFER terminator-size) are for blocks of data that end with a
    terminator of some kind. The terminator is not produced as part of the buffer. The terminator-size defaults to 1.

    All generated buffers and strings are guaranteed to have the type SIMPLE-ARRAY.

ARRAYS OF COMPLEX TYPES

    In the (SIMPLE-ARRAY element-type length) type specifier, the LENGTH is evaluated when the array is being read.
    Previously defined fields will be bound. The ELEMENT-TYPE can be any type supported by DEFBINARY, including
    the EVAL type specifier and the COUNTED-STRING, COUNTED-BUFFER, TERMINATED-STRING, and TERMINATED-BUFFER virtual
    types. Notably, ELEMENT-TYPE can be other types that were previously defined with DEFBINARY.

    There is also the type (COUNTED-ARRAY count-size element-type), which functions like COUNTED-STRING or COUNTED-BUFFER,
    except that each element of the array can be a complex type.

NON-BYTE-ALIGNED I/O: AN ALTERNATIVE TO BIT FIELDS

          DEFBINARY supports non-byte-aligned reads. For example, if you want to read a 4-bit
          unsigned integer and a 12-bit signed integer:

            (defbinary non-conforming (:byte-order :big-endian)
              (x 0 :type 4) 
              (y 0 :type 12)) ;; Total: 16 bits.

          The above will compile to a single 16-bit read, and the two values will be automatically
          extracted into their respective fields. The reverse operation is generated for writing.

          In fact, the above is converted into a BIT-FIELD declaration, so it is exactly equivalent
          to the following:

            (defbinary non-conforming-bit-field-version (:byte-order :big-endian)
              ((x y) 0 :type (bit-field :raw-type (unsigned-byte 16)
                                        :member-types ((unsigned-byte 4)
                                                       (unsigned-byte 12)))))

          The macro will group sets signed or unsigned bytes to achieve a read that consists of
          whole bytes. This grouping mechanism only works for SIGNED-BYTE and UNSIGNED-BYTE integers.

          For other types, DEFBINARY will generate a temporary BIT-STREAM for the non-byte-aligned parts:

             (defbinary non-byte-aligned-string (:byte-order :big-endian)
               (x 0 :type 4)
               (string \"\" :type (counted-string 1))
               (y 0 :type 4))
             ;; End of non-byte-aligned part
               (z \"\" :type (counted-string 1)))

          As long as the sum of the bits adds up to a whole number of bytes, no
          special handling is required on the part of the programmer. Internally,
          the above generates a temporary bit-stream and reads from it, and it discards
          the bit-stream before reading Z, because Z doesn't require non-byte-aligned I/O.
          This is slower than doing whole-byte reads.

          Finally, you can specify bytes that throw off the byte-alignment of the
          stream:

              (defbinary stays-non-byte-aligned ()
                 (x 0 :type 3))

          If the macro cannot group the fields in the struct into byte-aligned reads,
          then the struct can only be read from a BIT-STREAM and not a normal
          stream (see WRAP-IN-BIT-STREAM and WITH-WRAPPED-IN-BIT-STREAM). In this
          case, the macro will generate READ-BINARY and WRITE-BINARY methods that
          are specialized to a second argument of type BIT-STREAM.

          BIT-STREAMs can wrap any type of stream, including other BIT-STREAMs. This
          means that you can nest one struct that does BIT-STREAM I/O inside another:

             (defbinary stays-non-byte-aligned ()
                (x 0 :type 3)
                (y nil :type non-byte-aligned-string)) ;; See above


NON-BYTE-ALIGNED FIELDS and LITTLE ENDIANNESS

  Bit fields are inherently confusing when they are applied to little-endian data (unlike
  big-endian data, where they make perfect sense). This is because programmers who write
  specifications for little-endian formats sometimes still describe the bit fields by
  starting with the most significant bit.

  Also, code that handles bit fields from little endian data may also handle that data
  starting with the most significant bit (including some byte-order-independent code in
  this library).

  The BIT-FIELD type in DEFBINARY adds to this confusion, since the fields must always
  be given starting with the most significant bit, regardless of the format's byte order.

  However, when specifying non-byte-aligned fields without using BIT-FIELDs, they must be
  specified starting with the LEAST significant bit in a LITTLE-ENDIAN format, but they
  must be specified starting with the MOST significant bit in a BIG-ENDIAN format. For
  example, consider the following toy format:

     (defbinary toy-format (:byte-order :little-endian)
       (a 0 :type 4)
       (b 0 :type 16)
       (c 0 :type 4))

  Write it to disk with the following code:

     (with-open-binary-file (out #P\"/tmp/test-1.bin\" :direction :output
 					 :if-exists :supersede
					 :if-does-not-exist :create)
	       (write-binary (make-toy-format :a #x1 :b #x2345 :c #x6) out))

  The resulting file would produce the following confusing hex dump:

    51 34 62

  What is that 5 from the middle doing at the very beginning?!?! Since 0x5 is
  the second-least-significant nibble in the structure, it appears in the
  most significant nibble of the least significant byte. 

  Reading the above as a little-endian, 24-bit unsigned integer gives the
  integer #x623451, which is what you should have been expecting, since
  C is the most significant field, and its value is 6.

  You can also specify the above format using the BIT-FIELD type. But then you
  have to account for the fact that in a BIT-FIELD type description, you always
  describe the most significant bits first, no matter what. So the variables
  and their corresponding types have to be reversed:

     (defbinary toy-format/bit-field (:byte-order :little-endian)
       ((c b a) nil :type (bit-field :raw-type (unsigned-byte 24)
                                     :member-types ((unsigned-byte 4)
                                                    (unsigned-byte 16)
                                                    (unsigned-byte 4)))))


ALIGNMENT

  DEFBINARY can generate aligned structures. Alignment is calculated as an offset from the beginning of the structure
  being defined. If a SIMPLE-ARRAY is defined with :ALIGN-ELEMENT {boundary}, then each element will be aligned to that
  boundary. On writes, the alignment is achieved by writing NUL (0) bytes. On reads, the alignment is performed by
  reading bytes from the stream one at a time. Alignment is always performed before reading and writing, never after.

MANUAL ALIGNMENT

    If the &KEY argument :BYTE-COUNT-NAME is specified, then the name given will be bound as a variable whose value is the number of bytes read or
    written so far. This binding is visible in forms that are evaluated at runtime, such as array-length specifiers and EVAL type
    specifiers. 

FLOATING-POINT NUMBERS

   DEFBINARY can read IEEE floats ranging from half-precision up to octuple precision. Double-precision and below are
   represented in memory by hardware floats, while larger-precision floats are decoded into RATIONALs.

   Furthermore, single and double-precision floats are decoded from their byte representation by using CFFI, which
   lets the hardware do the work on those systems that have FPUs (such as x86/AMD machines).

   All other types of floating-point number are encoded and decoded using arithmetic.

"
  (setf defstruct-options
	(remove-plist-keys defstruct-options :export :byte-order :byte-count-name :align :preserve-*byte-order*))
  (let* ((stream-symbol (gensym "STREAM-SYMBOL-"))
	 (*ignore-on-write* nil)
	 (bit-stream-groups (make-hash-table))
	 (bit-stream-required nil)
	 (previous-defs-symbol (gensym "PREVIOUS-DEFS-SYMBOL-"))
	 (most-recent-byte-count (gensym "MOST-RECENT-BYTE-COUNT-"))
	 (form-value (gensym "FORM-VALUE-"))
	 (parse-field-descriptions-fn
	  (lambda (field-descriptions)
	    (loop for f in field-descriptions
		     collect (apply #'expand-defbinary-field
				    (append (list name) f `(:stream-symbol ,stream-symbol :byte-count-name ,byte-count-name
									   :previous-defs-symbol ,previous-defs-symbol)
					    (if (field-option f :byte-order)
						nil
						`(:byte-order ,(if (eq byte-order :dynamic)
								   '*byte-order*
								   byte-order))))))))
	 (fields (funcall parse-field-descriptions-fn field-descriptions))
	 (name-and-options (if defstruct-options
			       (cons name
				     (remove-plist-keys defstruct-options :byte-order))
			       name))
	 (previous-defs nil))    
    (declare (optimize (safety 3)))
    (multiple-value-bind (converted bit-stream-required*)
	(convert-to-bit-fields field-descriptions)
      (setf bit-stream-required (and (eq bit-stream-required* :bit-stream-required)
				     t))
      (unless (equal field-descriptions converted)		   
	(setf fields (funcall parse-field-descriptions-fn converted))
	(setf field-descriptions converted)))
    (pushover (cons name field-descriptions) *known-defbinary-types*
	      :key #'car)
    (loop for f in fields do
	 (awhen (slot-value f 'bit-stream-id)
	   (push f (gethash it bit-stream-groups nil))))
    
    ((lambda (form)
       (if preserve-*byte-order*
	   form
	   (remove-binding '*byte-order* form)))
     `(progn
	(defstruct ,name-and-options
	  ,@(loop for (name default-value . options) in
		 (mapcar #'binary-field-defstruct-field fields)
	       for type = (getf options :type)
	       if (listp name)
	       append (bitfield-spec->defstruct-specs
		       name options)
	       else collect (list* name default-value :type type (remove-plist-keys options :type :bit-stream-id))))
	(defmethod read-binary ((type (eql ',name)) ,(if bit-stream-required
							 `(,stream-symbol bit-stream)
							 stream-symbol))
	  ,@(if align
		`((let* ((current-pos (file-position ,stream-symbol))
			 (mod (mod current-pos ,align)))
		    (unless (= mod 0)
		      (file-position ,stream-symbol
				     (+ current-pos (- ,align mod)))))))

	  ,(flet ((make-reader-body (byte-order)
				    `(let-values* ,(list* `(,byte-count-name 0)
							  '(*byte-order* *byte-order*)
							  (add-stream-definitions bit-stream-groups
										  stream-symbol
										  byte-order
										  (loop for f in fields
										     if (eq (slot-value f 'type) 'null)
										     collect
										       `(,(slot-value f 'name) nil)
										     else collect (make-reader-let-def f)
										     finally (setf previous-defs nil))))
				       (values
					(,(defbinary-constructor-name name defstruct-options)
					  ,@(loop for name in (mapcar #'binary-field-name fields)
					       if (symbolp name)
					       collect (intern (symbol-name name) :keyword)
					       and collect name
					       else append (loop for real-name in name
							      collect (intern (symbol-name real-name) :keyword)
							      collect real-name)))
					,byte-count-name))))
		 (if (eq byte-order :dynamic)
		     `(ecase *byte-order*
			(:big-endian ,(make-reader-body :big-endian))
			(:little-endian ,(make-reader-body :little-endian)))
		     (make-reader-body byte-order))))
	
	(defmethod write-binary ((,name ,name) ,(if bit-stream-required
						    `(,stream-symbol bit-stream)
						    stream-symbol))
	  ,@(if align
		`((let* ((current-pos (file-position ,stream-symbol))
			 (mod (mod current-pos ,align)))
		    (unless (= mod 0)
		      (loop repeat (- ,align mod) do (write-byte 0 ,stream-symbol))))))
	    ,(let ((ignore-decls (append *ignore-on-write*
					 (loop for field in fields
					    when
					      (destructuring-case (binary-field-type field)
						((eval &rest args)
						 :where (eq eval 'eval)
						 (declare (ignore args))
						 t)
						(otherwise nil))
					    collect (binary-field-name field))))
		   (slots (loop for f in (mapcar #'binary-field-name
						 fields)
			     if (listp f)
			     append f
			     else collect f)))
		`(let* ,(list `(,byte-count-name 0)
			      '(*byte-order* *byte-order*))
		   (with-slots ,slots ,name
		     ,@(if ignore-decls
			   `((declare (ignorable ,@ignore-decls))))
		     ,@(loop for (stream-name . body)
			  in (group-write-forms (cons stream-symbol
						      (remove nil (mapcar #'binary-field-bit-stream-id fields)))
						(loop for processed-write-form in
						     (loop for write-form in (mapcar #'binary-field-write-form fields)
							when (recursive-find 'eval write-form)
							collect (let ((fixed-let-defs (loop for var in slots collect
											   (CONS VAR (CONS (list 'inject VAR) 'NIL)))))
								  (subst* `((,previous-defs-symbol ,fixed-let-defs))
									  write-form))
							else collect write-form)
						   collect `(incf ,byte-count-name ,processed-write-form)))
			  collect `(,@(if (or (null stream-name)
					      (eq stream-name stream-symbol))
					  '(progn)
					  `(with-wrapped-in-bit-stream (,stream-name ,stream-symbol
										     :byte-order ,byte-order)))
				      ,@body))
				       
		     ,byte-count-name))))
	,@(when export
		`((export ',name)
		  (export ',(defbinary-constructor-name name defstruct-options))
		  ,@(loop for f in (mapcar #'binary-field-name fields)
		       if (listp f)
		       append (loop for real-name in f
				   collect `(export ',real-name))
			 else collect `(export ',f))))))))



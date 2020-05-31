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
	 :octuple-float
	 :+inf :-inf :quiet-nan :signalling-nan :nanp :infinityp))
	 
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

(defmacro define-enum (name size/bytes (&key signed (byte-order :little-endian)) &rest values)
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
     (let ((definition (make-enum-definition :size ,size/bytes
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
       (declare (ignorable ,@(loop for (var nil) in let-defs collect var)))
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
			(stream *standard-input* :type stream)))

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
  `(alexandria:with-gensyms (stream-name field-name)
     (multiple-value-bind (defstruct-type runtime-reader runtime-writer)
       (expand-defbinary-type-field (make-instance 'defbinary-type
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

(defparameter *type-expanders* nil)
(defmacro define-lisp-binary-type (type-info-var lambda-list &body body)
  "Defines a LISP-BINARY type. The TYPE-INFO-VAR will be bound to a DEFBINARY-TYPE
object that describes the field. The LAMBDA-LIST will be structurally matched against
the :TYPE parameter specified in the DEFBINARY form. The BODY may begin with the keyword
:WHERE followed by a form. If present, this form acts as a further condition for considering
the LAMBDA-LIST to match the :TYPE parameter.

The BODY is expected to return 3 values:

1. The Lisp type that should be used to represent the instance of this LISP-BINARY type described
   by the LAMBDA-LIST. 
2. A form that reads an instance of the data described by the LAMBDA-LIST from a stream whose name
   will be bound to STREAM-SYMBOL.
3. A form that writes an instance of the data from a variable whose name will be bound to NAME, into
   a stream whose name is bound to STREAM-SYMBOL.

The slots of the DEFBINARY-TYPE object provide extra information, and will be bound in the BODY using
WITH-SLOTS. These slots are described below:

   NAME       - The name of the variable to be defined. The WRITER-FORM will be evaluated in a context
                where the NAME is bound to the object being written.
   TYPE       - The type specifier. It's the same value that will have been matched against the LAMBDA-LIST.

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
  (let ((args (gensym)))
    `(push (lambda (,type-info-var &rest ,args)
	     (declare (ignorable ,type-info-var))
	     (bind-class-slots defbinary-type ,type-info-var
	       (apply 
		(destructuring-lambda ,lambda-list ,@body) ,args)))
	   *type-expanders*)))

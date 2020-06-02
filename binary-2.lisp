(in-package :lisp-binary)

(defun expand-defbinary-type-field (type-info)
  "Expands the :TYPE field of a DEFBINARY form. Returns three values:
     1. A :TYPE specifier that can be spliced into a slot definition in a DEFSTRUCT form.

     2. A READER-FORM that can be spliced into another function to read a datum of the specified
        type. The READER-FORM will assume that it will be spliced into a scope where there's
        a readable stream. The name of this stream must be stored in (SLOT-VALUE TYPE-INFO 'STREAM-SYMBOL).

     3. A WRITER-FORM to write such a datum. It can be spliced into a scope similar to that of the READER-FORM.

"
  (declare (type defbinary-type type-info)
	   (optimize (safety 3) (debug 3) (speed 0)))
  (loop for expander in *type-expanders*
     do (handler-bind ((no-destructuring-match
			(lambda (exn)
			  (declare (ignore exn))
			  (continue))))
	  (restart-case
	      (return (multiple-value-bind (type reader writer)
			  (apply expander (cons type-info (let ((type (slot-value type-info 'type)))
							    (if (listp type)
								type
								(list type)))))
			(values `(:type ,type)
				(aif (slot-value type-info 'reader)
				     `(funcall ,it ,(slot-value type-info 'stream-symbol))
				     reader)
				(aif (slot-value type-info 'writer)
				     `(funcall ,it
					       ,(slot-value type-info 'name)
					       ,(slot-value type-info 'stream-symbol))
				     writer))))
	    (continue () nil)))
       finally (error "Unknown LISP-BINARY type: ~s" (slot-value type-info 'type))))
	  
(defun defbinary-constructor-name (name defstruct-options)
  (let ((con (getf defstruct-options :constructor)))
    (or (if (consp con)
	    (car con)
	    con)
	(intern (format nil "MAKE-~a" name) (symbol-package name)))))

(defun expand-defbinary-field (name default-value &rest other-keys &key type (byte-order :little-endian)
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

(defun bitfield-spec->defstruct-specs (name default-values options)
  (check-type name list)
  (let ((type (getf options :type)))
    (check-type type list)
    (unless (= (length name)
	       (length type))
      (error "In bitfield: Number of values ~a ~S doesn't match number of types ~a ~S"
	     (length name) name (length type) type))
    (loop for real-name in name
	 for default-value in default-values
       for real-type in type
       collect `(,real-name ,default-value :type ,real-type ,@(remove-plist-keys options :type)))))

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
  (let ((types (mapcar #'expand-byte-shorthand
		       (mapcar #'field-description-type source-fields)))
	(default-values (mapcar #'second source-fields)))
    (if (divisiblep (apply #'+ (mapcar 'type-size types)) 8)
	`(,(mapcar #'car source-fields)
	   ,default-values
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
  "Defines a struct that represents binary data. Also generates two methods for this struct, named
READ-BINARY and WRITE-BINARY, which (de)serialize the struct to or from a stream. The serialization is
a direct binary representation of the fields of the struct. For instance, if there's a field with a :TYPE of
(UNSIGNED-BYTE 32), 4 bytes will be written in the specified :BYTE-ORDER. The fields are written (or read) in
the order in which they are specified in the body of the DEFBINARY form.

ARGUMENTS

  NAME - Used as the name in the generated DEFSTRUCT form.

  :BYTE-ORDER - The byte-order to use when reading or writing multi-byte
                data. Accepted values are :BIG-ENDIAN, :LITTLE-ENDIAN,
                and :DYNAMIC. If :DYNAMIC is specified, then the
                READ- and WRITE-BINARY methods will consult the special
                variable LISP-BINARY:*BYTE-ORDER* at runtime to decide
                which byte order to use. That variable is expected to
                be either :LITTLE-ENDIAN or :BIG-ENDIAN.
  
  :PRESERVE-*BYTE-ORDER* - Don't revert changes that get made to
                           LISP-BINARY:*BYTE-ORDER* during the call
                           to either READ- or WRITE-BINARY.

  :ALIGN - Align to the specified byte boundary before reading or writing
           the struct.
  
  :EXPORT - Export all symbols associated with the generated struct,
            including the name of the struct, the name of the constructor,
            and all the slot names.

  :BYTE-COUNT-NAME - In all value and type forms, bind to this name
                     the number of bytes in the struct written so far.

&ALLOW-OTHER-KEYS - All other keyword arguments will be passed through
                    to the generated CL:DEFSTRUCT form as part of the
                    NAME-AND-OPTIONS argument.

FIELD-DESCRIPTIONS - A list of slot specifications, having the following structure:

   (FIELD-NAME DEFAULT-VALUE &KEY TYPE BYTE-ORDER ALIGN ELEMENT-ALIGN
                                  READER WRITER BIND-INDEX-TO)

   The parameters have the following meanings:
   
   FIELD-NAME    - The name of the slot.
   
   DEFAULT-VALUE - The default value.
   
   TYPE          - The type of the field. Some Common Lisp types such as
                   (UNSIGNED-BYTE 32) are supported. Any type defined
                   with DEFBINARY is also supported. For more info, see
                  'TYPES' below.
   
   BYTE-ORDER    - The byte order to use when reading or writing this
                   field. Defaults to the BYTE-ORDER given for the whole
                   struct.
   
   ALIGN         - If specified, reads and writes will be aligned on this
                   boundary. When reading, bytes will be thrown away until 
                   alignment is achieved. When writing, NUL bytes will be
                   written.
   
   ELEMENT-ALIGN - If the TYPE is an array, each element of the array will
                   be aligned to this boundary.
   
   READER        - If speficied, this function will be used to read the field.
                   It must accept one argument (a stream), and return two
                   values - The object read, and the the number of bytes read.
                   The number of bytes read is used for alignment purposes.
   
   WRITER        - If specified, this function will be used to write the field.
                   It must accept two arguments (the object to write, and the
                   stream), and return the number of bytes written, which is
                   used for alignment purposes.
   
   BIND-INDEX-TO - If the EVAL type specifier is used as an array's element type
                   (see below), BIND-INDEX-TO will be bound to the current index
                   into the array, in case that matters for determining the type
                   of the next element.


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

TYPES

    DEFBINARY supports two kinds of types: Ordinary Common Lisp types, and Virtual Types.

    Out of the Common Lisp types, DEFBINARY knows how to read:

        (UNSIGNED-BYTE n) and (SIGNED-BYTE n &key (signed-representation :twos-complement), 
        where N is the number of bits. Since these types are used so frequently in DEFBINARY
        structs, there is a shorthand for them: You can simply use the number of bits as the
        type. Positive for unsigned, and negative for signed (two's complement only). Example:

            (defbinary foobar ()
              (x 0 :type 16)  ;; 16-bit unsigned
              (y 1 :type -16)) ;; 16-bit signed

        If you need to read one's complement, it must be written out:

            (defbinary foobar ()
              (x 0 :type (signed-byte 16 :signed-representation :ones-complement)))

        float, short-float, half-float, single-float, double-float, quadruple-float,
        and octuple-float.

           FLOAT and SINGLE-FLOAT are treated as IEEE Single Precision,
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

        (COUNTED-ARRAY count-size-in-bytes element-type &key bind-index-to)

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
           
            The default value you specify for this field should be given as a list
            of default values for each of the subfields.

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
  (let-values* ((stream-symbol (gensym "STREAM-SYMBOL-"))
		(*ignore-on-write* nil)
		(bit-stream-groups (make-hash-table))
		(previous-defs-symbol (gensym "PREVIOUS-DEFS-SYMBOL-"))
		(most-recent-byte-count (gensym "MOST-RECENT-BYTE-COUNT-"))
		(form-value (gensym "FORM-VALUE-"))
		((field-descriptions bit-stream-required) (convert-to-bit-fields field-descriptions))
		(fields (loop for f in field-descriptions
			   collect (apply #'expand-defbinary-field
					  (append f `(:stream-symbol ,stream-symbol :byte-count-name ,byte-count-name
										 :previous-defs-symbol ,previous-defs-symbol)
						  (if (field-option f :byte-order)
						      nil
						      `(:byte-order ,(if (eq byte-order :dynamic)
									 '*byte-order*
									 byte-order)))))))
		(name-and-options (if defstruct-options
				      (cons name
					    (remove-plist-keys defstruct-options :byte-order))
				      name))
		(previous-defs nil))    
    (declare (optimize (safety 3)))
    
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
		       name default-value options)
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
										     :byte-order ,(if (eq byte-order :dynamic)
												      '*byte-order*
												      byte-order))))
				      ,@body))
				       
		     ,byte-count-name))))
	,@(when export
		`((export ',name)
		  (export ',(defbinary-constructor-name name defstruct-options))
		  ,@(loop for f in (mapcar #'binary-field-name fields)
		       if (listp f)
		       append (loop for real-name in f
				   collect `(export ',real-name))
		       else collect `(export ',f))))
	',name))))



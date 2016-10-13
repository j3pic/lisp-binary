# lisp-binary
A library to easily read and write complex binary formats.

LISP-BINARY provides the DEFBINARY macro, with which you can declare
the structure of some piece of binary data, whether it's a file format,
a network protocol, or what have you.

The DEFBINARY macro generates a DEFSTRUCT form to contain the data,
and instances of the methods READ-BINARY and WRITE-BINARY, which
read the data from a stream.

The DEFBINARY macro can generate code to automatically read and write
the following types of data:

* Integers of all sizes, including non-byte-aligned sizes.
* Checked magic numbers
* Bit fields
* Floating point values in IEEE half, single, double, quadruple, and octuple precision formats.
* Enumerated types (an integer goes in the file, but all you see is a symbol)
* Strings: NUL-terminated (or an alternative terminator if you need it), Pascal-style counted strings (preceded by an integer giving the length), and fixed-length strings. Any encoding supported by FLEXI-STREAMS is supported.
* Arrays: Fixed-length and Pascal-style counted arrays are directly supported. The element-type can be anything you could use elsewhere in the DEFBINARY form. The length of the "fixed-length" type of array is evaluated at runtime, and can incorporate the values of
other fields.
* Raw byte buffers. They read as integers instead of characters, and aren't subject to a character-encoding.
* Offsets to data found later in the file (you see the data pointed to, not the offset).
* Other structs declared with DEFBINARY.

It is also possible to declare fields whose type isn't chosen until runtime, and
fields that have custom reader and writer functions. With these features, this
library can be used to both read and write nearly any binary format.

LISP-BINARY also provides:

* Primitives for reading integers, floats, and strings.
* A bit-stream library which can be used to read as little as one bit at a time.

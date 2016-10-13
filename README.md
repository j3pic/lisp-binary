# lisp-binary
A library to easily read and write complex binary formats.

(Jump to [Documentation](https://github.com/heegaiximephoomeeghahyaiseekh/lisp-binary/wiki))

LISP-BINARY provides the DEFBINARY macro, with which you can declare
the structure of some piece of binary data, whether it's a file format,
a network protocol, or what have you.

It is similar in spirit to [Binary-Types](https://github.com/frodef/binary-types) and [Kaitai Struct](https://github.com/kaitai-io/kaitai_struct).

The DEFBINARY macro generates a DEFSTRUCT form to contain the data,
and instances of the methods READ-BINARY and WRITE-BINARY, which
read the data from a stream.

The DEFBINARY macro can generate code to automatically read and write
the following types of data:

* Signed (Two's complement) and unsigned integers of all sizes, including non-byte-aligned sizes.
* Checked magic numbers
* Bit fields
* Floating point values in IEEE half, single, double, quadruple, and octuple precision formats.
* Enumerated types (an integer goes in the file, but all you see is a symbol)
* Strings: NUL-terminated (or an alternative terminator if you need it), Pascal-style counted strings (preceded by an integer giving the length), and fixed-length strings. Any encoding supported by FLEXI-STREAMS is supported.
* Arrays: Fixed-length and Pascal-style counted arrays are directly supported. The element-type can be anything you could use elsewhere in the DEFBINARY form (for example, you could have an array of NUL-terminated strings). The length of the "fixed-length" type of array is evaluated at runtime, and can incorporate the values of
other fields.
* Raw byte buffers. They read as 8-bit integers instead of characters, and aren't subject to a character-encoding.
* Offsets to data found later in the file (you see the data pointed to, not the offset).
* Other structs declared with DEFBINARY.

It is also possible to declare fields whose type isn't chosen until runtime, and
fields that have custom reader and writer functions. With these features, this
library can be used to both read and write nearly any binary format.

Both byte orders are supported for all data types, and the byte order can be declared either
statically or dynamically (so that it changes as the file is read-- required by a number of
formats, including TIFF).

LISP-BINARY also provides:

* Primitives for reading integers, floats, and strings.
* A bit-stream library which can be used to read one bit at a time.

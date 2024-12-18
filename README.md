# cl-store-faster

A fast and lightly customizable serializer/deserializer of Common Lisp
objects to compact binary files.  (Mainly working, just a few TODO items
remaining).  Currently is SBCL specific, but that shouldn't be a big deal
to change if someone wants to.

## why?

cl-store is a great piece of software which works well and meets most
needs.  Unfortunately, I tend to serialize / deserialize about 1GB of
data and, well, cl-store is pretty slow.  Other options include
hyperluminal-mem which is fast, but doesn't have the features I need
and its really geared toward working in memory / mmapped memory which
means streaming compression is out.

## General features

All number types are supported, with the addition of specialized compact and fast
writers for ub8, ub16, ub32, ub64, fixnum, single-float, double-float,
(complex double-float), and (complex single-float).

All array types are supported, with the addition of specialized compact and fast
writers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats,
double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32,
ub62, and ub64 (all these being supported by SBCL).

## User facing entry points
### (store-to-file filename &rest elements) / (restore-from-file filename)
### (store-to-vector &rest elements) / (restore-from-vector vector)
### (store-to-stream stream &rest elements) / (restore-from-stream stream)

## Examples

    CL-USER> (cl-store-faster:store-to-vector (list "abcd" 1234))
    #(11 21 0 4 2 97 0 0 0 98 0 0 0 99 0 0 0 100 0 0 0 11 1 210 4 12)
    
    CL-USER> (cl-store-faster:restore-from-vector *)
    ("abcd" 1234)

    CL-USER> (let* ((*print-circle* t)
    		    (v (make-array 1)))
		(setf (svref v 0) v)
               (cl-store-faster:store-to-file "blarg.bin" 'a 'b 'c v)
               (format nil "~A" (cl-store-faster:restore-from-file "blarg.bin")))
    "(A B C #1=#(#1#))"

## TODO

Simple-strings are not currently compactly stored (they are stored raw as 32 bits per
element, not as UTF-8, but I will fix that soon).

conditions are not supported yet
maybe packages and metaclasses

We waste some time recording references during serialization that
cannot be referred to (strings of symbol-names and package-names, for
example, as if we ran into the symbol again it would be stored as a
reference).  Similarly a few other places we record references that
will never be hit.

Store/restore to raw memory.

Some more testing and another run at speed

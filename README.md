# cl-store-faster

A fast and lightly customizable serializer/deserializer of Common Lisp
objects to compact binary files.  (Mainly working, just a few TODO items
remaining).  Currently is SBCL specific, but that shouldn't be a big deal
to change if someone wants to.  This is a work in progress, so do not use
(there is no versioning of data yet).

## why?

cl-store is a great piece of software which works well and meets most
needs.  Unfortunately, I tend to serialize / deserialize about 1GB of
data and, well, cl-store is pretty slow.  Other options include
hyperluminal-mem which is fast, but doesn't have the features I need
and its really geared toward working in memory / mmapped memory which
means streaming compression is out.

## Ideas

### Explicit references

The original design mirrored cl-store with implicit referencing in the
file where restoration had to mirror storage to count references.
That's slow, so I switched to an explicit reference labelling scheme
where only multiply-referenced objects need to be tracked on restore.
This speeds up restore on some work load by 10x at the cost of a
reference counting phase on the serialization side.  Turns out this
costs almost no time since we had to do the work anyway.  Most of the
serialization work turns out to be hash-table lookups and updates.

The neat thing that an explicit referencing scheme does is allow
parallelization on restore.  But now that restore is super fast, I'm
not going to jump into that first.  First I'm going to work on
parallelization of storage.

### Parallelization during reference counting step

This step will require locked hash tables.  That's very likely to be a
bottle-neck, but we can alleviate that slightly by splitting the hash
tables out into an eq table for standard-objects, structure-objects,
conses, numbers, and symbols (or some smaller number of these).  We
have to dispatch by type anyway so we can carry along the correct hash
tables (some serializers will want multiple type --- like symbols).
For now I will assume the user has split up objects they want to
serialize into chunks --- if they pass us an initial list we have to
do some work to split it up.  A vector is easier.

So, so far this is abysmal

### Parallelization during serialization

This step isn't perfectly parallelizable due to whatever stream
backend we have (but we'd like to hit disk or network limitations),
but the main challenge here is we haven't written out all references
yet --- they are written out in a first come first serve basis.  The
reference hash table is now used to keep track of whether a reference
has been written out or not (we will keep the split hash table scheme
as during reference counting for simplicity).  Here we can still store
the reference -> reference-id in a read only hash table and use a
separate ub8 reference vector with a 0/1 atomic update.

## General features

All number types are supported, with the addition of specialized compact and fast
writers for ub8, ub16, ub32, ub64, fixnum, single-float, double-float,
(complex double-float), and (complex single-float).

All array types are supported, with the addition of specialized compact and fast
writers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats,
double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32,
ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default serialize/deserializers.

symbols, hash-tables, and pathnames are supported.

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

- [ ] simple-strings serialize/deserialize as UTF-8 (not 32 bits per element)
- [ ] maybe handle serialize/deserialize conditions
- [ ] maybe handle serialize/deserialize packages
- [ ] add restarts to handle missing packages during symbol restore (create-package / rehome / discard)
- [ ] detect class and structure change on restore (just on the struct-info restore)
- [ ] support store/restore from raw memory (mmap, sap, etc)
- [ ] very large object storage without copying
- [ ] Parallel store and restore
- [ ] Reduced copying if using a sap backend?

Some more testing and another run at speed

## Parallelization

It's unlikely we would be able to hit disk bandwidths without
parallelizing (both during store and restore).  Part of the win of
parallelizing is that you can do thread local bump allocation for
small objects so things zoom quickly.  For a single thread and small
intrinsic objects we can hit >1M-objects per second, which isn't
terrible, but not nearly what I'd like.

The storage phase is the slowest phase 

## Random comments

Using single floats makes cl-store grind to a halt --- it's something
bad in eql hash tables with mixed objects with a few single floats
around.  The hash function for single floats is not amazing, but I
don't think that's the problem.  Also, the final hash table we
construct in these tests has a good bucket / object ratio, so it can't
be collisions...  but a million objects can take ten seconds.  I don't
see an easy way to instrument the hash table code, so I'm going to
just ignore this for now.  We don't hash single floats in this package
because they are immediates so any deduplication is not useful.  It
would shrink the file size by maximally 3 bytes per single float
(assuming we have less than 256 reference ids in the file) but not
worth it.

You'd think we could avoid storing symbol-name strings in the
reference tracker, but it's possible to have the same symbol name in
multiple packages so its worth it in the end.
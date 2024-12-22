# cl-store-faster

A fast and lightly customizable serializer/deserializer of Common Lisp
objects to compact binary format.  There is some simple versioning so
far, but I would not use this in production yet as I am still
developing this (22 Dec 2025).

Currently have a bunch of sbcl specific code and no alternative for other
Common Lisp implementations (see TODO).

I suggest for the time being you just continue to use cl-store which works
well and is not terribly slow.

## Focus
- Data that has multiple references to the same object as well as circular references
  - This dominates serialization time, so should be disable-able.
  - Complex list circularity and references (other than to the head of lists, separately disable-able))
- Speed and compactness
- Speed of restore

## General features

All number types are supported, with the addition of specialized
compact and fast writers for ub8, ub16, ub32, ub64, sb8, sb16, sb32,
fixnum, single-float, double-float, (complex double-float), and
(complex single-float).

All array types are supported, with the addition of specialized compact and fast
writers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats,
double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32,
ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default
serialize/deserializers.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality is
preserved across serialization / deserialization and circularity is
supported.

## Why?

I've been using [cl-store](https://cl-store.common-lisp.dev/) forever
and it works well and meets most needs once you tweak how it
serializes a bunch of things for speed.  I am usually serializing /
deserializing > 1GB of data and in the end all the tweaks I've done
don't make it fast enough.  The original cl-store hits about 1-2MB/sec
for store and restore on the data set I use which just is too slow.
It also doesn't support complex list circularity in a reasonable way
(the correct-list-store you can turn on blows the stack).

- It uses read-byte / write-byte to emit data which is OK-ish as long
  as you are using file streams in sbcl, but once you plug in a
  generic stream it crawls.
- The file format uses implicit referencing which makes it hard to
  deserialize quickly as each object must be tracked.  There was no
  easy way to to turn on and off what objects we should track references
  for (and it must match during store and restore).  It also tracks
  references for single floats which triggers a severe performance
  issue, at least on sbcl, related to eql hash tables which I have
  not tracked down..

I have also used [hyperluminal-mem](https://github.com/cosmos72/hyperluminal-mem),
which is fast if you meet the restrictions it has.  It's built in
stuff generates huge files.  It does not support delayed object
construction or references really, though you can extend it with
manual referencing on a per type basis.

## What is different here?

### Explicit reference scheme

To make deserialization fast, we break the symmetry of store and
restore by not using an implicit referencing scheme.  We add a
reference counting pass across the input data where we find out what
objects are multiply referenced.  Then on the actual write pass we
write the references out on first use.  This speeds up restoration by
about 10x.  Serialization with referencing is hash-table table
lookup/update dominated.

With explicit referencing we can do parallelization during restore,
though I haven't implemented it because it's already reasonably fast
(it averages 20MB/sec on my data set)... see TODO.

### Performance during serialization

Performance during serialization is dominated by the reference hash
table building and use.  This is quite hard to improve as the hash
table is an eq / eql table.  I could move the eql stuff (double-floats,
symbols, ratios, bignums, complex) to a separate table, which then
in principle would allow me to use a lockless table which would
enable parallelization.  That's a TODO item.  BUT, for my use case
it's the eq stuff (lots of cons'es) that dominates, which is
hard to improve without hacking on the internals of the common lisp
implementation.  This also makes it near impossible to parallelize
serialization because the default sbcl hash table is too slow when
synchronized.

I currently do not have a method to disable reference tracking, see
TODO.  If that's your need, I'd suggest you use hyperluminal-mem, but
it is on my TODO list and I would like this to be as fast.

## User facing entry points
### (store-to-file filename &rest elements) / (restore-from-file filename)
### (store-to-vector &rest elements) / (restore-from-vector vector)
### (store-to-stream stream &rest elements) / (restore-from-stream stream)

## Examples

    CL-USER> (cl-store-faster:store-to-vector (list "abcd" 1234))
    #(14 38 0 4 97 98 99 100 14 1 210 4 15)
    ;; 14 = cons, 38 = simple-string, 0 4 = length 4, 97 98 99 100 = abcd, 14 = cons
    ;; 1 210 4 = 16 bit integer 1234, 15 = nil
    
    CL-USER> (cl-store-faster:restore-from-vector *)
    ("abcd" 1234)

    CL-USER> (cl-store-faster:store-to-vector (make-string 1 :initial-element #\U+03b1))
    #(38 0 2 206 177) ;; 4 bytes, 38 = utf-8 string, 0 2 is encoded length = 2, 206 117 = alpha

    CL-USER> (let* ((*print-circle* t)
    		    (v (make-array 1)))
		(setf (svref v 0) v)
               (cl-store-faster:store-to-file "blarg.bin" 'a 'b 'c v)
               (format nil "~A" (cl-store-faster:restore-from-file "blarg.bin")))
    "(A B C #1=#(#1#))"

## TODO

- [ ] add large object without extending storage buffer
- [ ] add restarts to handle missing packages during symbol restore (create-package / rehome / discard)
- [ ] detect class and structure change on restore (just on the struct-info restore)
- [ ] support store/restore from raw memory (mmap, sap, etc)
- [ ] very large object storage without copying
- [ ] Parallel store and restore
- [ ] Reduced copying if using a sap backend?
- [ ] Separate EQ and EQL reference tables.  Support no reference table as an option for speed.
- [ ] Provide non-sbcl specific serializers
- [ ] Address slow compilation (a bit too much inlining --- remove most of it based on testing without reference table.
- [ ] Store number of references in the output so deserialization doesn't have to grow the references vector (minor tweak)

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
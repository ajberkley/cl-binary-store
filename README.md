# cl-store-faster

A fast and reasonably customizable serializer/deserializer of Common Lisp
objects to compact binary format.

Currently has a bunch of sbcl specific code though it shouldn't be too
hard to make work with other systems, just not top of mind right now. (see TODO).

## Focus
- Data that has multiple references to the same object as well as circular references
  - This dominates serialization time, so you can disable this.
  - Complex list circularity and references (other than to the head of lists, separately disableable))
- Speed and compactness

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
serialize/deserializers and also conditions.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality is
preserved across serialization / deserialization and circularity is
supported.  If you disable reference tracking serialization is quite
fast and appropriate for large simple-arrays which have dedicated (at
least on sbcl) serializers / deserializers which should be close to
disk/network/memory bandwidth limited.

Support for writing and reading from files, vectors, streams, and
raw memory without having to jump through hoops.

A very very simple versioning scheme is implemented.

## User interface
### (store place &rest data)

*place* can be string or pathname referring to a file, or a stream, or a
 vector, or NIL (in which case you will be returned a vector of data).

### (restore place)

*place* can be a string or pathname referring to a file, or a stream, or
a vector.  For restoring to a system-area-pointer use restore-from-sap
as you have to note how much data is available (good for mmap'ed files and
large arrays).

### (store-to-sap sap size &rest data)
### (restore-from-sap sap size)
 If you have an mmap'ed file or a raw system-area-pointer, you can store to
 it and restore from it.  For storing, if your allocated size (or mmap-ed
 file size) is not enough you will be thrown an 

For storing to one, you'll have to know that there
 is enough room otherwise we will error (storage is stateful so not
 easily restartable from the middle)
 a system-area-pointer for storing to say an mmap'ed file... up to you to
 know there is enough space though.  Good for large arrays, etc., which will
 be blitted directly into there at high speed.

## Examples
git clone the repo into your local quicklisp directory (usually ~/quicklisp/local-packages)
    CL-USER> (quicklisp:quickload "cl-store-faster")
    CL-USER> (in-package :cl-store-faster)
    CL-STORE-FASTER> (store nil (list "abcd" 1234))
    #(4 39 0 4 97 98 99 100 4 1 210 4 5)
    ;; 4 = cons, 39 = simple-string, 0 4 = length 4, 97 98 99 100 = abcd, 4 = cons
    ;; 1 210 4 = 16 bit integer 1234, 5 = nil
    
    CL-STORE-FASTER> (restore (store nil (list "abcd" 1234)))
    ("abcd" 1234)

    CL-STORE-FASTER> (store "blarg.bin" 'hi)
    "blarg.bin"
    CL-STORE-FASTER> (restore "blarg.bin")
    HI
    CL-STORE-FASTER> (store nil (make-string 1 :initial-element #\U+03b1))
    #(39 0 2 206 177) ;; 4 bytes, 39 = utf-8 string, 0 2 is encoded length = 2, 206 117 = alpha

    CL-STORE-FASTER> (let* ((*print-circle* t)
    		            (v (make-array 1)))
                        (setf (svref v 0) v)
                        (store "blarg.bin" 'a 'b 'c v)
                        (format nil "~A" (multiple-value-list (restore "blarg.bin"))))
    "(A B C #1=#(#1#))"

### Configurable options

\*track-references\* default is T.  Let this to NIL if you have simple
data with no references between them at all (so lists of data, no
circularity, no repeated objects).  This is very fast (100s of MB/sec
for small data; > 500 MB/sec for big data chunks; on my laptop).

\*support-shared-list-structures\* default is T.  Let this to NIL if
you have no list circularity (when it is NIL basic circular lists are
supported as the first CONS we see in a list is recorded as a
reference, so almost all data will work fine with this NIL; NIL makes
this package behave like cl-store which will die / explode if given
any complex circularity in a list).  Setting this to NIL is a
significant performance improvement if you have list heavy data.

\*store-class-slots\* default is NIL. Standard object :class allocated
slots will be stored if this is T.

\*write-magic-number\* default is NIL.  If T we will write out a magic
number and the \*write-version\* to the output, which will then be
validated against \*supported-versions\* when read back in.

### Extension points

Each object is stored with a 8-bit type tag.  You can define new
type tags and write specialized storage and restore methods.  See
codes.lisp for examples.  If you do this, you probably want to change
the \*write-version\* and \*supported-versions\*.

For serializing objects, the default behavior is probably good enough
for 95% of users.  We provide a simple extension point with a generic
function serializable-slot-info which you can change the behavior of,
for example to enable calling initialize-instance instead of
allocate-instance, or to not save certain slots or transform them
before saving.  See comments in src/objects.lisp for an example.

If that is not enough, I suggest adding a new store / restore method
on your object type or class.

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

I have also used
[hyperluminal-mem](https://github.com/cosmos72/hyperluminal-mem),
which is really fast if you meet the restrictions it has.  It does not
support delayed object construction or references really, nor a nice
default for structures and classes (though you can extend it with
manual referencing on a per type basis and with serializers for each
structure or class you want).

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

If your data does not contain multiple references or repeated objects
(in particular, repeated symbols will be stored repeatedly!), then you
can let \*track-references\* to NIL and you can hit hundreds of MB/sec
depending on your data (unicode strings are currently a bit slow as we
are not using a fast utf-8 encoder) and 100M cons+integer objects per
second.  It's about 10-30x faster than cl-store in this mode.  This
isn't the main focus of this package, though, so if you want > 1GB/sec
speeds, you will do a better with hyperluminal mem (though data is a
bit smaller with this package).  This package is between 3x faster and
3x slower than hyperluminal-mem depending on data patterns both on
store and restore.

## Parallelization

Unless we are using \*track-references\* nil we cannot come close
to hitting disk bandwidths on the storage phase.  So, one possible
direction is trying to parallelize the storage phase.  Unfortunately,
that is quite hard to do, as we are eq hash-table performance limited
and the sbcl synchronized hash tables are way too slow.

Parallelization during restore is much easier, but performance is
quite good already, so this isn't top of my list.

## TODO

- [ ] add restarts to handle missing packages during symbol restore (create-package / rehome / discard)
- [ ] Parallel store and restore ... restore is easy but store is near impossible
- [ ] Separate EQ and EQL reference tables.
- [ ] Provide non-sbcl specific serializers
- [ ] Faster UTF-8 encoding / decoding (currently doing extra copy using sb-ext string-to-octets / octets-to-string... babel is faster)
- [ ] Sort reference-ids by use amount if we have more than 255 or 65535 of them to shrink file?  An approximate radix sort might work quickly enough, but the cost of an extra puthash to keep an exact count is probably not worth it.

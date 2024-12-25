# cl-store-faster

A fast and reasonably customizable serializer/deserializer of Common Lisp
objects to compact binary format.

Currently has a bunch of sbcl specific code though it shouldn't be too
hard to make work with other systems, just not top of mind right now. (see TODO).

## Focus
- Data that has multiple references to the same object as well as circular references
  - This dominates serialization time (but you can disable this feature to get crazy speeds)
  - Complex list circularity and references (other than to the head of lists, separately disableable as well))
- Speed and compactness
- Extensibility for specialized data
- Should work out of the box without any effort with an easy programmer / user interface

## General features

All number types are supported, with the addition of specialized
compact and fast writers for ub8, ub16, ub32, ub64, sb8, sb16, sb32,
fixnum, single-float, double-float, (complex double-float), and
(complex single-float).

All array types are supported, with the addition of specialized
compact and very fast writers / readers for vectors and arrays of
simple-bit-vectors, simple-base-string, single-floats, double-floats,
fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31,
ub32, ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default
serialize/deserializers, and also conditions.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality is
preserved across serialization / deserialization and circularity is
supported.  If you disable reference tracking serialization is quite
fast.

Large simple-arrays have dedicated (at
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
 it and restore from it.

 For storing to one, you don't need to know the size in advance as we throw
 a restartable error allow you to allocate more system memory and continue
 on.  See tests/cl-store-faster-tests.lisp test-sap-write/read for a silly
 example of how to use this.  Nominally you'll be updating mmap regions of
 files or something.

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
circularity, no repeated objects).  This then goes very fast
(>500MB/sec / > 500 Mobjects/second for lists of numbers; > 5000
MB/sec for big simple arrays chunks).

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
the \*write-version\* and \*supported-versions\*.  Currently there isn't
an easy way to trigger rebuilding of the dispatch code --- I've just been
recompiling the dispatch.lisp and codes.lisp file.  It's on my TODO
list.

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
table is an eq table.  To enable parallelization on the serialization
side would require hacking the internals of the system.

If your data does not contain multiple references or repeated objects
(in particular, repeated symbols will be stored repeatedly!), then you
can let \*track-references\* to NIL and you can hit hundreds of MB/sec
depending on your data (unicode strings are currently a bit slow as we
are not using a fast utf-8 encoder) and 500M cons+integer objects per
second.  It's about 10-30x faster than cl-store in this mode.  This
package is between 3x faster and 3x slower than hyperluminal-mem
depending on data patterns both on store and restore.  If you are
storing simple arrays though, you want to use this package instead.

## TODO

- [ ] Make it easy for extensions to trigger a rebuild of dispatch code after adding new codes / extensions
- [ ] add restarts to handle missing packages during symbol restore (create-package / rehome / discard)
- [ ] Parallel store and restore ... restore is easy but store is near impossible
- [ ] Provide non-sbcl specific serializers
- [ ] Faster UTF-8 encoding / decoding (currently doing extra copy using sb-ext string-to-octets / octets-to-string)
- [ ] Sort reference-ids by use amount if we have more than 255 or 65535 of them to shrink file?  An approximate radix sort might work quickly enough, but the cost of an extra puthash to keep an exact count is probably not worth it.
- [ ] Add an end/stop marker so user can read objects one by one from raw memory or vectors or files?  Or more easily from sap vectors.

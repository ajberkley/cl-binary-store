# cl-store-faster

A fast and reasonably customizable serializer/deserializer of Common Lisp
objects to a binary format.

Currently this is sbcl specific, but I'd like to fix this up to work on other
implementations, but it isn't my goal since every other implementation (except
the commercial ones) are just too slow for me.

This project works, has some test coverage, but is not finished yet.
I expect it to be polished and released mid-January 2025.

> :warning: This is a work in progress, do not rely on it yet!

## Focus
- Data that has multiple references to the same object as well as circular references
  - This dominates serialization time (but you can disable this feature to get crazy speeds)
  - Complex list circularity and references (other than to the head of lists, separately disableable as well))
- Speed and compactness
- Extensibility for specialized data and objects
- Should work out of the box without any effort with an easy programmer / user interface
- Serialization deserialization on the same architecture
- 64-bit only currently (and it'll stay that way unless someone else cares).

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
 on.  See tests/cl-binary-store-tests.lisp test-sap-write/read for a silly
 example of how to use this.  Nominally you'll be updating mmap regions of
 files or something.

## Examples
git clone the repo into your local quicklisp directory (usually ~/quicklisp/local-packages)

    CL-USER> (quicklisp:quickload "cl-binary-store")
    CL-USER> (in-package :cl-binary-store-user) ;  or (use-package :cl-binary-store-use)
    CL-BINARY-STORE-USER> (store nil (list "abcd" 1234))
    #(4 39 0 4 97 98 99 100 4 1 210 4 5)
    ;; 4 = cons, 39 = simple-string, 0 4 = length 4, 97 98 99 100 = abcd, 4 = cons
    ;; 1 210 4 = 16 bit integer 1234, 5 = nil
    
    CL-BINARY-STORE-USER> (restore (store nil (list "abcd" 1234)))
    ("abcd" 1234)

    CL-BINARY-STORE-USER> (store "blarg.bin" 'hi)
    "blarg.bin"
    CL-BINARY-STORE-USER> (restore "blarg.bin")
    HI
    CL-BINARY-STORE-USER> (store nil (make-string 1 :initial-element #\U+03b1))
    #(39 0 2 206 177) ;; 4 bytes, 39 = utf-8 string, 0 2 is encoded length = 2, 206 117 = alpha

    CL-BINARY-STORE-USER> (let* ((*print-circle* t)
                                 (v (make-array 1)))
                             (setf (svref v 0) v)
                             (store "blarg.bin" 'a 'b 'c v)
                             (format nil "~A" (multiple-value-list (restore "blarg.bin"))))
    "(A B C #1=#(#1#))"

To run tests you want to do
    CL-USER> (quicklisp:quickload "cl-store-binary-tests")
    CL-USER> (parachute:test 'cl-store-binary-tests)

The package :cl-binary-store-user exports all the user facing interfaces above.  The package
:cl-binary-store (will eventually) exports all the symbols needed to add your own extensions.

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

## Benchmarking

To make something fast you have to measure it!  In src/benchmarks.lisp you'll see comparisons
between CL-STORE, HYPERLUMINAL-MEM, and this package.  For example:

### A long list of very small integers
    ;; HYPERLUMINAL-MEM
    ;;  OUTPUT SIZE: 8.00 MB
    ;;  HLMEM WRITE: 2.32 ms at 3448 MB/sec
    ;;  HLMEM READ : 5.04 ms at 1587 MB/sec
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 3.00 MB
    ;;  CL-BINARY-STORE WRITE: 3.60 ms at 833 MB/sec ;; (/ 2e6 3.6e-3) = 550Mobjs/second
    ;;  CL-BINARY-STORE READ : 8.20 ms at 366 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 5.00MB
    ;;  CL-STORE WRITE: 59.20 ms at 84 MB/sec
    ;;  CL-STORE READ : 50.40 ms at 99 MB/sec
    (defun long-list-of-small-integers (&optional (n 1000000))
      (make-list n :initial-element (random 256)))

This is the most synthetic of all the tests.  It runs cl-binary-store without reference
tracking which matches the behavior of cl-store and hyperluminal-mem for this single list
case.  First thing to note is that the output file is small, because very small integers
get written at 2 bytes per, one for a tag and one for the value.  For numbers less than
64-bit represented, we special case 8-bit, 16-bit, 32-bit, fixnum.  Note that MB/sec isn't
a great benchmark when output sizes vary a lot.  I note the above Mobjs/second instead,
which is pretty incredible when you think about it --- I mean this is a super synthetic
benchmark but we are doing type dispatching nice and quick here (OK, conses and fixnums
are immediate objects, so type dispatch is crazy fast and they are at the front of the
dispatch table --- see type-discrimination.md for some poking at this stuff).

### A long list of fixnums

The next one is cool because you can see the neat way hyperluminal-mem does tagging,
a lot like the underlying sbcl implementation.  It's cool and makes things go very
very fast in some cases.  Here you can see the wild difference between cl-store and
this package.

    ;; HYPERLUMINAL-MEM
    ;;  OUTPUT SIZE: 8.00 MB ;; bit tagging, not byte tagging
    ;;  HLMEM WRITE: 2.24 ms at 3571 MB/sec
    ;;  HLMEM READ : 5.04 ms at 1587 MB/sec
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 10.00 MB
    ;;  CL-BINARY-STORE WRITE: 3.84 ms at 2604 MB/sec
    ;;  CL-BINARY-STORE READ : 9.48 ms at 1055 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 38.00MB
    ;;  CL-STORE WRITE: 422.39 ms at 90 MB/sec
    ;;  CL-STORE READ : 293.20 ms at 130 MB/sec
    (defun long-list-of-random-fixnums (&optional (n 1000000))
      (make-list n :initial-element (random (- (expt 2 61) (expt 2 60)))))

### Keywords!

While we are talking about synthetic things, how about a bunch of
random keywords, cause who doesn't like randomly filling their heap
with stuff.  Here we have reference tracking on all three systems
(symbols is the only case where hyperluminal-mem does this).

    ;; With reference tracking on everyone
    ;; HYPERLUMINAL-MEM
    ;;  OUTPUT SIZE: 3.20 MB
    ;;  HLMEM WRITE: 18.52 ms at 173 MB/sec
    ;;  HLMEM READ : 33.32 ms at 96 MB/sec
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 1.15 MB
    ;;  CL-BINARY-STORE WRITE: 28.56 ms at 40 MB/sec
    ;;  CL-BINARY-STORE READ : 29.36 ms at 39 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 1.24MB
    ;;  CL-STORE WRITE: 52.80 ms at 24 MB/sec
    ;;  CL-STORE READ : 63.60 ms at 20 MB/sec
    (defun lots-of-keywords ()
      (loop for i fixnum from 0 below 100000
    	collect (intern (format nil "~A" (random 250000)) 'keyword)))

Here we are eq hash table dominated... not much to do about it right now.

### structure-objects

I didn't check hyperluminal-mem on this because you have to write a couple lines of
code to make it work for user structs.

    ;; hyperluminal mem needs an extension for this so skipping it for now
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 1.79 MB
    ;;  CL-BINARY-STORE WRITE: 49.00 ms at 37 MB/sec
    ;;  CL-BINARY-STORE READ : 9.24 ms at 194 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 6.69MB
    ;;  CL-STORE WRITE: 171.20 ms at 39 MB/sec
    ;;  CL-STORE READ : 182.40 ms at 37 MB/sec
    (defun lots-of-structure-objects ()
      (loop for i below 100000
            collect (make-blarg :a (random 1d0) :b (format nil "~A" (random 100)))))

### UTF-8 encoded strings

So hyperluminal-mem and cl-store output the raw 32-bit code points, whereas this package
utf-8 encodes/decodes them.  I just happen to have a lot of (simple-array character (*))
in the data set that I use that are really just simple-base-strings... so a feature for me.
It does increase the cons'ing during store restore as I have to copy the strings during
encode / decode currently.

    ;; HYPERLUMINAL-MEM
    ;;  OUTPUT SIZE: 2.40 MB
    ;;  HLMEM WRITE: 4.52 ms at 531 MB/sec
    ;;  HLMEM READ : 3.12 ms at 769 MB/sec
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 1.19 MB
    ;;  CL-BINARY-STORE WRITE: 4.04 ms at 294 MB/sec
    ;;  CL-BINARY-STORE READ : 8.68 ms at 137 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 2.08MB
    ;;  CL-STORE WRITE: 23.20 ms at 90 MB/sec
    ;;  CL-STORE READ : 11.60 ms at 179 MB/sec
    (defun simple-strings ()
      (loop for i below 100000
            collect (format nil "~A~A" (random 1000000) #\U+03b1)))

### Maybe more real world like?

Heh, not a chance.  This is still synthetic but I wanted to stress the
dispatch mechanism a little bit. It is funny here if you put in random
single floats cl-store doesn't finish for ages... it's something bad
in sbcl eql hash tables but I haven't tracked it down yet.  Anyhow the first
set of numbers are without reference tracking, so eql'ity is violated (that is
they are all storing multiple copies of the strings --- do not worry, this is
not the default settings of my package!).  Anyhow, still moving at a rapid clip,
beating hyperluminal-mem which is nice.  Of course the real world use is the second
set of numbers below "and using referencing" comment, where you see the explicit
referencing scheme speeding up restore quite a lot.

    ;; HYPERLUMINAL-MEM
    ;;  OUTPUT SIZE: 20.48 MB
    ;;  HLMEM WRITE: 34.12 ms at 600 MB/sec
    ;;  HLMEM READ : 30.96 ms at 662 MB/sec
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 8.06 MB
    ;;  CL-BINARY-STORE WRITE: 25.20 ms at 320 MB/sec
    ;;  CL-BINARY-STORE READ : 43.76 ms at 184 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 27.78MB
    ;;  CL-STORE WRITE: 448.79 ms at 62 MB/sec
    ;;  CL-STORE READ : 408.40 ms at 68 MB/sec
    ;; and using referencing to deduplicate the double-floats and strings
    ;; CL-BINARY-STORE
    ;;  OUTPUT SIZE: 3.38 MB
    ;;  CL-BINARY-STORE WRITE: 65.52 ms at 52 MB/sec
    ;;  CL-BINARY-STORE READ : 12.32 ms at 274 MB/sec
    ;; CL-STORE
    ;;  OUTPUT SIZE: 4.88MB
    ;;  CL-STORE WRITE: 129.60 ms at 38 MB/sec
    ;;  CL-STORE READ : 110.40 ms at 44 MB/sec
    (defun long-complex-list ()
      (loop repeat 1000000 collect (if (> (random 1000) 500)
    				   3.1415d0
    				   ;; (complex 1d0) ;; cl-store chokes
    				   ;; (random 1d0) ;; cl-store chokes
    				   (if (> (random 100) 50)
    				       ;;(random 1f0) ;; <- makes cl-store take forever!
    				       "hi" ;;(format nil "~A" (random 123))
    				       (if (> (random 100) 50)
    					   (cons (random 30) 2)
    					   (if (= (random 2) 1)
    					       "hello"
    					       ;; (random 1f0) slows cl-store crazily
    					       #()))))))

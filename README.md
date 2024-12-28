# cl-binary-store

A fast and reasonably customizable serializer/deserializer of Common Lisp objects to/from a binary format.  Equality of objects, circular references, and the full Common Lisp type system are supported.

> :warning: Note that this is not a serializer/deserializer for communication with other types of systems *yet*.  If you want that maybe use JSON or if you want a binary format maybe [cl-messagepack](https://github.com/mbrezu/cl-messagepack).  Or see the TODO list for messagepack support.

Currently this is sbcl specific, but I'd like to fix this up to work on other implementations, but it isn't top of mind.

This project works, has some test coverage, but is not finished yet.  I expect it to be polished and 1.0 released mid-January 2025.

> :warning: This is a work in progress, do not rely on it yet!

## Status

I have not cut a first release tag even though the system works well currently because I'd like to put in the pluggable versioned coding schemes first to always keep backwards compatibility with first release

## Focus
- Speed
- Compact output
- Data that has multiple and circular references within it
  - This dominates serialization time (but you can disable this feature to get crazy speeds)
  - Complex list circularity and references (other than to the head of lists, separately disableable as well)
- Versioned output
- Extensibility for user data types (with good defaults for 99% of use)
- Ability to change the coding mechanism straight-forwardly
- Should work out of the box without any effort with an easy programmer / user interface
- Stable API and no breaking changes (this is a standard Common Lisp goal)

## General features

All number types are supported, with the addition of specialized compact and fast writers for ub8, ub16, ub32, ub64, sb8, sb16, sb32, fixnum, single-float, double-float, (complex double-float), and (complex single-float).

All array types are supported, with the addition of specialized compact and very fast writers / readers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats, double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32, ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default serialize/deserializers, and also conditions.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality is preserved across serialization / deserialization and circularity is supported.  If you disable reference tracking serialization is quite fast.

Large simple-arrays have dedicated (at least on sbcl) serializers / deserializers which should be close to disk/network/memory bandwidth limited.

Support for writing and reading from files, vectors, streams, and raw memory without having to jump through hoops.

A simple versioning scheme is implemented.

## User interface
### (store place &rest data)

**place** can be string or pathname referring to a file, or a stream, or a vector, or NIL (in which case you will be returned a vector of data).

### (restore place)

**place** can be a string or pathname referring to a file, or a stream, or a vector.  For restoring to a system-area-pointer use restore-from-sap as you have to note how much data is available (good for mmap'ed files and large arrays).

### (store-to-sap sap size &rest data)
### (restore-from-sap sap size)
 If you have an mmap'ed file or a raw system-area-pointer, you can store to it and restore from it.

 For storing to one, you don't need to know the size in advance as we throw a restartable error allow you to allocate more system memory and continue on.  See tests/cl-binary-store-tests.lisp test-sap-write/read for a silly example of how to use this.  Nominally you'll be updating mmap regions of files or something.

## Examples
git clone the repo into your local quicklisp directory (usually ~/quicklisp/local-packages)

    CL-USER> (quicklisp:quickload "cl-binary-store")
    CL-USER> (in-package :cl-binary-store-user) ;  or (use-package :cl-binary-store-user)
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
    #(39 0 2 206 177) ;; 4 bytes, 39 = utf-8 string, 0 2 is encoded length = 2, 206 117 = α

    CL-BINARY-STORE-USER> (let* ((*print-circle* t)
                                 (v (make-array 1)))
                             (setf (svref v 0) v)
                             (store "blarg.bin" 'a 'b 'c v)
                             (format nil "~A" (multiple-value-list (restore "blarg.bin"))))
    "(A B C #1=#(#1#))"

To run tests you want to do

    CL-USER> (quicklisp:quickload "cl-store-binary-tests")
    CL-USER> (parachute:test 'cl-store-binary-tests)

The package :cl-binary-store-user exports all the user facing interfaces above.  The package :cl-binary-store exports all the symbols needed to add your own extensions.

### Configurable options
#### \*track-references\* default: T

Let this to NIL around your calls to store / restore if you have simple data with no references between them at all (so lists of data, no circularity, no repeated objects).  This then goes very fast (>500MB/sec / > 500 Mobjects/second for lists of numbers; > 5000 MB/sec for big simple arrays chunks).

#### \*support-shared-list-structures\* default: T

Let this to NIL if you have no list circularity (when it is NIL basic circular lists are supported as the first CONS we see in a list is recorded as a reference, so almost all data will work fine with this NIL; NIL makes this package behave like cl-store which will die / explode if given any complex circularity in a list).  Setting this to NIL is a significant performance improvement if you have list heavy data.

#### \*store-class-slots\* default: NIL

Standard object :class allocated slots will be stored if this is T.

#### \*write-magic-number\* default: NIL

If T we will write out a magic number and the \*write-version\* to the output, which will then be validated against \*supported-versions\* when read back in.

#### \*write-end-marker*\* default: NIL

If T we will write an end marker at the end of every call to STORE.  This helps for use when sending data across the network or reading from raw memory to avoid tracking lengths around.  It also means you can write multiple chunks of objects to a stream and have restore-objects return between each chunk.

## Extensions

### Changing object slot serialization

For serializing objects, the default behavior is probably good enough for 95% of users.  We provide a simple extension point with a generic function serializable-slot-info which you can change the behavior of, for example to enable calling initialize-instance instead of allocate-instance, or to not save certain slots or transform them before saving.  See comments in [objects.lisp](src/objects.lisp) and the example extension [example-extension.lisp](src/example-extension.lisp) for an example.

### Adding code points to the codespace

The storage mechanism dispatches on types and the dispatch mechanism on deserialization is byte based --- we read a byte, called a *tag* and dispatch to an appropriate reader based on it.

The notion we have for extension and versioning is a "codespace", that is a set of types and storage functions for them and a set of bytes and restore functions for them.  We start reading data with whatever the default codespace is (specified by \*read-version\*).  If we hit a magic number (+action-code+ then +magic-number-code+ followed by a coded number) we then try and load the specified codespace version, erroring if we do not support it.  Here we are versioning by codespace.  You can easily extend the actions to do whatever further versioning you want of the file.  See [magic-numbers.lisp](src/magic-numbers.lisp) and [actions.lisp](src/actions.lisp).  During writing you would specify what codespace you want with \*write-version\*.

For an example, look at [example-extension.lisp](src/example-extension.lisp) and [example-extension-codespace.lisp](src/example-extension-codespace.lisp) which I reproduce here:

    (define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
      (defstore something-else (store-something-else obj storage store-object))
      (defrestore +test-code+ (restore-something-else restore-object)))

Note that you can also define local reference tracking hash-tables with register-references.

Here we are definining how to store an object of type SOMETHING-ELSE (which is defined in [example-extension.lisp](src/example-extension.lisp)).  We define our type *tag* +test-code+ and we write it out in #'store-something-else along with whatever else we feel like.  Then on restore we call (restore-something-else restore-object) when we see +test-code+.  restore-object is a function we can call to restore further objects from the data stream.  See [basic-codespace.lisp](src/basic-codespace.lisp) for lots of examples.

Anyhow, here is the result of running the code

    CL-USER> (quicklisp:quickload "example-extension")
    CL-USER> (example-extension:test-serializable-slot-info)
    Success!
    CL-USER> (example-extension:test-special-serializer/deserializer)
    Example of writing something completely different for a 'something-else object:
    First example writing a version number into the stream to switch codespaces
    Switching codespace from basic codespace to #x9999 (extension-codespace)
    Hi, I decided to write this instead of a 'something-else
    But actually, it told me to tell you:
    Hi from slot information!
    Second example forcing the right codespace
    Hi, I decided to write this instead of a 'something-else
    But actually, it told me to tell you:
    Hi from slot information!
    "And here is a bonus thing returned to you"

## Why?

I've been using [cl-store](https://cl-store.common-lisp.dev/) forever and it works well and meets most needs once you tweak how it serializes a bunch of things for speed.  I am usually serializing / deserializing > 1GB of data and in the end all the tweaks I've done don't make it fast enough.  The original cl-store hits about 1-2MB/sec for store and restore on the data set I use which just is too slow. It also doesn't support complex list circularity in a reasonable way (the correct-list-store you can turn on blows the stack).

- It uses read-byte / write-byte to emit data which is OK-ish as long as you are using file streams in sbcl, but once you plug in a generic stream it crawls.
- The file format uses implicit referencing which makes it hard to deserialize quickly as each object must be tracked.  There was no easy way to to turn on and off what objects we should track references for (and it must match during store and restore).  It also tracks references for single floats which triggers a severe performance issue, at least on sbcl, related to eql hash tables which I have not tracked down..

I have also used [hyperluminal-mem](https://github.com/cosmos72/hyperluminal-mem), which is really fast if you meet the restrictions it has.  It does not support delayed object construction or references really, nor a nice default for structures and classes (though you can extend it with manual referencing on a per type basis and with serializers for each structure or class you want).

They are extensible, but not enough --- you cannot completely change the coding mechanism.

## What is different here?

### Explicit reference scheme

To make deserialization fast, we break the symmetry of store and restore by not using an implicit referencing scheme.  We add a reference counting pass across the input data where we find out what objects are multiply referenced.  Then on the actual write pass we write the references out when we first see them.  This speeds up restoration by about 10x.  Serialization with referencing is hash-table table lookup/update dominated.

With explicit referencing we can do some parallelization during restore (things would block if we don't have reference resolved yet, but we can use fixups for that), though I haven't implemented it because it's already reasonably fast (it averages 20MB/sec on my data set)... see TODO.

### Performance during serialization

Performance during serialization is dominated by the reference hash table building and use.  This is quite hard to improve as the hash table is an eq table.  To enable parallelization on the serialization side would require hacking the internals of the system.

If your data does not contain multiple references or repeated objects (in particular, repeated symbols will be stored repeatedly!), then you can let \*track-references\* to NIL and you can hit hundreds of MB/sec depending on your data (unicode strings are currently a bit slow as we
are not using a fast utf-8 encoder) and 500M cons+integer objects per second.  It's about 10-30x faster than cl-store in this mode.  This package is between 3x faster and 3x slower than hyperluminal-mem depending on data patterns both on store and restore.  If you are storing simple arrays / matrices, you want to use cl-binary-store.

### Easily versionable and extensible

See the example in extensions earlier.

## Compressed output

I suggest just piping the output through gzip.  Otherwise you can use [deoxybyte-gzip](https://github.com/keithj/deoxybyte-gzip) but warning it has a small bug in read-sequence, so use the version [here](https://github.com/ajberkley/deoxybyte-gzip).

## Benchmarking

See [benchmarking.md](benchmarking.md).

## TODO
- [X] Pluggable versioned coding schemes
  - [X] codespace optimized for highly referential data (tag bits mainly used for references instead of 8 bit tag codes) (in progress)
  - [ ] messagepack (for example; for a limited subset of types)
- [ ] Support for other Common Lisps aside from sbcl
  - [ ] At least basic slow support
  - [ ] Optimized support
- [ ] Further discoveries from real world use cases (that is, my use case)
  - [ ] Add restarts to handle missing packages / moved symboles during restore (create-package / rehome / discard)
  - [ ] Build compacted hash tables after the reference counting
- [ ] Faster UTF-8 encoding / decoding (currently doing extra copy using sb-ext string-to-octets / octets-to-string)  (look at what hyperluminal-mem does or find some package somewhere)
- [X] Always use references for symbols even when *track-references* nil.
- [X] small integer immediate storage

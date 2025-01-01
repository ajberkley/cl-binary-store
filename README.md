# cl-binary-store

A fast and customizable serializer/deserializer of Common Lisp objects to/from a binary format.  Equality of objects, circular references, and the full Common Lisp type system are supported.

> :warning: Note that this is not a serializer/deserializer for communication with other types of systems *yet*.  If you want that maybe use JSON or if you want a binary format maybe [cl-conspack](https://github.com/conspack/cl-conspack) or [cl-messagepack](https://github.com/mbrezu/cl-messagepack).  Or see the TODO list for messagepack support.

Currently cl-binary-store runs on SBCL, ECL, CCL, and Allegro Common Lisp.  The SBCL implementation is the fast one.  Neither ECL, CCL, nor Allegro have fast serializers / deserializers for simple-arrays and general performance is terrible (Allegro is 10x slower than SBCL, the others 100x; similar for cl-store and cl-conspack).  ABCL doesn't work yet either, but I will spend some more time on it.

## Status

Everything seems to work well, but I'm not cutting a first release tag until I get some mileage in my production use case.  Once release, backwards compatibility will be guaranteed (the versioned codespaces should make that straightforward enough).

I expect it to be polished and 1.0 released mid-January 2025.

> :warning: This is still very much a work in progress, do not rely on it yet!

## Focus
- Speed and compact output
- Data/objects that have multiple and circular references within them
  - List circularity and general circular references between objects
  - Preservation of equality amongst the serialized objects (objects are eq de-duplicated and referred to by references in the output)
- Should do the right thing out of the box for structure-objects and standard-objects, but be extensible when you want to do more
- Versioned output
- Ability to change the coding mechanism straight-forwardly
- Should work out of the box without any effort with an easy programmer / user interface
- Stable API and no breaking changes (this is a standard Common Lisp goal)

## General features

All number types are supported, with the addition of specialized compact and fast writers for ub8, ub16, ub32, ub64, sb8, sb16, sb32, fixnum, single-float, double-float, (complex double-float), and (complex single-float).

All array types are supported, with the addition of specialized compact and very fast writers / readers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats, double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32, ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default serialize/deserializers, and also conditions can be serialized/deserialized.  The object serialization / deserialization is customizable.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality among the restored objects is preserved across serialization / deserialization and circularity is supported.

Large simple-arrays have dedicated (at least on sbcl) serializers / deserializers which should be close to disk/network/memory bandwidth limited.

Support for writing and reading from files, vectors, streams, and raw memory without having to jump through hoops.

A simple versioning scheme is implemented.

If you disable reference tracking, serialization is quite fast, and otherwise serialization is dominated by #'eq hash table performance.

## User interface
#### (store place &rest data)

**place** can be string or pathname referring to a file, or a stream, or a vector, or NIL (in which case you will be returned a vector of data).

#### (restore place)

**place** can be a string or pathname referring to a file, or a stream, or a vector.  For restoring to a system-area-pointer use restore-from-sap as you have to note how much data is available (good for mmap'ed files and large arrays).

#### (store-to-sap sap size &rest data)
#### (restore-from-sap sap size)
 If you have an mmap'ed file or a raw system-area-pointer, you can store to it and restore from it.

 For storing to one, you don't need to know the size in advance as we throw a restartable error allow you to allocate more system memory and continue on.  See tests/cl-binary-store-tests.lisp test-sap-write/read for a silly example of how to use this.  Nominally you'll be updating mmap regions of files or something.

## Examples
git clone the repo into your local quicklisp directory (usually ~/quicklisp/local-packages)

    CL-USER> (quicklisp:quickload "cl-binary-store")
    CL-USER> (in-package :cl-binary-store-user) ;  or (use-package :cl-binary-store-user)
    CL-BINARY-STORE-USER> (store nil (list "abcd" 1234))
    #(4 30 212 97 98 99 100 4 1 210 4 5)
    ;; 4 = cons, 30 = simple-string, 212 = tiny integer length 4, 97 98 99 100 = abcd, 4 = cons 1 210 4 = 16 bit integer 1234, 5 = nil
    
    CL-BINARY-STORE-USER> (restore (store nil (list "abcd" 1234)))
    ("abcd" 1234)

    CL-BINARY-STORE-USER> (store "blarg.bin" 'hi)
    "blarg.bin"
    CL-BINARY-STORE-USER> (restore "blarg.bin")
    HI
    CL-BINARY-STORE-USER> (store nil (make-string 1 :initial-element #\U+03b1))
    #(31 210 206 177) ;; 31 is simple-string, 210 encoded length = 2, 206 117 = α

    CL-BINARY-STORE-USER> (let* ((*print-circle* t)
                                 (v (make-array 1)))
                             (setf (svref v 0) v)
                             (store "blarg.bin" 'a 'b 'c v)
                             (format nil "~A" (multiple-value-list (restore "blarg.bin"))))
    "(A B C #1=#(#1#))"

To run tests you want to do

    CL-USER> (quicklisp:quickload "cl-store-binary/tests")
    CL-USER> (parachute:test 'cl-store-binary-tests)

The package :cl-binary-store-user exports all the user facing interfaces above.  The package :cl-binary-store exports all the symbols needed to add your own extensions.

## Missing objects, structures, and symbols

If you keep files around long enough, eventually you find you have stored stuff you don't remember.  It's nice if you don't get horrible errors while loading the files.  cl-binary-store provides a good set of restarts for missing packages (create-package, rehome symbol) and for missing objects or structures (create them, use a different class) or for changes in slots (discard, change slot name).  The deserialization is extensible enough that you can put in line upgrading of objects.

## Extending object serialization

For serializing objects, the default behavior is probably good enough for 95% of users.  There are four further methods of extension provided at with increasing degrees of complexity and control.

#### \*store-class-slots\* default: NIL

Standard object :class allocated slots will be serialized / deserialized if this is T

#### (defmethod SERIALIZABLE-OBJECT-INFO (type))

Must return two values.  The first value must be a list of slot-names (symbols) which should be serialized for this object.

The second value may be NIL or a function which will be called with each slot-name and slot-value and should return a serializable object (like nil) for that slot.

For example:

    (defstruct blarg
      (a-may-contain-sensitive-info)
      (b-do-not-serialize))

    ;; Just say do not serialize b-do-not-serialize
    (defmethod serializable-object-info ((type (eql 'blarg)))
      (values (list 'a-may-contain-sensitive-info) nil))

    ;; If you use the above technique for a structure-object, you may want
    ;; to provide a `specialized-object-constructor' too as the
    ;; unserialized slots are undefined.  For a standard-object they are
    ;; just unbound which is fine, but for a structure-object it's
    ;; undefined what they are.

    ;; Or if you want to filter out sensitive information on a per object basis:
    (defmethod serializable-object-info ((type (eql 'blarg)))
      (values
        (call-next-method) ;; all the slots of 'blarg
        (lambda (slot-name slot-value)
          (if (sensitive-information-p slot-name slot-value)
              "censored"
              slot-value))))

#### (defmethod SPECIALIZED-OBJECT-CONSTRUCTOR (type))

This may return a FUNCTION which will be used to build an object from restored slot values.  The FUNCTION will be called with parameters OBJECT-INFO and SLOT-VALUES.  Object-info is a structure defined in (objects.lisp)[src/objects.lisp]

The default object constructor calls ALLOCATE-INSTANCE and then populates slots with their values.  Perhaps you really want initialize-instance called.  So then you could do:

    (defmethod specialized-object-constructor ((type (eql 'my-type)))
      (lambda (object-info slot-values)
        (apply #'make-instance (object-info-type object-info)
                               (garble (object-info-slot-names object-info) slot-values))))

In rare cases if your slot-values will have references back to your object you may find a slot-value that is a FIXUP object.  See (referrers-and-fixup.lisp)[src/referrers-and-fixup.lisp] restore-object-to for how to deal with this (you have to register a callback for when the object is finally reified to update whatever slot you have).  This will only happen if you have as a slot value a reference to a displaced array which is displaced to an array which has a reference back to your object (the challenge here is one cannot build a displaced array without knowing what it is displaced to).

#### (defmethod SPECIALIZED-SERIALIZER/DESERIALIZER (type))

This method should return as two values two functions which will replace the default serialization / deserialization of objects of that type.  If you do this, you probably want to define a new codespace anyway, so you could just do it directly with defstore / defrestore functions, but there is no penalty to doing it this way.

The specialized-serializer function will be called with parameters (object storage eq-refs store-object assign-new-reference-id) which should have the side effect of modifying storage, eq-refs, calling store-object and or assign-new-reference-id. Correspondingly, the specialized-deserializer function will be called with (storage restore-object)

### Configurable options
#### \*track-references\* default: T

Let this to NIL around your calls to store / restore if you have simple data with no references between them at all (so lists of data, no circularity, no repeated objects).  This then goes very fast (>200MB/sec / > 200 Mobjects/second for lists of numbers; > 5000 MB/sec for big simple arrays chunks).

#### \*support-shared-list-structures\* default: T

Let this to NIL if you have no list circularity (when it is NIL basic circular lists are supported as the first CONS we see in a list is recorded as a reference, so almost all data will work fine with this NIL; this is the same as cl-store:\*precise-list-storage\*. Setting this to NIL is a significant performance improvement if you have list heavy data.

#### \*write-magic-number\* default: NIL

If T we will write out a magic number and the \*write-version\* to the output, which will be used on restore to load the correct codespace (or error if it does not exist).

#### \*write-end-marker*\* default: NIL

If T we will write an end marker at the end of every call to STORE.  This helps for use when sending data across the network or reading from raw memory to avoid tracking lengths around.  It also means you can write multiple chunks of objects to a stream and have restore-objects return between each chunk.

### Adding code points to the codespace

The storage mechanism dispatches on types and the dispatch mechanism on deserialization is byte based --- we read a byte, called a *tag* and dispatch to an appropriate reader based on it.

The notion we have for extension and versioning is a "codespace", that is a set of types and storage functions for them and a set of bytes and restore functions for them.  We start reading data with whatever the default codespace is (specified by \*read-version\*).  If we hit a magic number (+action-code+ then +magic-number-code+ followed by a coded number) we then try and load the specified codespace version, erroring if we do not support it.  Here we are versioning by codespace.  You can easily extend the actions to do whatever further versioning you want of the file.  See [magic-numbers.lisp](src/magic-numbers.lisp) and [actions.lisp](src/actions.lisp).  During writing you would specify what codespace you want with \*write-version\*.

For an example, look at [example-extension.lisp](src/example-extension.lisp) and [example-extension-codespace.lisp](src/example-extension-codespace.lisp) which I reproduce here:

    (define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
      ;; Disable storing and loading of double-floats because we hate them or something
      (delete-store double-float)
      (delete-restore cl-binary-store::+double-float-code+)
      ;; Add low level support for something-else objects
      (defstore something-else (store-something-else obj storage store-object))
      (defrestore +test-code+ (restore-something-else restore-object)))

Here we are definining how to store an object of type SOMETHING-ELSE (which is defined in [example-extension.lisp](src/example-extension.lisp)).  We define our type *tag* +test-code+ and we write it out in #'store-something-else along with whatever else we feel like.  Then on restore we call (restore-something-else restore-object) when we see +test-code+.  restore-object is a function we can call to restore further objects from the data stream.  See [basic-codespace.lisp](src/basic-codespace.lisp) for lots of examples.

Anyhow, here is the result of running the code

    CL-USER> (quicklisp:quickload "example-extension")
    CL-USER> (example-extension:test-serializable-object-info)
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

The codespace can nominally be changed multiple times in a file if needed.

As a further example, we decided to disable double-float storing and restoration because we think they are dangerous and too precise for normal human brains to handle.  In the example-extension file there is an example:

    CL-USER> (example-extension:test-unable-to-restore-double-floats)
    Successfully denied codespace switching!
    Error was: Switching codespace away from #x9999 (extension-codespace) is DISALLOWED
    Successfully read double-float when we were allowed to!

## Why?

I've been using [cl-store](https://cl-store.common-lisp.dev/) forever and it works well and meets most needs once you tweak how it serializes a bunch of things for speed.  I am usually serializing / deserializing > 1GB of data and in the end all the tweaks I've done don't make it fast enough.  The original cl-store hits about 1-2MB/sec for store and restore on the data set I use which just is too slow.

- It uses read-byte / write-byte to emit data which is OK-ish as long as you are using file streams in sbcl, but once you plug in a generic stream it crawls.
- The file format uses implicit referencing which makes it hard to deserialize quickly as each object must be tracked.  There was no easy way to to turn on and off what objects we should track references for (and it must match during store and restore).  It also tracks references for single floats which triggers a severe performance issue, at least on sbcl, related to eql hash tables which I have not tracked down..

I have also used [hyperluminal-mem](https://github.com/cosmos72/hyperluminal-mem), which is really fast if you meet the restrictions it has.  It does not support delayed object construction or references really, nor a nice default for structures and classes (though you can extend it with manual referencing on a per type basis and with serializers for each structure or class you want).

I will admit when I started this project, I didn't know about cl-conspack.  It is faster than cl-store and generates nice compact output (until you start throwing many instances of structs or classes at it)!  It still requires extending for simple objects and structures and doesn't handle repeated structures efficiently, both of which are problems for me.

They are extensible, but not enough to make them into what I need.

## What is different here?

### Explicit reference scheme

To make deserialization fast, we break the symmetry of store and restore by using an explicit referencing scheme (as opposed to an implicit one, like is used in cl-store).  We add a reference counting pass across the input data where we find out what objects are multiply referenced.  Then on the actual storage pass we write out a new-reference-indicator just before serializing an object that will be referred to again.  We will then refer to this object by its reference-id (implicitly counted during restore) in the future in the file.  Knowing which objects will be multiply referenced in advance speeds up restoration by about 10x compared with cl-store.  If we didn't know this, then during deserialization we would have to keep track of every object we have deserialized (which makes it as hard as serialization).  Serialization with referencing is #'eq hash-table table lookup/update time dominated.  See [referrers-and-fixup.lisp](src/referrers-and-fixup.lisp) for some discussion.

### Performance during serialization

Performance during serialization is dominated by the reference hash table building and use.  This is quite hard to improve as the hash table is an eq table, so I can't just plug something else in.  You can hint the size of your data to save a lot of hash table growing and resizing if you have a large data set.

If your data does not contain multiple references or repeated objects (in particular, repeated symbols will have their names and packages stored repeatedly!), then you can let \*track-references\* to NIL and you can hit hundreds of MB/sec depending on your data (unicode strings are currently a bit slow as we are not using a fast utf-8 encoder) and 200+M cons+integer objects per second.  It's about 10-30x faster than cl-store in this mode.  This package is between 100x faster and 3x slower than hyperluminal-mem depending on data patterns both on store and restore.  If you are storing simple arrays / matrices, you want to use cl-binary-store.

### Easily versionable and extensible

See the example in extensions earlier.

## Compressed output

I suggest just piping the output through gzip if you need the smallest possible files, though the output is reasonably compact as is.  Otherwise you can use [deoxybyte-gzip](https://github.com/keithj/deoxybyte-gzip) but make sure to use the most up to date version.

## Debugging

We generate the codespace code through a maze of macros and functions in [codespaces.lisp](src/codespaces.lisp), so if something isn't doing what you want, it is easiest to inspect cl-binary-store::\*codespaces\* and look at the codespace objects that are built and then look at the slots RESTORE-OBJECTS-SOURCE-CODE and STORE-OBJECTS-SOURCE-CODE (which are what are used to build the restore-objects and store-objects functions in the codespace).  These can be compiled at the repl or put into a file and compiled so that you can get full debugging of store-objects / restore-objects.

## Benchmarking

For my use case (>1GB of messy data and objects), cl-binary-store is almost 10x faster than CL-STORE on restore, and 2.5x faster for storing.  It's comparable to hyperluminal-mem when not tracking references / circularity.

See [benchmarking.md](benchmarking.md).

## TODO ideas
- [ ] A codespace for messagepack (for example; for a limited subset of types).  This is mainly as an exercise as cl-messagepack is fine.
- [ ] Faster UTF-8 encoding / decoding (currently doing extra copy using sb-ext string-to-octets / octets-to-string)  (look at what hyperluminal-mem does or find some package somewhere)
- [ ] Speed up cl-binary-store on Allegro Common Lisp and ABCL (and maybe on ECL and CCL?)

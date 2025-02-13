# cl-binary-store (v1.1)

A super fast and customizable serializer/deserializer of Common Lisp objects to/from a very compact binary format.  Equality of objects, circular references, and the full Common Lisp type system are supported.  Specialized arrays (on SBCL) are stored/restore at lightning speed.

> :warning: Since we support the full Common Lisp type system (in the default codespace), this is not a serializer/deserializer for communication with other types of systems.  If you want that maybe use JSON or if you want a binary format maybe [cl-conspack](https://github.com/conspack/cl-conspack) or [cl-messagepack](https://github.com/mbrezu/cl-messagepack).  Or see the TODO list for conspack or messagepack support.

cl-binary-store works on 64-bit SBCL, ECL, CCL, ABCL, Allegro Common Lisp, and Lispworks.  The SBCL implementation is the fast one.  If you care about one of the platforms tell me and I can make it faster.  Binary files are compatible between different lisps.

## Focus
- Speed and compact output
- Data/objects that have multiple and circular references within them
  - Circular references between objects
  - Preservation of equality amongst the serialized objects (objects are eq de-duplicated and referred to by references in the output)
  - Circular lists and lists with shared structure
- Do the right thing out of the box for structure-objects and standard-objects, but be extensible for those who want to do more
- Versioned output
- Ability to change the coding mechanism straight-forwardly, but also have room in the basic codespace for user tags
- Should work out of the box without any effort with an easy programmer / user interface (no need to write code for each class/struct you use!)
- Stable API and no breaking changes (this is a standard Common Lisp goal)
- Ability to limit amount of data written or read (safety rails)
- Safe from malicious input (some amount of fuzz testing and code reading done, but if you want to rely on safety, please contribute!)

## General features

All number types are supported, with the addition of specialized compact and fast writers for ub8, ub16, ub32, ub64, sb8, sb16, sb32, fixnum, single-float, double-float, (complex double-float), and (complex single-float).

All array types are supported, with the addition of specialized compact and very fast writers / readers for vectors and arrays of simple-bit-vectors, simple-base-string, single-floats, double-floats, fixnums, sb8, sb16, sb32, sb64, ub2, ub4, ub7, ub8, ub15, ub16, ub31, ub32, ub62, and ub64 (all these being supported by SBCL).

structure-objects and standard-objects have good default serialize/deserializers, and also conditions can be serialized/deserialized.  The object serialization / deserialization is customizable.

symbols, hash-tables, and pathnames are supported.

Multiply referenced objects are stored as references so equality among the restored objects is preserved across serialization / deserialization and circularity is supported.

Large simple-arrays have dedicated (on sbcl) serializers / deserializers which are close to disk/network/memory bandwidth limited.

Support for writing and reading from files, vectors, streams, and raw memory without having to jump through hoops.

A simple versioning scheme is implemented.

A range of tag numbers are reserved for users if they want more efficient or compact output.

## User interface
#### (store place data &key track-references support-shared-list-structures max-to-write as-separate-objects output-end-marker output-magic-number write-version load/save-progress-indicator)

**place** can be string or pathname referring to a file, or a stream, or a vector, or NIL (in which case you will be returned a vector of data).  Track-references is T by default and ensures if two objects are eql in data, they will be eql in the restored output (that is, it allows you to have references between objects in your data).  Support-shared-list-structures is NIL by default and if T allows you to share tails of lists or have circular lists in your data.  Max-to-write is a number in bytes which limits the output size, it defaults to 10GB.  As-separate-objects specifies that data is a (short) list of objects and they should be written so a subsequent call to restore returns them as multiple values.  Output-end-marker means that an end marker will be written on the output which is useful if working with raw buffers or sending stuff on the network.  Output-magic-number means a magic/version number equal to write-version will be written on the output which will instruct the system how to restore the data (or be validated against the allowed encodings).  Write-version is the codespace version to write with, the default being the basic-codespace which supports all Common Lisp objects.  Load/save-progress-indicator prints some stuff so you can tell what is happening in case you have huge complex data sets and get bored. 

#### (restore place &key allow-codespace-switching max-to-read read-version load/save-progress-indicator)

**place** can be a string or pathname referring to a file, or a stream, or a vector.  For restoring to a system-area-pointer use restore-from-sap as you have to note how much data is available (good for mmap'ed files and large arrays).

Max-to-read specifies a number in bytes to signal an error after during restore (to prevent poisonous data files).  Allow-codespace-switching means if the file has a magic-number/version indicator that does not match read-version, we will switch to the correct version (if it exists), otherwise we signal an error.  Read-version is the default codespace number to use while restoring (the default is the basic-codespace that supports the full Common Lisp type space).  load/save-progress-indicator is for when you are bored.

#### (store-to-sap sap size &rest data)
#### (restore-from-sap sap size)
 If you have a raw system-area-pointer on sbcl (or a sap pointer to a mmap'ed file), or a vector from the static-vectors package, you can store to it and restore directly from it.

 For storing to one, you don't need to know the size in advance as we throw a restartable error allow you to allocate more system memory and continue on.  See tests/cl-binary-store-tests.lisp test-sap-write/read for a silly example of how to use this.  Nominally you'll be updating mmap regions of files or something.

## Examples
git clone the repo into your local quicklisp directory (usually ~/quicklisp/local-packages)

    CL-USER> (quicklisp:quickload "cl-binary-store")
    CL-USER> (in-package :cl-binary-store-user) ;  or (use-package :cl-binary-store-user)
    CL-BINARY-STORE-USER> (store nil (list "abcd" 1234))
    #(32 6 30 8 97 98 99 100 1 210 4 5)    
    ;; 32 = proper list, 6 = integer length 2, 30 = simple-string, 8 = integer length 4, 97 98 99 100 = abcd, 4 = cons 1 210 4 = 16 bit integer 1234, 5 = nil
    
    CL-BINARY-STORE-USER> (restore (store nil (list "abcd" 1234)))
    ("abcd" 1234)

    CL-BINARY-STORE-USER> (store "blarg.bin" 'hi)
    "blarg.bin"
    CL-BINARY-STORE-USER> (restore "blarg.bin")
    HI
    CL-BINARY-STORE-USER> (store nil (make-string 1 :initial-element #\U+03b1))
    #(30 6 206 177) ;; 31 is simple-string, 6 encoded length = 2, 206 117 = α

    CL-BINARY-STORE-USER> (let* ((*print-circle* t)
                                 (u (make-array 1))
                                 (v (make-array 1 :initial-element u)))
                             (setf (svref u 0) v)
                             (store "blarg.bin" (list 'a 'b 'c u v) :as-separate-objects t)
                             (format nil "~A" (multiple-value-list (restore "blarg.bin"))))
    "(A B C #1=#(#2=#(#1#)) #2#)"
    ;; Below we enable the optional list circularity detection (general circularity as above is supported by default)
    CL-BINARY-STORE-USER> (let* ((*support-shared-list-structures* t)
                                 (*print-circle* t)
                                 (a (list 1 2 3)))
                            (setf (cdr (last a)) a)
                            (restore (store nil a)))
    #1=(1 2 3 . #1#)

To run tests you want to do

    CL-USER> (quicklisp:quickload "cl-store-binary/tests")
    CL-USER> (parachute:test 'cl-store-binary-tests)

The package :cl-binary-store-user exports all the user facing interfaces above.  The package :cl-binary-store exports all the symbols needed to add your own extensions.

## Missing objects, structures, and symbols

If you keep files around long enough, eventually you find you have stored stuff you don't remember.  It's nice if you don't get horrible errors while loading the files.  cl-binary-store provides a good set of restarts for missing packages (create-package, rehome symbol) and for missing objects or structures (create them, use a different class) or for changes in slots (discard, change slot name).  The deserialization is extensible enough that you can put in line upgrading of objects.

The two conditions signalled here are of type MAYBE-EXPECTED-ERROR and INVALID-INPUT-DATA and are MISSING-SLOT and OBJECT-TYPE-NOT-FOUND

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

All of these can be set in the calls to STORE and RESTORE directly via keyword parameters with the same names, the global variables are just the default values if not specified.

#### \*track-references\* default: T

If you have simple data with no references between them at all (so lists of data, no circularity, no repeated objects).  This then goes very fast (>200MB/sec / > 200 Mobjects/second for lists of numbers; > 5000 MB/sec for big simple arrays chunks).  Even with this T speed is quite good.

#### \*support-shared-list-structures\* default: NIL

Set this to T if you have complex list structures where you share tails of lists, or have any list only circularity (circularity between lists, objects, etc in general is handled by \*track-references\*, this setting is just for conses.  Most of the time this is fine to leave at nil.  It's a significant performance hit if you set it to T as we have to track every cons we see.  If you set this to T, *track-references* must be T too.

#### \*output-magic-number\* default: NIL

If T we will write out a magic number and the \*write-version\* to the output, which will be used on restore to load the correct codespace (or error if it does not exist).

#### \*output-end-marker*\* default: NIL

If T we will write an end marker at the end of every call to STORE.  This helps for use when sending data across the network or reading from raw memory to avoid tracking lengths around.

#### \*max-to-write\* default: 10GB

The default limit on the amount of data we will write.  Maybe overridden in each call to store.  This is approximate (it is checked every 8K or so).

#### \*max-to-read\* default: 2GB

The default amount of data we will restore.  This is approximate, it is checked every 8K or so (or on a big cons'ing operation).

#### \*allow-codespace-switching*\* default: NIL

This adjusts the codespace based on what the restored file says it was written with instead of just using \*read-version\*

#### \*read-version\* default: #x0001 the basic codespace for all Common Lisp objects

This can be used to override the restoration with a user provided codespace in case the version was not written to the file.

#### \*write-version\* default: #x0001 the basic codespace for all Common Lisp objects

Specify what codespace to use during writing.  Use \*output-magic-number\* so the file records what was used during writing.

### Conditions and malicious input

cl-binary-store attempts to handle both malicious input and corrupted input reasonably.  There is by default a \*max-to-read\* of 2GB which will prevent the equivalent of zip bombs, and I have done some fuzz testing so that in general one expects to see an INVALID-INPUT-DATA error signalled if there is bad input data as opposed to crashing.  There are two types of errors one might expect, MISSING-SLOT and OBJECT-TYPE-NOT-FOUND which inherit from MAYBE-EXPECTED-ERROR which is of type INVALID-INPUT-DATA.  This allows you to either catch all INVALID-INPUT-DATA (if you just want things to work) or all INVALID-INPUT-DATA except MAYBE-EXPECTED-ERRORs (if you want some interactive recovery).  If you actually have corrupted input and wish to recover it, I suggest adding :debug-cbs to \*features\*, recompiling, and pulling the partial data out of the debugger where some of the data will be available on the stack.  It is too complicated to support corrupted data recovery and maintain high performance.

### Extending the codespace

A codespace is a definition of the binary file format, they are identified with a magic / version number.  At write time the codespace is identified by \*write-version\*.  The codespace can optionally be written out to the output (\*write-magic-number\*).  Currently we have baked in a notion of tag bytes between objects that identify the type of the next object --- you could presumably switch to whatever tagging scheme you want with a bit of work on the code generation side.  We automatically build the storage time typecase dispatch, provide the basics of reference tracking, and some other niceties, and as well a dispatch case statement during restore.  This code is specialized for each codespace and built at compile / load time.  This can lead to some complexities debugging as the source code is not accessible.  To alleviate this one may define-codespace with :debug t, in which case the store and restore functions that are built are dumped to a file "codespace-debug.lisp" and loaded so the usual nice Common Lisp debugging experience can occur.  Usually you want to inline many of your functions for performance reasons (especially if you have regular data, the inlining, at least on sbcl, allows very nice performance as, for example, the first restore-object call from inside a wrapper function can be inlined --- so the list restore, for example, is not bouncing back and forth between functions).

During restore, we start reading data with whatever the default codespace is (specified by \*read-version\*).  If we hit a magic number (+action-code+ then +magic-number-code+ followed by a coded number) we then try and load the specified codespace version, erroring if we do not support it (or if codespace switching is disallowed for security reasons).  You can easily extend the actions to do whatever further versioning you want of the file.  See [magic-numbers.lisp](src/magic-numbers.lisp) and [actions.lisp](src/actions.lisp).  

In cl-binary-store we provide a codespace that is my best guess at a general purpose very dense and fast binary encoding for all Common Lisp types in [basic-codespace.lisp](src/basic-codespace.lisp).  You may wish to extend that or specialize it to more specific types.  For example, you might want to store (simple-array (signed-byte 8) 4) specially for some odd reason, or to serialize a structure in a very dense format.  To do so you would do something like what is in [example-extension-codespace.lisp](src/example-extension-codespace.lisp).

For an example, look at [example-extension.lisp](src/example-extension.lisp) and [example-extension-codespace.lisp](src/example-extension-codespace.lisp) which I reproduce here.  The define-codespace environment provides an imperative means of describing the codespace.  We define restore routines with defrestore, store routines with defstore, global state with register-global-state, reference-tables for reference tracking with register-reference-table.  Within the define-codespace environment we have several globally defined names, OBJ, STORAGE, STORE-OBJECT, RESTORE-OBJECT

    (define-codespace ("extension-codespace" +extension-codespace+ :inherits-from +basic-codespace+)
      ;; Disable storing and loading of double-floats because we hate them or something
      (delete-store double-float)
      (delete-restore cl-binary-store::+double-float-code+)
      ;; Add low level support for something-else objects
      (defstore something-else (store-something-else obj storage store-object))
      (defrestore +test-code+ (restore-something-else restore-object)))

Here we are definining how to store an object of type SOMETHING-ELSE (which is defined in [example-extension.lisp](src/example-extension.lisp)).  We define our type *tag* +test-code+ and we write it out in #'store-something-else along with whatever else we feel like.  Then on restore we call (restore-something-else restore-object) when we see +test-code+.  restore-object is a function we can call to restore further objects from the data stream.  See [basic-codespace.lisp](src/basic-codespace.lisp) for lots of examples.

Anyhow, here is the boring result of running the code in [example-extension.lisp](src/example-extension.lisp]

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

We generate the codespace code through a maze of macros and functions in [codespaces.lisp](src/codespaces.lisp), so if something isn't doing what you want, it is easiest to inspect cl-binary-store::\*codespaces\* and look at the codespace objects that are built and then look at the slots RESTORE-OBJECTS-SOURCE-CODE and STORE-OBJECTS-SOURCE-CODE (which are what are used to build the restore-objects and store-objects functions in the codespace).  These can be compiled at the repl or put into a file and compiled so that you can get full debugging of store-objects / restore-objects.  To improve the debugging experience you can specify :debug t in [basic-codespace.lisp](src/basic-codespace.lisp) which will emit the code to a file for you so you get the full debugging experience.  Pushing :debug-cbs to \*features\* will also help.

## Basic codespace and user codes

The basic codespace (the default) has magic number #x001 and uses all 8 bit codes up to 224 inclusive.  User space for codes is 225-250 inclusive, and codes 251-255 are reserved for extensions.  See [example-extension-codespace.lisp](src/example-extension-codespace.lisp) for how to add user codes.

## Benchmarking

For my use case (>1GB of messy data and objects), cl-binary-store is almost 10x faster than CL-STORE on restore, and 2.5x faster for storing.  It's comparable to hyperluminal-mem when not tracking references / circularity.

See [benchmarking.md](benchmarking.md).

## TODO ideas
- [ ] Speed up cl-binary-store on ABCL and ECL so it is less than 10x slower than on SBCL
- [ ] Handle specialized multi-dimensional array data on non-SBCL faster.  See babel for all the variants on with-array-data
- [ ] Faster standard-object serialization / deserialization using direct slot location accessors


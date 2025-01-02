# Benchmarking

What's the fun of writing fast code if you cannot show it off?

The comparables sorted by fastest to slowest: HYPERLUMINAL-MEM, CL-CONSPACK and CL-STORE. 

hyperluminal-mem is currently broken in quicklisp (though you can hit accept a couple times to get it compiled and running).

In terms of feature comparable, cl-binary-store (and cl-store) has the best behavior out of the box without requiring anything of the user.  cl-conspack is really nice too, but has some different goals, and it requires adding specialized serializers for each structure or object you use.  It is faster than cl-store which is nice, but not fast enough, and as noted earlier doesn't work well with data containing many instances of structures or classes.  Hyperluminal-mem is a very optimized binary serializer made to write to SAP buffers directly.  It also requires writing code for every struct or object you want to serialize.  It does not do reference or circularity tracking at all.  Hyperluminal-mem is currently broken in quicklisp unfortunately, but you can still load it by accepting the compilation errors (it's problems in stm library).  It's my benchmark for zippy raw data serialization.  We are almost as fast as it in many cases, faster in others, slower in others.

All the tests here are mainly using SBCL as that is (by far) the fastest implementation.  There is a section at the end where I run some on ECL, CCL, and Allegro.

## Reference tracking disabled

There are three modes we do benchmarking in.  The first is with \*track-references\* nil, which means that every piece of data is considered unique and there is no circular or multiple referencing supported.  This is the only mode that hyperluminal-mem runs in.  Conspack and cl-store can be run in a mode like this as well, so the comparisons are fair.  The second is with \*track-references\* t and \*support-shared-list-structures\* nil, which is comparable to the default behavior of cl-store.  Conspack when not tracking-refs does determine lengths of lists in advance, but will explode if you give it a circular list; so either it does full circularity tracking or none.  So it's hard to do apples to apples here.  The third is with \*track-references\* t and \*support-shared-list-structures\* t which is comparable to the behavior of conspack when conspack:tracking-refs is enabled.

### Tiny numbers

Note that here we are writing out 1M conses and 1M small integers in < 10 ms, that's roughly 200Mobjects per second.  Not too shabby!  The MB/sec numbers below are a bit skewed because hyperluminal-mem writes out 8 bytes per tiny integer.  On this test we have reference tracking disabled on cl-binary-store (\*track-references\* set to NIL) and similarly on cl-store (\*check-for-circs\* nil) on cl-conspack.

    (defun long-list-of-tiny-integers (&optional (n 1000000))
      (loop repeat n collect (- (random 33) 16)))

    CL-BINARY-STORE> (test-on-data (long-list-of-tiny-integers))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 2.28 ms at 3509 MB/sec
     READ : 5.36 ms at 1493 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.00 MB  <-- tiny
     WRITE: 10.28 ms at 97 MB/sec
     READ : 5.20 ms at 192 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 2.00MB
     WRITE: 38.80 ms at 52 MB/sec
     READ : 28.40 ms at 70 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     WRITE: 83.60 ms at 60 MB/sec
     READ : 56.00 ms at 89 MB/sec

Let's go farther into the synthetic zone.  Let's turn on reference tracking \*track-references\* (which is the default behavior anyway).  This rules out hyperluminal-mem as a comparable, and conspack does a bit heavier reference tracking so shouldn't be compared with this mode.

    CL-BINARY-STORE
     OUTPUT SIZE: 1.00 MB
     WRITE: 13.88 ms at 72 MB/sec
     READ : 4.92 ms at 203 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     WRITE: 72.79 ms at 69 MB/sec
     READ : 76.39 ms at 65 MB/sec

Now let's turn on all the features, which adds support for any type of circularity you want (this is \*support-shared-list-structures\* t and \*track-references\* t).  We cannot elide cons markers in this case (at least not in a way that makes any sense from a performance view to me).  Here we are nominally #'eq hash-table speed dominated.  Nominally cl-store supports this mode too with precise-list-storage, but it blows the stack for any data set that is not tiny (so I had to scale the data set down for it).  We are now comparing now with conspack:tracking-refs which supports the same behavior as cl-binary-store:

    CL-BINARY-STORE
     OUTPUT SIZE: 2.00 MB <-- larger file because user has asserted complex list circularity
     WRITE: 90.71 ms at 22 MB/sec
     READ : 5.68 ms at 352 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 2.00MB
     WRITE: 171.58 ms at 12 MB/sec
     READ : 29.60 ms at 68 MB/sec
    CL-STORE (run on a 50x smaller set of data, scaled results up)
     OUTPUT SIZE: 5.00MB
     WRITE: 200.00 ms at 25 MB/sec
     READ : 180.00 ms at 28 MB/sec

We can win back some performance by hinting at the amount of data we are looking at by let'ing \*eq-refs-table-size\* to 1.3 million as we spend a lot of time in grow-hash-table / rehash.  We can do the same thing for cl-store.

    CL-BINARY-STORE
     OUTPUT SIZE: 2.00 MB
     WRITE: 55.59 ms at 36 MB/sec
     READ : 5.32 ms at 376 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     WRITE: 80.79 ms at 62 MB/sec
     READ : 69.99 ms at 71 MB/sec

In this case, though, the lists are simple, so using just \*track-references\* t is the right choice (it allows us to handle normal circularity and is generally the default right choice, the cost is small).

Of course, who the heck would use a list for this sort of thing.  Here's a simple array of signed-byte 8 integers... obviously having a specialized serializer helps:

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 3.12 ms at 2564 MB/sec
     READ : 3.20 ms at 2500 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.00 MB
     WRITE: 0.04 ms at 25000 MB/sec
     READ : 0.24 ms at 4167 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 1.00MB
     WRITE: 26.80 ms at 37 MB/sec
     READ : 20.80 ms at 48 MB/sec
    CL-STORE
     OUTPUT SIZE: 4.00MB
     WRITE: 56.79 ms at 70 MB/sec
     READ : 52.79 ms at 76 MB/sec

## Integers

OK, now onto fixnums.  These are pretty good numbers.  Here is a simple vector of random fixnums... (we showed simple-arrays are no fun to benchmark with because cl-binary-store wins too easily):

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 1.84 ms at 4348 MB/sec
     READ : 3.60 ms at 2222 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 9.00 MB
     WRITE: 2.32 ms at 3880 MB/sec
     READ : 3.36 ms at 2679 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB
     WRITE: 52.79 ms at 170 MB/sec
     READ : 46.80 ms at 192 MB/sec
    CL-STORE
     OUTPUT SIZE: 36.75MB
     WRITE: 416.36 ms at 88 MB/sec
     READ : 289.17 ms at 127 MB/sec

A simple-vector of double floats:

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 16.00 MB
     WRITE: 31.72 ms at 504 MB/sec
     READ : 15.08 ms at 1061 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 9.00 MB
     WRITE: 3.28 ms at 2744 MB/sec
     READ : 7.60 ms at 1184 MB/sec
    CL-BINARY-STORE  <--- just to show what a (simple-array double-float (*)) gets
     OUTPUT SIZE: 8.00 MB
     WRITE: 0.56 ms at 14287 MB/sec
     READ : 2.04 ms at 3922 MB/sec     
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB
     WRITE: 93.19 ms at 97 MB/sec
     READ : 51.99 ms at 173 MB/sec
    CL-STORE
     OUTPUT SIZE: 47.00MB
     WRITE: 553.55 ms at 85 MB/sec
     READ : 860.72 ms at 55 MB/sec

Here's a *list* of random single floats.  Here again for the case of long lists, we are making a small trade in speed for smaller size here (like conspack does).  But still very very fast.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-single-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 3.00 ms at 2667 MB/sec
     READ : 6.04 ms at 1325 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 5.00 MB
     WRITE: 8.20 ms at 610 MB/sec
     READ : 6.28 ms at 796 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 5.00MB
     WRITE: 66.79 ms at 75 MB/sec
     READ : 34.00 ms at 147 MB/sec
    CL-STORE
     OUTPUT SIZE: 22.00MB
     WRITE: 310.77 ms at 71 MB/sec
     READ : 499.55 ms at 44 MB/sec
 
Complex double float numbers.  cl-binary-store goes fast.  Same for complex single floats.  Pretty niche until you starting doing heterodyning and mixing and then you have a large pile of them.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-complex-double-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 24.00 MB
     WRITE: 38.40 ms at 625 MB/sec
     READ : 21.92 ms at 1095 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 17.00 MB
     WRITE: 15.12 ms at 1124 MB/sec
     READ : 15.56 ms at 1093 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 19.00MB
     WRITE: 214.80 ms at 88 MB/sec
     READ : 136.40 ms at 139 MB/sec
    CL-STORE
     OUTPUT SIZE: 96.00MB
     WRITE: 1537.60 ms at 62 MB/sec
     READ : 1882.00 ms at 51 MB/sec    

## Specialized vectors and arrays

cl-binary-store does good work on specialized vectors.  It's just blitting the underlying store to whatever your target is, so hard to beat.  This is very much a reason to use cl-binary-store --- not only is it faster and smaller than the competition

    CL-BINARY-STORE> (test-on-data (long-list-of-big-ub8-vectors))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 80.02 MB
     WRITE: 32.48 ms at 2464 MB/sec
     READ : 39.04 ms at 2050 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.01 MB
     WRITE: 1.20 ms at 8338 MB/sec
     READ : 2.52 ms at 3971 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 10.00MB
     WRITE: 236.00 ms at 42 MB/sec
     READ : 148.40 ms at 67 MB/sec
    CL-STORE
     OUTPUT SIZE: 10.01MB
     WRITE: 112.00 ms at 89 MB/sec
     READ : 36.80 ms at 272 MB/sec

Here are simple-bit-vectors, one of my favorite data structures in Common Lisp

    CL-BINARY-STORE> (test-on-data (long-list-of-big-simple-bit-vectors))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 1.27 MB
     WRITE: 8.00 ms at 159 MB/sec
     READ : 14.44 ms at 88 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.26 MB
     WRITE: 0.12 ms at 10467 MB/sec
     READ : 0.16 ms at 7850 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 20.00MB
     WRITE: 282.00 ms at 71 MB/sec
     READ : 243.20 ms at 82 MB/sec
    CL-STORE
     OUTPUT SIZE: 40.05MB
     WRITE: 664.40 ms at 60 MB/sec
     READ : 504.80 ms at 79 MB/sec

The rest of the story is the same here, for specialized vectors and specialized arrays we are very very fast because we are just blitting them.  This means cl-binary-store is a good choice for serializing double float matrices, for example:

    CL-BINARY-STORE> (test-on-data (list-of-double-float-matrices))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 16.00 MB
     WRITE: 39.48 ms at 405 MB/sec
     READ : 16.48 ms at 971 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 8.00 MB
     WRITE: 0.72 ms at 11112 MB/sec
     READ : 2.00 ms at 4000 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 8.01MB
     WRITE: 89.20 ms at 90 MB/sec
     READ : 62.00 ms at 129 MB/sec
    CL-STORE
     OUTPUT SIZE: 47.01MB
     WRITE: 772.80 ms at 61 MB/sec
     READ : 873.60 ms at 54 MB/sec

## Strings

simple-base-strings, we win again, it's just a simple specialized vector

    CL-BINARY-STORE> (test-on-data (simple-base-strings))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 2.40 MB
     WRITE: 4.04 ms at 594 MB/sec
     READ : 2.56 ms at 938 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 0.79 MB
     WRITE: 2.44 ms at 323 MB/sec
     READ : 2.24 ms at 352 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 0.79MB
     WRITE: 13.60 ms at 58 MB/sec
     READ : 6.00 ms at 131 MB/sec
    CL-STORE
     OUTPUT SIZE: 0.99MB
     WRITE: 16.00 ms at 62 MB/sec
     READ : 7.60 ms at 130 MB/sec

unicode strings.  Here cl-binary-store is doing utf-8 encoding, and you can see our decoder isn't amazing.

    CL-BINARY-STORE> (test-on-data (simple-strings))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 2.40 MB
     WRITE: 4.64 ms at 517 MB/sec
     READ : 3.68 ms at 652 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 0.99 MB
     WRITE: 4.36 ms at 227 MB/sec
     READ : 8.76 ms at 113 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 0.99MB
     WRITE: 14.40 ms at 69 MB/sec
     READ : 5.20 ms at 190 MB/sec <-- Using package trivial-utf8, fast!
    CL-STORE
     OUTPUT SIZE: 2.08MB
     WRITE: 32.40 ms at 64 MB/sec
     READ : 13.20 ms at 157 MB/sec

## Random stuff

Just a bunch of random crud in long list just to average out dispatch stuff.

    CL-BINARY-STORE> (test-on-data (long-complex-list))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 20.49 MB
     WRITE: 33.68 ms at 608 MB/sec
     READ : 29.40 ms at 697 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 7.14 MB
     WRITE: 24.00 ms at 297 MB/sec
     READ : 39.32 ms at 182 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 7.46MB
     WRITE: 133.19 ms at 56 MB/sec
     READ : 86.79 ms at 86 MB/sec
    CL-STORE
     OUTPUT SIZE: 33.43MB
     WRITE: 446.76 ms at 75 MB/sec
     READ : 501.55 ms at 67 MB/sec

## structure-objects

Both hyperluminal-mem and cl-conspack requires writing code to support each new struct.  First we test without reference tracking.  This means writing information out about the structures every time --- at least I think that's how these others all work.  Except hyperluminal-mem which has you write code so it doesn't have to figure structures out.  cl-conspack could do the same, but it doesn't.  cl-binary-store does not ever disable object type tracking, so switches to implicit referencing for object slot and type information.  You can see a big differerence here.  (This is #'lots-of-structure-objects in benchmarks.lisp)

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 10.40 MB
     WRITE: 26.04 ms at 399 MB/sec
     READ : 25.12 ms at 414 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.59 MB  <--- much smaller
     WRITE: 8.96 ms at 177 MB/sec
     READ : 11.92 ms at 133 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.09MB
     WRITE: 183.98 ms at 49 MB/sec
     READ : 129.59 ms at 70 MB/sec
    CL-STORE
     OUTPUT SIZE: 13.69MB
     WRITE: 232.78 ms at 59 MB/sec
     READ : 231.98 ms at 59 MB/sec

Now with reference tracking.  cl-conspack output size improves, but not a huge amount.

    CL-BINARY-STORE
     OUTPUT SIZE: 1.49 MB  <--- (there are some repeated strings in the structs, so even smaller)
     WRITE: 41.28 ms at 36 MB/sec
     READ : 10.60 ms at 141 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 4.89MB
     WRITE: 209.98 ms at 23 MB/sec
     READ : 74.39 ms at 66 MB/sec
    CL-STORE
     OUTPUT SIZE: 6.59MB
     WRITE: 169.98 ms at 39 MB/sec
     READ : 179.98 ms at 37 MB/sec

cl-conspack comes with some cool explain tool that lets you analyze the output.  For each structure it sees it repeats the structure symbol not as a reference even if it has seen that structure before.  Here is a bit of the output.  It's a typed-map.  (:ref 0) is the slot name of the first struct slot and (:ref 1) is the slot name of the second struct slot.  So, yeah, it just isn't aggressive enough about reference tracking.

    ((:LIST 3
      ((:TMAP 2
        ((:SYMBOL BENCH-BLARG) (:TAG 0 (:SYMBOL B)) (:STRING "54")
         (:TAG 1 (:SYMBOL A)) (:NUMBER :DOUBLE-FLOAT 0.8779502726820398d0)
         (:TMAP 2
          ((:SYMBOL BENCH-BLARG) (:REF 0) (:STRING "74") (:REF 1)
           (:NUMBER :DOUBLE-FLOAT 0.42889587902623627d0)))
         (:BOOLEAN NIL)))
       END-OF-FILE))
     END-OF-FILE)

## Lots of the same string

Because cl-conspack behaves badly above for structures, here I am making sure that it is working.  It does.

    CL-BINARY-STORE> (test-on-data (lots-of-the-same-string) :hlmem nil)
    CL-BINARY-STORE
     OUTPUT SIZE: 2.00 MB
     WRITE: 36.16 ms at 55 MB/sec
     READ : 5.56 ms at 360 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 1.00MB
     WRITE: 208.40 ms at 5 MB/sec
     READ : 24.00 ms at 42 MB/sec
    CL-STORE
     OUTPUT SIZE: 4.00MB
     WRITE: 90.80 ms at 44 MB/sec
     READ : 82.00 ms at 49 MB/sec
 
## standard-objects

Again I did not feel like writing an extension for hyperluminal-mem or cl-conspack for this, though it is straightforward to do.  Small files, good speeds.  This is using reference tracking.

    CL-BINARY-STORE> (test-on-data (lots-of-standard-objects) :hlmem nil)
    CL-BINARY-STORE
     OUTPUT SIZE: 0.59 MB
     WRITE: 32.12 ms at 18 MB/sec
     READ : 9.08 ms at 65 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 3.50MB
     WRITE: 197.58 ms at 18 MB/sec
     READ : 66.39 ms at 53 MB/sec     
    CL-STORE
     OUTPUT SIZE: 2.20MB
     WRITE: 85.20 ms at 26 MB/sec
     READ : 64.00 ms at 34 MB/sec

## A pile of tangled conses

Here hyperluminal mem explodes because of list circularity.  Enabling list circularity detection beyond just the normal reference tracking . Notice the asymmetry between store and restore (and also the nice small file).  The faster restore is nice.  Note that this will eventually blow the stack if you don't have it big enough.  We follow CDRs without recursion, but we do CAR following with recursive calls, so...

    CL-BINARY-STORE> (defparameter *blarg* (a-pile-of-tangled-conses))
    CL-BINARY-STORE> (test-cl-binary-store-on-data *blarg* :support-shared-list-structures t :track-references t)
    CL-BINARY-STORE
     OUTPUT SIZE: 0.12 MB
     WRITE: 4.08 ms at 29 MB/sec
     READ : 1.20 ms at 99 MB/sec
    CL-BINARY-STORE> (test-cl-store-on-data *blarg* :check-for-circs t :precise-list-storage t)
    CL-STORE
     OUTPUT SIZE: 0.26MB
     WRITE: 9.20 ms at 28 MB/sec
     READ : 6.40 ms at 40 MB/sec    
    CL-BINARY-STORE> (test-conspack-on-data *blarg* :track-references t)
    CL-CONSPACK
     OUTPUT SIZE: 0.21MB
     WRITE: 15.20 ms at 14 MB/sec
     READ : 2218.00 ms at 0 MB/sec <-- I don't know what is going on here

## ECL, CCL, Allegro, Lispworks and ABCL

Allegro, Lispworks, and CCL are not terrible from a speed point of view, about 4-5x slower than SBCL.  ECL is about 10x slower, but this is due to some failures in structure inlining that is being fixed currently.  ABCl is 100x slower, but I am sure somene can fix that up.  cl-binary-store is still the fastest among cl-store and cl-conspack.  I would do more optimization on Allegro and Lispworks but their heap/time limitations make it near impossible to do any real work.

    ;; Using a 100x shorter list for this test because these implementations are slow
    CL-USER> (test-on-data (long-list-of-tiny-integers 10000))
    ALLEGRO CL-BINARY-STORE <-- about 4x slower than sbcl
     OUTPUT SIZE: 0.02 MB
     WRITE: 0.24 ms at 83 MB/sec
     READ : 0.89 ms at 22 MB/sec
    LISPWORKS CL-BINARY-STORE <-- about 5x slower than sbcl
     OUTPUT SIZE: 0.02 MB
     WRITE: 0.32 ms at 63 MB/sec
     READ : 1.17 ms at 17 MB/sec
    CCL CL-BINARY-STORE  <-- about 4x slower than sbcl
     OUTPUT SIZE: 0.02 MB
     WRITE: 0.27 ms at 74 MB/sec
     READ : 1.58 ms at 13 MB/sec
    ECL CL-BINARY-STORE  <-- about 10x slower than sbcl
     OUTPUT SIZE: 0.02 MB
     WRITE: 4.76 ms at 4 MB/sec
     READ : 7.96 ms at 3 MB/sec

Here are double-floats too:

    CL-USER> (test-on-data (long-list-of-random-double-floats 10000))
    ALLEGRO CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 1.58 ms at 63 MB/sec
     READ : 4.13 ms at 24 MB/sec
    ECL CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 10.07 ms at 10 MB/sec
     READ : 22.61 ms at 4 MB/sec
    CCL CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 0.93 ms at 108 MB/sec
     READ : 6.98 ms at 14 MB/sec

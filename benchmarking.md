# Benchmarking

What's the fun of writing fast code if you cannot show it off?

The comparables sorted by fastest to slowest: HYPERLUMINAL-MEM, CL-CONSPACK and CL-STORE. 

hyperluminal-mem is currently broken in quicklisp (though you can hit accept a couple times to get it compiled and running).

In terms of feature comparable, cl-store has the best behavior out of the box without requiring anything of the user.  cl-conspack is really nice too, but has some different goals, and it requires adding specialized serializers for each structure or object you use.  It is faster than cl-store which is nice, but not fast enough, and as noted earlier doesn't work well with data containing many instances of structures or classes.  Hyperluminal-mem is a very optimized binary serializer made to write to SAP buffers directly.  It is somewhat extensible and again requires writing code for every struct or object you want to serialize.  It does not do reference or circularity tracking at all.  Hyperluminal-mem is currently broken in quicklisp unfortunately, but you can still load it by accepting the compilation errors (it's problems in stm library).  It's my benchmark for zippy raw data serialization.  We are almost as fast as it in many cases, faster in others, slower in others.

All the tests here are using SBCL.  There is a section at the end where I run some on ECL, CCL, and Allegro which are... slow (currently).  I think at least Allegro could be sped up quite a lot.

## Reference tracking disabled

Here we use hyperluminal-mem as the benchmark as it does no reference tracking.  Let's start with some very artificial tests, mainly focused on dispatch code and raw data writing speed.  In this section we disable reference-tracking.  Generally hyperluminal mem wins here (though it generates large files)

### Numbers

Note that here we are writing out 1M conses and 1M small integers in < 10 ms, that's roughly 200Mobjects per second.  Not too shabby!  The MB/sec numbers below are a bit skewed because hyperluminal-mem writes out 8 bytes per tiny integer.  On this test we have reference tracking disabled on cl-binary-store (\*track-references\* set to NIL) and similarly on cl-store (\*check-for-circs\* nil).

    (defun long-list-of-tiny-integers (&optional (n 1000000))
      (loop repeat n collect (- (random 33) 16)))

    CL-BINARY-STORE> (test-on-data (long-list-of-tiny-integers))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 2.28 ms at 3509 MB/sec
     READ : 5.36 ms at 1493 MB/sec
    CL-BINARY-STORE <-- ~290 M objects / second (1 million conses, 1 million integers)
     OUTPUT SIZE: 2.00 MB <--- tiny file!
     WRITE: 6.88 ms at 291 MB/sec
     READ : 6.52 ms at 307 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 2.00MB  <--- tiny files too!
     WRITE: 38.80 ms at 52 MB/sec
     READ : 28.40 ms at 70 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     WRITE: 83.60 ms at 60 MB/sec
     READ : 56.00 ms at 89 MB/sec

OK, so the tiny integers are silly.  Let's use random 8 bit unsigned numbers.  It's a bit
faster than the above, but whatever.

    CL-BINARY-STORE> (test-on-data (long-list-of-not-tiny-integers))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 2.24 ms at 3571 MB/sec
     READ : 4.68 ms at 1709 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 3.00 MB
     WRITE: 3.08 ms at 974 MB/sec
     READ : 7.24 ms at 414 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 2.00MB  <--- cute doing un-boxed conses
     WRITE: 36.40 ms at 55 MB/sec
     READ : 18.40 ms at 109 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     WRITE: 77.60 ms at 64 MB/sec
     READ : 52.80 ms at 95 MB/sec

OK, now onto fixnums.  Still not doing shabbily.  I am not sure why the read dispatch code isn't as fast as hyperluminal-mem, but tracking that down is low down on my list.  These are pretty good numbers.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-fixnums))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 2.24 ms at 3571 MB/sec
     READ : 4.88 ms at 1639 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.00 MB
     WRITE: 3.28 ms at 3049 MB/sec
     READ : 5.96 ms at 1678 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB <-- cute, must be detecting proper lists and eliding cons tags
     WRITE: 58.80 ms at 153 MB/sec
     READ : 47.20 ms at 191 MB/sec
    CL-STORE
     OUTPUT SIZE: 37.75MB
     WRITE: 542.00 ms at 70 MB/sec
     READ : 300.40 ms at 126 MB/sec

Double floats are more interesting.  Here cl-binary-store pulls ahead in writing and reading (note again the smaller files as we are storing the double-floats raw with a one byte tag instead of an 8 byte tag as in the hyperluminal-mem case).

    CL-BINARY-STORE> (test-on-data (long-list-of-random-double-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 16.00 MB
     WRITE: 32.44 ms at 493 MB/sec
     READ : 17.92 ms at 893 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.00 MB  <-- 8MB of double-floats and 2MB of cons tags
     WRITE: 4.16 ms at 2404 MB/sec
     READ : 11.88 ms at 842 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB
     WRITE: 100.80 ms at 89 MB/sec
     READ : 55.60 ms at 162 MB/sec
    CL-STORE
     OUTPUT SIZE: 48.00MB
     WRITE: 805.20 ms at 60 MB/sec
     READ : 880.80 ms at 54 MB/sec

Single floats go very fast, no boxing required on restore

    CL-BINARY-STORE> (test-on-data (long-list-of-random-single-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     WRITE: 3.08 ms at 2597 MB/sec
     READ : 6.52 ms at 1227 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 6.00 MB
     WRITE: 3.40 ms at 1765 MB/sec
     READ : 6.60 ms at 909 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 5.00MB
     WRITE: 70.80 ms at 71 MB/sec
     READ : 35.20 ms at 142 MB/sec
    CL-STORE
     OUTPUT SIZE: 22.00MB
     WRITE: 472.80 ms at 47 MB/sec
     READ : 524.00 ms at 42 MB/sec

Complex double float numbers.  cl-binary-store goes fast.  Same for complex single floats.  Pretty niche until you starting doing heterodyning and mixing and then you have a large pile of them.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-complex-double-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 24.00 MB
     WRITE: 38.40 ms at 625 MB/sec
     READ : 21.92 ms at 1095 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 18.00 MB
     WRITE: 12.32 ms at 1461 MB/sec
     READ : 16.28 ms at 1106 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 19.00MB
     WRITE: 214.80 ms at 88 MB/sec
     READ : 136.40 ms at 139 MB/sec
    CL-STORE
     OUTPUT SIZE: 96.00MB
     WRITE: 1537.60 ms at 62 MB/sec
     READ : 1882.00 ms at 51 MB/sec    

## Specialized vectors and arrays

cl-binary-store does good work on specialized vectors.  It's just blitting the underlying store to whatever your target is, so hard to beat.  This is on my laptop, so.

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
     OUTPUT SIZE: 0.89 MB
     WRITE: 2.28 ms at 390 MB/sec
     READ : 2.20 ms at 404 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 0.79MB  <--- specialized string encoding?
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
     OUTPUT SIZE: 1.09 MB
     WRITE: 3.76 ms at 290 MB/sec
     READ : 8.72 ms at 125 MB/sec
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
     WRITE: 34.04 ms at 602 MB/sec
     READ : 29.92 ms at 685 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 8.14 MB
     WRITE: 18.64 ms at 437 MB/sec
     READ : 39.44 ms at 207 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 7.47MB
     WRITE: 132.80 ms at 56 MB/sec
     READ : 72.00 ms at 104 MB/sec
    CL-STORE
     OUTPUT SIZE: 33.48MB
     WRITE: 539.59 ms at 62 MB/sec
     READ : 489.60 ms at 68 MB/sec

## structure-objects

Both hyperluminal-mem and cl-conspack requires writing code to support each new struct.  First we test without reference tracking.  This means writing information out about the structures every time --- at least I think that's how these others all work.  Except hyperluminal-mem which has you write code so it doesn't have to figure structures out.  cl-conspack could do the same, but it doesn't.  cl-binary-store does not ever disable object type tracking, so switches to implicit referencing for object slot and type information.  You can see a big differerence here

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 10.40 MB
     WRITE: 26.88 ms at 387 MB/sec
     READ : 25.56 ms at 407 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.69 MB
     WRITE: 8.56 ms at 197 MB/sec
     READ : 11.36 ms at 149 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.09MB
     WRITE: 186.40 ms at 49 MB/sec
     READ : 134.80 ms at 67 MB/sec
    CL-STORE
     OUTPUT SIZE: 13.69MB
     WRITE: 272.40 ms at 50 MB/sec
     READ : 237.20 ms at 58 MB/sec

Now with reference tracking.  cl-conspack output size improves, but not a huge amount.

    CL-BINARY-STORE
     OUTPUT SIZE: 1.59 MB
     WRITE: 46.28 ms at 34 MB/sec
     READ : 11.08 ms at 144 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 4.89MB
     WRITE: 182.80 ms at 27 MB/sec
     READ : 74.00 ms at 66 MB/sec
    CL-STORE
     OUTPUT SIZE: 6.69MB
     WRITE: 224.00 ms at 30 MB/sec
     READ : 183.20 ms at 37 MB/sec

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

## ECL, CCL, Allegro

None are recommended for speed, they are about 100x slower than the sbcl version (which is really bonkers --- I think this could easily be fixed, especially Allegro can be made very zippy I am sure).  I couldn't test hyperluminal mem on Allegro as it does not load.

    ;; Using a 100x shorter list for this test because its slow (and no reference tracking)
    CL-BINARY-STORE> (test-on-data (long-list-of-tiny-integers 10000))
    ALLEGRO CL-BINARY-STORE
     OUTPUT SIZE: 0.02 MB
     WRITE: 5.20 ms at 4 MB/sec
     READ : 1.03 ms at 19 MB/sec
    ALLEGRO CL-CONSPACK
     OUTPUT SIZE: 0.02MB
     WRITE: 0.90 ms at 22 MB/sec
     READ : 1.50 ms at 13 MB/sec
    ALLEGRO CL-STORE
     OUTPUT SIZE: 0.05MB
     WRITE: 3.10 ms at 16 MB/sec
     READ : 1.90 ms at 26 MB/sec    
    ECL HYPERLUMINAL-MEM
     OUTPUT SIZE: 0.08 MB
     WRITE: 4.80 ms at 17 MB/sec
     READ : 5.55 ms at 14 MB/sec
    ECL CL-BINARY-STORE
     OUTPUT SIZE: 0.02 MB
     WRITE: 6.34 ms at 3 MB/sec
     READ : 7.71 ms at 3 MB/sec
    ECL CL-CONSPACK
     OUTPUT SIZE: 0.02MB
     WRITE: 5.04 ms at 4 MB/sec
     READ : 4.66 ms at 4 MB/sec
    ECL CL-STORE
     OUTPUT SIZE: 0.05MB
     WRITE: 6.62 ms at 8 MB/sec
     READ : 10.79 ms at 5 MB/sec
    CCL CL-BINARY-STORE
     OUTPUT SIZE: 0.02 MB
     WRITE: 27.11 ms at 1 MB/sec
     READ : 0.23 ms at 87 MB/sec
    CCL CL-CONSPACK
     OUTPUT SIZE: 0.02MB
     WRITE: 5.56 ms at 4 MB/sec
     READ : 0.68 ms at 29 MB/sec

It's not quite so terrible at double floats... on ECL and CCL and Allegro

    CL-BINARY-STORE> (test-on-data (long-list-of-random-double-floats 10000))
    ALLEGRO CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 2.53 ms at 40 MB/sec
     READ : 4.35 ms at 23 MB/sec
    ALLEGRO CL-CONSPACK
     OUTPUT SIZE: 0.09MB
     WRITE: 8.30 ms at 11 MB/sec
     READ : 16.60 ms at 5 MB/sec
    ALLEGRO CL-STORE
     OUTPUT SIZE: 0.48MB
     WRITE: 12.50 ms at 38 MB/sec
     READ : 19.20 ms at 25 MB/sec    
    ECL HYPERLUMINAL-MEM
     OUTPUT SIZE: 0.16 MB
     WRITE: 13.96 ms at 11 MB/sec
     READ : 11.62 ms at 14 MB/sec
    ECL CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 12.30 ms at 8 MB/sec
     READ : 23.30 ms at 4 MB/sec
    ECL CL-CONSPACK
     OUTPUT SIZE: 0.09MB
     WRITE: 25.91 ms at 3 MB/sec
     READ : 29.86 ms at 3 MB/sec
    ECL CL-STORE
     OUTPUT SIZE: 0.48MB
     WRITE: 70.37 ms at 7 MB/sec
     READ : 62.12 ms at 8 MB/sec
    CCL CL-BINARY-STORE
     OUTPUT SIZE: 0.10 MB
     WRITE: 19.08 ms at 5 MB/sec
     READ : 0.35 ms at 288 MB/sec  <-- this is interesting, a sweet spot for CCL
    CCL CL-CONSPACK
     OUTPUT SIZE: 0.09MB
     WRITE: 14.69 ms at 6 MB/sec
     READ : 12.18 ms at 7 MB/sec

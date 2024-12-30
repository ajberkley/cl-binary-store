# Benchmarking

What's the fun of writing fast code if you cannot show it off?

The comparables are: CL-STORE, HYPERLUMINAL-MEM, and CL-CONSPACK.  In terms of feature comparable, cl-store has the best behavior out of the box.  cl-conspack 

In the running here is hyperluminal-mem, a very optimized binary serializer made to
write to SAP buffers directly.  Somewhat extensible, but does not do reference or circularity
tracking at all.  Currently broken in quicklisp unfortunately, but you can still load it
by accepting the compilation errors (it's problems in stm library).  It's my benchmark for
zippy raw data serialization.  We are almost as fast as it in many cases, faster in others,
slower in others.

## Reference tracking disabled

Here we use hyperluminal-mem as the benchmark as it does no reference tracking.  Let's start with some very artificial tests, mainly focused on dispatch code and raw data writing speed.  In this section we disable reference-tracking.  Generally hyperluminal mem wins here (though it generates large files)

### Numbers

Note that here we are writing out 1M conses and 1M small integers in < 10 ms, that's roughly 200Mobjects per second.  Not too shabby!  The MB/sec numbers below are a bit skewed because hyperluminal-mem writes out 8 bytes per tiny integer.  On this test we have reference tracking disabled on cl-binary-store (\*track-references\* set to NIL) and similarly on cl-store (\*check-for-circs\* nil).

    (defun long-list-of-tiny-integers (&optional (n 1000000))
      (loop repeat n collect (random 8)))

    CL-BINARY-STORE> (test-on-data (long-list-of-tiny-integers))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     HLMEM WRITE: 2.52 ms at 3175 MB/sec
     HLMEM READ : 5.04 ms at 1587 MB/sec
     CL-BINARY-STORE
      OUTPUT SIZE: 2.00 MB  <--- small
      CL-BINARY-STORE WRITE: 3.08 ms at 649 MB/sec
      CL-BINARY-STORE READ : 6.44 ms at 311 MB/sec
     CL-CONSPACK
      OUTPUT SIZE: 2.00MB   <--- conspack generates tiny files too
      CONSPACK WRITE: 33.20 ms at 60 MB/sec
      CONSPACK READ: 23.60 ms at 85 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     CL-STORE WRITE: 76.39 ms at 65 MB/sec
     CL-STORE READ : 57.19 ms at 87 MB/sec

OK, so the tiny integers are silly.  Let's use random 8 bit unsigned numbers.  It's a bit
faster than the above, but whatever.

    CL-BINARY-STORE> (test-on-data (long-list-of-not-tiny-integers))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     HLMEM WRITE: 2.36 ms at 3390 MB/sec
     HLMEM READ : 4.96 ms at 1613 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 3.00 MB <-- one byte for each cons, two bytes for each small integer
     CL-BINARY-STORE WRITE: 3.12 ms at 961 MB/sec
     CL-BINARY-STORE READ : 6.60 ms at 455 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 2.00MB <-- this is pretty amazingly small!
     CONSPACK MEMORY WRITE: 32.00 ms at 63 MB/sec
     CONSPACK READ: 27.20 ms at 74 MB/sec
    CL-STORE
     OUTPUT SIZE: 5.00MB
     CL-STORE WRITE: 72.40 ms at 69 MB/sec
     CL-STORE READ : 54.00 ms at 93 MB/sec

OK, now onto fixnums.  Still not doing shabbily.  I am not sure why the read dispatch code isn't as fast as hyperluminal-mem, but tracking that down is low down on my list.  These are pretty good numbers.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-fixnums))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     HLMEM WRITE: 2.28 ms at 3508 MB/sec
     HLMEM READ : 4.76 ms at 1680 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.00 MB
     CL-BINARY-STORE WRITE: 3.28 ms at 3048 MB/sec
     CL-BINARY-STORE READ : 8.52 ms at 1174 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB <--- smallest!  It must be detecting proper lists and eliding conses
     CONSPACK WRITE: 58.80 ms at 153 MB/sec
     CONSPACK READ: 48.80 ms at 184 MB/sec     
    CL-STORE
     OUTPUT SIZE: 37.75MB
     CL-STORE WRITE: 638.57 ms at 59 MB/sec
     CL-STORE READ : 308.48 ms at 122 MB/sec

Double floats are more interesting.  Here cl-binary-store pulls ahead in writing and reading (note again the smaller files as we are storing the double-floats raw with a one byte tag instead of an 8 byte tag as in the hyperluminal-mem case).

    CL-BINARY-STORE> (test-on-data (long-list-of-random-double-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 16.00 MB
     HLMEM WRITE: 33.48 ms at 478 MB/sec
     HLMEM READ : 18.28 ms at 875 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.00 MB
     CL-BINARY-STORE WRITE: 14.11 ms at 709 MB/sec
     CL-BINARY-STORE READ : 15.74 ms at 635 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.00MB
     CONSPACK WRITE: 99.20 ms at 91 MB/sec
     CONSPACK READ: 57.20 ms at 157 MB/sec     
    CL-STORE
     OUTPUT SIZE: 48.00MB
     CL-STORE WRITE: 804.85 ms at 60 MB/sec
     CL-STORE READ : 896.06 ms at 54 MB/sec

Single floats go faster on hyperluminal mem.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-single-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 8.00 MB
     HLMEM WRITE: 3.20 ms at 2501 MB/sec
     HLMEM READ : 6.12 ms at 1308 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 6.00 MB
     CL-BINARY-STORE WRITE: 6.72 ms at 893 MB/sec
     CL-BINARY-STORE READ : 9.08 ms at 661 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 5.00MB
     CONSPACK WRITE: 74.40 ms at 67 MB/sec
     CONSPACK READ: 37.20 ms at 134 MB/sec     
    CL-STORE
     OUTPUT SIZE: 22.00MB
     CL-STORE WRITE: 408.18 ms at 54 MB/sec
     CL-STORE READ : 511.61 ms at 43 MB/sec

Complex double float numbers.  cl-binary-store goes fast.  Same for complex single floats.  Pretty niche.

    CL-BINARY-STORE> (test-on-data (long-list-of-random-complex-double-floats))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 24.00 MB
     HLMEM WRITE: 39.32 ms at 610 MB/sec
     HLMEM READ : 22.08 ms at 1087 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 18.00 MB
     CL-BINARY-STORE WRITE: 23.80 ms at 756 MB/sec
     CL-BINARY-STORE READ : 23.08 ms at 780 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 19.00MB
     CONSPACK WRITE: 221.20 ms at 86 MB/sec
     CONSPACK READ: 138.00 ms at 138 MB/sec     
    CL-STORE
     OUTPUT SIZE: 96.00MB
     CL-STORE WRITE: 1514.06 ms at 63 MB/sec
     CL-STORE READ : 1857.67 ms at 52 MB/sec

## Specialized vectors and arrays

cl-binary-store does good work on specialized vectors.  It's just blitting the underlying store to whatever your target is, so hard to beat.  This is on my laptop, so.

    CL-BINARY-STORE> (test-on-data (long-list-of-big-ub8-vectors))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 80.02 MB
     HLMEM WRITE: 33.44 ms at 2393 MB/sec
     HLMEM READ : 40.52 ms at 1975 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 10.01 MB
     CL-BINARY-STORE WRITE: 1.40 ms at 7147 MB/sec
     CL-BINARY-STORE READ : 2.60 ms at 3848 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 10.00MB
     CONSPACK WRITE: 229.20 ms at 44 MB/sec
     CONSPACK READ: 154.80 ms at 65 MB/sec     
    CL-STORE
     OUTPUT SIZE: 10.01MB
     CL-STORE WRITE: 108.41 ms at 92 MB/sec
     CL-STORE READ : 32.80 ms at 305 MB/sec

Here are simple-bit-vectors, one of my favorite data structures in Common Lisp

    CL-BINARY-STORE> (test-on-data (long-list-of-big-simple-bit-vectors))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 1.27 MB
     HLMEM WRITE: 8.20 ms at 155 MB/sec
     HLMEM READ : 14.72 ms at 86 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.26 MB
     CL-BINARY-STORE WRITE: 0.12 ms at 10467 MB/sec
     CL-BINARY-STORE READ : 0.40 ms at 3140 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 20.00MB
     CONSPACK WRITE: 292.00 ms at 69 MB/sec
     CONSPACK READ: 246.80 ms at 81 MB/sec     
    CL-STORE
     OUTPUT SIZE: 40.05MB
     CL-STORE WRITE: 718.01 ms at 56 MB/sec
     CL-STORE READ : 511.61 ms at 78 MB/sec

The rest of the story is the same here, for specialized vectors and specialized arrays we are very very fast because we are just blitting them.  This means cl-binary-store is a good choice for serializing double float matrices, for example:

    CL-BINARY-STORE> (test-on-data (list-of-double-float-matrices))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 16.00 MB
     HLMEM WRITE: 38.92 ms at 411 MB/sec
     HLMEM READ : 18.80 ms at 851 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 8.00 MB
     CL-BINARY-STORE WRITE: 0.76 ms at 10527 MB/sec
     CL-BINARY-STORE READ : 1.88 ms at 4256 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 8.01MB
     CONSPACK WRITE: 89.60 ms at 89 MB/sec
     CONSPACK READ: 63.60 ms at 126 MB/sec     
    CL-STORE
     OUTPUT SIZE: 47.01MB
     CL-STORE WRITE: 757.61 ms at 62 MB/sec
     CL-STORE READ : 871.62 ms at 54 MB/sec

## Strings

simple-base-strings, we win again, it's just a simple specialized vector

    CL-BINARY-STORE> (test-on-data (simple-base-strings))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 2.40 MB
     HLMEM WRITE: 4.12 ms at 583 MB/sec
     HLMEM READ : 2.92 ms at 822 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 0.89 MB
     CL-BINARY-STORE WRITE: 2.20 ms at 404 MB/sec
     CL-BINARY-STORE READ : 2.36 ms at 377 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 0.79MB <--- specialized string encoding
     CONSPACK WRITE: 14.00 ms at 56 MB/sec
     CONSPACK READ: 5.20 ms at 152 MB/sec
    CL-STORE
     OUTPUT SIZE: 0.99MB
     CL-STORE WRITE: 14.80 ms at 67 MB/sec
     CL-STORE READ : 8.80 ms at 112 MB/sec

unicode strings.  Here cl-binary-store is doing utf-8 encoding, and you can see our decoder isn't amazing.

    CL-BINARY-STORE> (test-on-data (simple-strings))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 2.40 MB
     HLMEM WRITE: 4.72 ms at 509 MB/sec
     HLMEM READ : 3.32 ms at 723 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 1.09 MB
     CL-BINARY-STORE WRITE: 3.68 ms at 296 MB/sec
     CL-BINARY-STORE READ : 9.12 ms at 119 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 0.99MB
     CONSPACK WRITE: 15.20 ms at 65 MB/sec
     CONSPACK READ: 6.40 ms at 155 MB/sec  <--- zooom!
    CL-STORE
     OUTPUT SIZE: 2.08MB
     CL-STORE WRITE: 29.20 ms at 71 MB/sec
     CL-STORE READ : 15.60 ms at 133 MB/sec

## Random stuff

Just a bunch of random crud in long list

    CL-BINARY-STORE> (test-on-data (long-complex-list))
    HYPERLUMINAL-MEM
     OUTPUT SIZE: 20.50 MB
     HLMEM WRITE: 37.52 ms at 546 MB/sec
     HLMEM READ : 33.12 ms at 619 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 7.52 MB
     CL-BINARY-STORE WRITE: 22.92 ms at 328 MB/sec
     CL-BINARY-STORE READ : 52.44 ms at 143 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 6.69MB
     CONSPACK WRITE: 141.60 ms at 47 MB/sec
     CONSPACK READ: 69.20 ms at 97 MB/sec     
    CL-STORE
     OUTPUT SIZE: 27.75MB
     CL-STORE WRITE: 522.40 ms at 53 MB/sec
     CL-STORE READ : 430.40 ms at 64 MB/sec

## structure-objects

Both hyperluminal-mem and cl-conspack requires writing code to support each new struct.  First we test without reference tracking.  This means writing information out about the structures every time --- at least I think that's how these all work.

    HYPERLUMINAL-MEM
     OUTPUT SIZE: 9.60 MB
     HLMEM WRITE: 24.36 ms at 394 MB/sec
     HLMEM READ : 23.04 ms at 417 MB/sec
    CL-BINARY-STORE
     OUTPUT SIZE: 9.40 MB
     CL-BINARY-STORE WRITE: 31.88 ms at 295 MB/sec
     CL-BINARY-STORE READ : 88.20 ms at 106 MB/sec      <--- terrible, investigate!
    CL-CONSPACK
     OUTPUT SIZE: 8.49MB
     CONSPACK WRITE: 182.40 ms at 47 MB/sec
     CONSPACK READ: 128.40 ms at 66 MB/sec
    CL-STORE
     OUTPUT SIZE: 13.09MB
     CL-STORE WRITE: 260.40 ms at 50 MB/sec
     CL-STORE READ : 253.20 ms at 52 MB/sec

Now with reference tracking.  cl-conspack does not seem to do the right thing here

    CL-BINARY-STORE
     OUTPUT SIZE: 1.59 MB
     CL-BINARY-STORE WRITE: 53.20 ms at 30 MB/sec
     CL-BINARY-STORE READ : 9.72 ms at 164 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 9.09MB <-- not tracking right?
     CONSPACK WRITE: 187.20 ms at 49 MB/sec
     CONSPACK READ: 136.00 ms at 67 MB/sec
    CL-STORE
     OUTPUT SIZE: 6.69MB
     CL-STORE WRITE: 260.40 ms at 26 MB/sec
     CL-STORE READ : 184.80 ms at 36 MB/sec

## Lots of the same string

Because cl-conspack behaves badly above for structures, here I am making sure that it is working.  It does (really well).  I don't understand why cl-binary-store is so slow at writing

    CL-BINARY-STORE> (test-on-data (lots-of-the-same-string) :hlmem nil)
    CL-BINARY-STORE
     OUTPUT SIZE: 2.00 MB
     CL-BINARY-STORE WRITE: 34.48 ms at 58 MB/sec
     CL-BINARY-STORE READ : 6.64 ms at 301 MB/sec
    CL-CONSPACK
     OUTPUT SIZE: 6.00MB <-- does not seem to be tracking string references?
     CONSPACK WRITE: 141.20 ms at 42 MB/sec
     CONSPACK READ: 64.00 ms at 94 MB/sec
    CL-STORE
     OUTPUT SIZE: 4.00MB
     CL-STORE WRITE: 96.00 ms at 42 MB/sec
     CL-STORE READ : 69.20 ms at 58 MB/sec

## standard-objects

Again I did not feel like writing an extension for hyperluminal-mem or cl-conspack for this, though it is straightforward to do.  Small files, good speeds.  This is using reference tracking.

    CL-BINARY-STORE> (test-on-data (lots-of-standard-objects) :hlmem nil)
    CL-BINARY-STORE
     OUTPUT SIZE: 0.60 MB
     CL-BINARY-STORE WRITE: 34.92 ms at 17 MB/sec
     CL-BINARY-STORE READ : 8.04 ms at 74 MB/sec
    CL-STORE
     OUTPUT SIZE: 2.20MB
     CL-STORE WRITE: 97.60 ms at 23 MB/sec
     CL-STORE READ : 66.80 ms at 33 MB/sec

## A pile of tangled conses

Here hyperluminal mem explodes because no circularity detection. Notice the asymmetry between store and restore (and also the nice small file).  The faster restore is nice.  Note that this will eventually blow the stack if you don't have it big enough.  We follow CDRs without recursion, but we do CAR following with recursive calls, so...

    CL-BINARY-STORE> (defparameter *blarg* (a-pile-of-tangled-conses))
    CL-BINARY-STORE> (test-cl-binary-store-on-data *blarg* :support-shared-list-structures t)
    CL-BINARY-STORE
     OUTPUT SIZE: 0.12 MB
     CL-BINARY-STORE WRITE: 4.08 ms at 29 MB/sec
     CL-BINARY-STORE READ : 1.56 ms at 76 MB/sec
    CL-BINARY-STORE> (test-cl-store-on-data *blarg* :check-for-circs t)
    CL-STORE
     OUTPUT SIZE: 0.39MB
     CL-STORE WRITE: 10.00 ms at 39 MB/sec
     CL-STORE READ : 9.20 ms at 42 MB/sec
    CL-BINARY-STORE> (test-conspack-on-data *blarg* :track-references t)
     did not finish (did not handle circularity?)



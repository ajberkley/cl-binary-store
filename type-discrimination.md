# Serialization speed

So, this whole thing works reasonably well for the use case I have and
deserialization of complex objects is blazingly fast.  But
serialization in the case of non-multiply referenced data (and no
circularity) is still about 3x slower than hyperluminal-mem.  So I did
a bit of benchmarking and found that about 30% of my time is spent in
type dispatch (this is for a very synthetic example of a list of a
million small numbers).  My dispatch mechanism is just a flat
etypecase grouped into disjoint types and sorted by subtype.  So, for
example, I allow dispatch to different functions for an (unsigned-byte
8) and a fixnum, or for a (simple-array double-float (*)) versus just
a simple-vector.

Nominally I'd expect the CLOS infrastructure for dispatch to be
relatively fast, but you cannot do easy top level specialization for
things like (simple-array (signed-byte 8) (4)), for example, which in
some cases it makes sense to do (for what I care about, maybe not for
the rest of the world!).

So, you'll find in the file src/type-discrimation.lisp my very crude
cut at determining how fast I'd expect a not-so-smart compiler to be
able to determine types of objects.  Remember, I am currently just
using a flat typecase sorted by subtype for cl-store-faster.  Anyway,
here my model is a bunch of nested, auto-generated, typecases that
mirror the Common Lisp type hierarchy.  Now, nominally there are
tricks that are smarter than this based on tag hashing and jump tables
probably, but I've got to start with my understanding somewhere.  So,
anyway, I cobbled together an estimator for what it would take to do
dispatch using a tree of discriminators (don't judge the code please,
I wrote it as fast as possible because I wanted to see the results).

First, I cut my type space automatically into disjoint subsets (either
fully or with some hinting as to a good order to do the top level
discrimination) and then I compile discriminators, like I'd expect the
compiler to do in a nested typecase scenario.  At each node in the
tree, I know what the object under test's super-type is (even if just
T) and what I have learned so far about what the object is not.  I do
a rough count of the number of instructions, function-calls, and
compares done to get to a decision.  So, for an example, here I choose
a not so random top-level ordering (the first few are
immediate/unboxed objects in a sense, and so should be easy to
discriminate) (you can also run this with just '(t) as a singular tree
root, but it's far from optimal!).

    (simulate-discriminators *many-types*
       '(cons fixnum null (eql t) single-float
         array number structure-object standard-object t))

Here's my ascii art representation of the resulting tree and some annotations run
on sbcl 2.4.4.

    CONS 3 compares, 13 instructions, and 0 function-calls
    FIXNUM 4 compares, 22 instructions, and 0 function-calls
     (UNSIGNED-BYTE 32) 5 compares, 32 instructions, and 0 function-calls
      (UNSIGNED-BYTE 16) 6 compares, 42 instructions, and 0 function-calls
       (UNSIGNED-BYTE 8) 7 compares, 52 instructions, and 0 function-calls
    NULL 5 compares, 31 instructions, and 0 function-calls
    (EQL T) 6 compares, 39 instructions, and 0 function-calls
    SINGLE-FLOAT 7 compares, 48 instructions, and 0 function-calls
    ARRAY 9 compares, 61 instructions, and 0 function-calls
     SIMPLE-ARRAY 10 compares, 72 instructions, and 0 function-calls
      (SIMPLE-ARRAY FIXNUM *) 14 compares, 91 instructions, and 0 function-calls
     VECTOR 11 compares, 81 instructions, and 0 function-calls
      SIMPLE-VECTOR 12 compares, 90 instructions, and 0 function-calls
      (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) 13 compares, 99 instructions, and 0 function-calls
      (SIMPLE-ARRAY SINGLE-FLOAT (*)) 14 compares, 108 instructions, and 0 function-calls
      (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 15 compares, 117 instructions, and 0 function-calls
      (SIMPLE-ARRAY FIXNUM (*)) 16 compares, 126 instructions, and 0 function-calls
    NUMBER 11 compares, 74 instructions, and 0 function-calls
     COMPLEX 13 compares, 89 instructions, and 0 function-calls
      (COMPLEX DOUBLE-FLOAT) 14 compares, 98 instructions, and 0 function-calls
      (COMPLEX SINGLE-FLOAT) 15 compares, 107 instructions, and 0 function-calls
     REAL 17 compares, 104 instructions, and 0 function-calls
      RATIONAL 20 compares, 119 instructions, and 0 function-calls
       INTEGER 22 compares, 132 instructions, and 0 function-calls
        (UNSIGNED-BYTE 64) 31 compares, 155 instructions, and 0 function-calls
        BIGNUM 32 compares, 164 instructions, and 0 function-calls
       RATIO 22 compares, 138 instructions, and 0 function-calls
      DOUBLE-FLOAT 21 compares, 128 instructions, and 0 function-calls
    STRUCTURE-OBJECT 14 compares, 88 instructions, and 0 function-calls
     ANOTHER 15 compares, 99 instructions, and 0 function-calls
     BLARG 16 compares, 110 instructions, and 0 function-calls
      INCLUDES-BLARG 17 compares, 121 instructions, and 0 function-calls
    STANDARD-OBJECT 15 compares, 94 instructions, and 1 function-calls
     STANDARD-CLASS 15 compares, 100 instructions, and 2 function-calls
    T 15 compares, 100 instructions, and 1 function-calls
     SATISFIES-SOMETHING 17 compares, 115 instructions, and 2 function-calls
 
So to read this you can say the code that would be generated by this
set of typecases would take 3 compares to determine that something was
a cons and dispatch to the relevant function, if it was not a cons,
then we need one more comparison to determine if it is a fixnum or
not, and if it was, then say 2 more to determine if the number is an
(unsigned-byte 16).  If we weren't a fixnum, then in the end to
determine if we are a standard-object (near the end) we would have
performed 15 total compares, 94 instructions and one function call.

You can tell that this is obviously a silly thing to do in some cases,
why would you test if something was a REAL, then a RATIONAL then an
INTEGER, as you likely would actually be testing already (OR INTEGER
RATIO).  In fact you can see that's the case, because it takes 22
compares to get to INTEGER and also 22 to get to RATIO.  But, we can
make one of them faster than the other by removing the RATIONAL type
node intermediary from \*many-types\*.  Focusing just on this part, we
find:

    NUMBER 11 compares, 74 instructions, and 0 function-calls
     COMPLEX 13 compares, 89 instructions, and 0 function-calls
      (COMPLEX DOUBLE-FLOAT) 14 compares, 98 instructions, and 0 function-calls
      (COMPLEX SINGLE-FLOAT) 15 compares, 107 instructions, and 0 function-calls
     REAL 17 compares, 104 instructions, and 0 function-calls
      RATIO 19 compares, 116 instructions, and 0 function-calls
      INTEGER 22 compares, 131 instructions, and 0 function-calls
       BIGNUM 23 compares, 140 instructions, and 0 function-calls
       (UNSIGNED-BYTE 64) 24 compares, 149 instructions, and 0 function-calls
      DOUBLE-FLOAT 23 compares, 140 instructions, and 0 function-calls

So here the discrimination is best-cased for ratio which is tested
before integer.  So far I haven't put this information into use, but
it's cool.  Anyhow, this is a work in progress, I'm posting this to in
the hope that someone will get annoyed and show me a how to do this
way faster.
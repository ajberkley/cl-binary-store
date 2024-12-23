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

I had hoped the compiler would do some magic with perfect tag hashes
and jump-tables as I see some of that in the source code, but I wasn't
able to get it to do it, so I ran down this rabbit hole a little bit
to see if nesting type-cases is worth it.

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

## What does sbcl generate for a typecase?

I couldn't get it to generate anything complex, but there are strong hints in the source
code that it can do some really smart dispatch generation with jump-tables and hashing, but
I couldn't figure out the magic incantation yet.  Let's just examine what I get out of the
box and see if my estimates above are reasonable.  See *trust-sbcl* in type-discrimination.lisp
which is just a simple flat typecase with a bunch of types in it without any re-ordering:

    (simulate-discriminators '((unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                               (unsigned-byte 64) fixnum blarg includes-blarg
			       (simple-array double-float (*)) simple-array vector
			       array ratio complex)
                             '((unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                               (unsigned-byte 64) fixnum blarg includes-blarg
			       (simple-array double-float (*)) simple-array vector
			       array ratio complex))

versus

    (defun trust-sbcl (x)
      (declare (optimize (speed 3) (safety 0) (debug 0)))
      (typecase x
        ((unsigned-byte 8) 0)
        ((unsigned-byte 16) 1)
        ((unsigned-byte 32) 2)
        ((unsigned-byte 64) 3)
        (fixnum 4)
        (blarg 5)
        (includes-blarg 6)
        ((simple-array double-float (*)) 7)
        (simple-array 8)
        (vector 9)
        (array 10)
        (ratio 11)
        (complex 12)))

    (UNSIGNED-BYTE 8) 1 compares, 10 instructions, and 0 function-calls
    (UNSIGNED-BYTE 16) 2 compares, 20 instructions, and 0 function-calls
    (UNSIGNED-BYTE 32) 3 compares, 30 instructions, and 0 function-calls
    (UNSIGNED-BYTE 64) 11 compares, 54 instructions, and 0 function-calls
    FIXNUM 12 compares, 63 instructions, and 0 function-calls
    BLARG 14 compares, 77 instructions, and 0 function-calls
    INCLUDES-BLARG 14 compares, 83 instructions, and 0 function-calls
    (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 16 compares, 95 instructions, and 0 function-calls
    SIMPLE-ARRAY 18 compares, 109 instructions, and 0 function-calls
    VECTOR 20 compares, 123 instructions, and 0 function-calls
    ARRAY 22 compares, 136 instructions, and 0 function-calls
    RATIO 24 compares, 148 instructions, and 0 function-calls
    COMPLEX 26 compares, 163 instructions, and 0 function-calls

versus the sbcl code below.  Note that there is a lot of complexity in the ub32->ub64 thing
reflected in the assembly below.  You can see that testing for fixnum first would have been
way smarter.

    ; disassembly for TRUST-SBCL
    ; Size: 307 bytes. Origin: #x55414696                         ; TRUST-SBCL
    ; 696:       48F7C201FEFFFF   TEST RDX, -511
    ; 69D:       7505             JNE L1
    ; 69F:       31D2             XOR EDX, EDX      <--- unsigned-byte 8 return 0
    ; 6A1: L0:   C9               LEAVE
    ; 6A2:       F8               CLC
    ; 6A3:       C3               RET
    ; 6A4: L1:   48F7C20100FEFF   TEST RDX, -131071
    ; 6AB:       0F840E010000     JEQ L15          <--- (unsigned-byte 16)
    ; 6B1:       488515C0FFFFFF   TEST RDX, [RIP-64]              ; [#x55414678] = #xFFFFFFFE00000001
    ; 6B8:       0F84F7000000     JEQ L14          <--- (unsigned-byte 32)
    ; 6BE:       4885142560000050 TEST RDX, [#x50000060]
    ; 6C6:       0F84DF000000     JEQ L13          <--- one possible case of (unsigned-byte 64)?
    ; 6CC:       8D42F1           LEA EAX, [RDX-15]
    ; 6CF:       A80F             TEST AL, 15
    ; 6D1:       752D             JNE L3
    ; 6D3:       488B42F1         MOV RAX, [RDX-15]
    ; 6D7:       483D11010000     CMP RAX, 273
    ; 6DD:       7414             JEQ L2
    ; 6DF:       482D11020000     SUB RAX, 529
    ; 6E5:       7519             JNE L3
    ; 6E7:       48394201         CMP [RDX+1], RAX
    ; 6EB:       0F84BA000000     JEQ L13         <--- another possible case of (unsigned-byte 64)
    ; 6F1:       EB0D             JMP L3
    ; 6F3: L2:   488B42F9         MOV RAX, [RDX-7]
    ; 6F7:       4885C0           TEST RAX, RAX
    ; 6FA:       0F89AB000000     JNS L13         <--- again (unsigned-byte 64)
    ; 700: L3:   F6C201           TEST DL, 1
    ; 703:       0F8498000000     JEQ L12         <--- fixnum (sure would have been easier to do this first!)
    ; 709:       8D42FD           LEA EAX, [RDX-3]
    ; 70C:       A80F             TEST AL, 15
    ; 70E:       7516             JNE L4
    ; 710:       8B4201           MOV EAX, [RDX+1]
    ; 713:       81784D5B010000   CMP DWORD PTR [RAX+77], 347
    ; 71A:       750A             JNE L4
    ; 71C:       BA0A000000       MOV EDX, 10
    ; 721:       E97BFFFFFF       JMP L0
    ; 726: L4:   488D4AF1         LEA RCX, [RDX-15]
    ; 72A:       F6C10F           TEST CL, 15
    ; 72D:       752C             JNE L5
    ; 72F:       8A09             MOV CL, [RCX]
    ; 731:       8BC1             MOV EAX, ECX
    ; 733:       3CD5             CMP AL, -43
    ; 735:       7460             JEQ L11          <--- (simple-array double-float (*))
    ; 737:       8BC1             MOV EAX, ECX
    ; 739:       2C81             SUB AL, -127
    ; 73B:       3C64             CMP AL, 100
    ; 73D:       764E             JBE L10          <--- simple-array
    ; 73F:       8BC1             MOV EAX, ECX
    ; 741:       2C85             SUB AL, -123
    ; 743:       3C70             CMP AL, 112
    ; 745:       763C             JBE L9           <--- vector
    ; 747:       8BC1             MOV EAX, ECX
    ; 749:       3C81             CMP AL, -127
    ; 74B:       732C             JAE L8           <--- array
    ; 74D:       8BC1             MOV EAX, ECX
    ; 74F:       3C15             CMP AL, 21
    ; 751:       741C             JEQ L7           <--- ratio
    ; 753:       8BC1             MOV EAX, ECX
    ; 755:       2C21             SUB AL, 33
    ; 757:       3C08             CMP AL, 8
    ; 759:       760A             JBE L6           <--- complex
    ; 75B: L5:   BA17010050       MOV EDX, #x50000117             ; NIL
    ; 760:       E93CFFFFFF       JMP L0           <--- otherwise (no match)
    ; 765: L6:   BA18000000       MOV EDX, 24
    ; 76A:       E932FFFFFF       JMP L0
    ; 76F: L7:   BA16000000       MOV EDX, 22
    ; 774:       E928FFFFFF       JMP L0
    ; 779: L8:   BA14000000       MOV EDX, 20
    ; 77E:       E91EFFFFFF       JMP L0
    ; 783: L9:   BA12000000       MOV EDX, 18
    ; 788:       E914FFFFFF       JMP L0
    ; 78D: L10:  BA10000000       MOV EDX, 16
    ; 792:       E90AFFFFFF       JMP L0
    ; 797: L11:  BA0E000000       MOV EDX, 14
    ; 79C:       E900FFFFFF       JMP L0
    ; 7A1: L12:  BA08000000       MOV EDX, 8
    ; 7A6:       E9F6FEFFFF       JMP L0
    ; 7AB: L13:  BA06000000       MOV EDX, 6
    ; 7B0:       E9ECFEFFFF       JMP L0
    ; 7B5: L14:  BA04000000       MOV EDX, 4
    ; 7BA:       E9E2FEFFFF       JMP L0
    ; 7BF: L15:  BA02000000       MOV EDX, 2
    ; 7C4:       E9D8FEFFFF       JMP L0
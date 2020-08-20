https://groups.google.com/g/racket-users/c/2X5olKMV3C4/m/mJhsp9ZWBgAJ

I think I may understand what’s going on here, but a student and I worked on this for quite a while today before I found the problem. 

Here’s a program: 

#lang typed/racket 

(define-type Store (Mutable-HashTable Integer Value)) 
(define-type Value (U Real Boolean String)) 

(define top-store (cast (make-hash (list 
(cons -1 14) 
(cons 1 #t) 
(cons 2 #f))) 
Store)) 

(hash-set! top-store 5 1234) 

It fails with this error: 

contract violation 
expected: (or/c (and/c byte? positive?) #t #f) 
given: 1234 
in: the values of 
the 3rd conjunct of 
(and/c 
hash? 
hash-mutable? 
(hash/c 
exact-integer? 
(or/c (and/c byte? positive?) #t #f) 
#:immutable 
#f)) 
contract from: typed-world 
blaming: cast 
(assuming the contract is correct) 
at: unsaved-editor:6.18 

If I understand what’s going on here, the basic issue is that the mutable hash table’s type is being inferred as (Immutable-HashTable Exact-Integer (U Boolean Positive-Byte)), and then as part of the cast, a contract is being inserted, which checks that all added values match the expected value type. The outer cast allows type checking to proceed, but then at runtime it fails because the given value doesn’t match the inferred value type. 

This error doesn’t occur with immutable hash tables, because it’s fine to extend an immutable hash table to a larger one that contains it; the original one’s contract isn’t violated. 

In this case, one easy error is to change the ‘cast’ into an ‘ann’, which works fine. 

This is the first time I’ve encouraged my students to use a mutable hash, which is presumably why I haven’t encountered this before. 

I’m trying to formulate a solution that I can put in a Hints for TR file, and I think the answer is probably this: 

- When you use “make-hash” in TR, you should always specify the types explicitly using an “inst”. 
or maybe 
- When you call “make-hash” in TR, the call should be immediately wrapped with an “ann” type annotation. 

In a perfect world, I think I would ask for a warning when casting a mutable hash table. It could go in that nice “warnings” box. Oh, wait… 

… joking aside, actually I just did turn on the “log” window and I don’t see any warning about this, which is not too surprising. Oh, wait, I see another bug… 

Thanks for reading, let me know if there’s any more obvious solution. 

John 

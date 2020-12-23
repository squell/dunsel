Code which serves no useful purpose
===================================

?aesh
-----
An implementation of AES in straight Bourne Shell; speed is measures in 'seconds per block encrypted'. Still quite a nice and compact implementation, though!

Despite Daan Sprenkels' efforts (see commit dae3df5ff72f02eec9063e203fcbe9249c2393f0), I am pretty sure this implementation is very far from being constant-time.

bitslice
--------
An example of bit-slicing as applied to a multiplication in GF(2^8), implementedin both C and assembly. Mainly to argue the point that doing it right in assembly (with macro's) is easier, more readable, and less error-prone than in C.

consttime
---------
Constant time implementations for stuff that you never knew needed constant-time implementations!

* `memcmp` and `sort`
* A C++ container class template with constant-time reversal

frege
-----
A 'trusted kernel'-type proof assistant for propositional logic in
Standard ML, written during the course Proof Assistants. Comes in two
flavours; with LCF-style tactics and Isabelle-style tactics.

It is also a deliberate and cynical attempt at mocking theorem provers.

While the 'trusted kernel' of Frege is very small and simple, it
only knows negation and implication (hence its name!). Conjuction and
disjunction is translated into this in the usual way, their inference rules
are proven, and the pretty printer has the ability to print them.

The result is a prover where non-obvious things will *seem to be proven*
in the pretty printer. Or perhaps they actually *are proven*, but the
pretty-printer is part of the trusted kernel, too?

If none of this make sense, just read Logicomix, it is more fun.

goto
----
Haskell-implementations for the semantics of a WHILE-like language in
various styles (Monads, Continuations); including an extension of WHILE
which has scoped variables and jumps, where the semantics relies heavily on
lazy evaluation and can produce some weird results.

Essentially a cleaner expression (no pun intended) of the idea behind:
http://cs.ru.nl/~M.Schoolderman/StdImperative/

iostream
--------
Three simple benchmarks to make the point that C++ iostreams are decrepit
and horribly slow when used in the 'obvious' way. But that C++ streambufs
are awesome.

At least in C++ '03.

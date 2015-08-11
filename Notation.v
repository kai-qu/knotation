(* Axes of notation:

- concision
- redundancy of information
- 1 notation -> n knots (0, 1, constant, not constant but bounded, unbounded)
- 1 knot -> n notations (0, 1, constant, not constant but bounded, unbounded)
- ease of operations/functions/manipulation
  - by human
  - by computer
- ease of "understanding"
  - by human
  - by computer
- ease of transcription by human
- *** Properties encoded ***
- ?

Properties encoded

- number of crossings
- over/under
- relation to other crossings
- flypes

Operations changing knots:

- Reidenmeister moves
- flypes
- sum
- alternate crossings

Functions on knots:

- drawing
- enumeration/tabulation
- computation of invariants

Things to consider:

- crossings
- loops
- sub-knots or tangles *)

(* --------------- *)
(* Dowker notation *)

Definition dKnot := list nat.

Definition addOdds := id.

Definition draw := id. (* have to do it in haskell/gloss? what about proofs? *)

Definition enumerate (numCrossings : nat) := id.

(* Prove that all Dowker notations are drawable? *)
(* http://mathworld.wolfram.com/DowkerNotation.html *)

(* Enumerating the Dowker notation for n crossings must
cover all (alternating) knots of n crossings. Every (alternating) knot is expressible in Dowker notation, therefore it must be present in the enumerated list.

These are all knots of three crossings:
a
1 3 5
2 4 6

b
1 3 5
2 6 4

c
1 3 5
4 2 6

d
1 3 5
4 6 2

e
1 3 5
6 2 4

f
1 3 5
6 4 2

Any one with (n n+1) paired contains a self-loop that can be undone.
  (n, n+1) = 1 2, 2 3, 3 4, 4 5, 5 6
Then it will only have 2 crossings. There are no knots with two crossings.
Therefore everything containing a self-loop here is the unknot.
Therefore knots a, b, c, e, and f are the unknot.
Therefore d is the only one that may be a knot.
(It is indeed the trefoil knot).
(What about choices in how to draw c? Mirror of trefoil?)
Therefore there is only one knot of three crossings.

Still not clear on why all of these are drawable.
Not drawable = must draw a loop around a region containing an odd number of
crossings? *)

(* --------------- *)
(* Conway notation *)

Definition cKnot := list nat.

Definition tangle := list nat.

Definition flype (t : tangle) : tangle := t.




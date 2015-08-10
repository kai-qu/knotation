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

(* --------------- *)
(* Conway notation *)

Definition cKnot := list nat.

Definition tangle := list nat.

Definition flype (t : tangle) : tangle := t.




* Knotation: 
  how to tell a knot to a nerd over the phone, and 
  why you would want to

* Roadmap:
(I don't know how to slice the material)



* Thesis: / Why you should care about knot notation and enumeration
Knots are complex objects. Serializing them into text notation is hard. Mathematicians design these notations in order to encode insights and facilitate computation on knots, such as enumerating them. Studying the pros and cons of various knot notations will teach us lessons about programming languages, which are a form of notation that encode a complex object: 
- encoding insights takes extra manipulation, but allows ___? (do-notation)
- computation on knots...?? enumerating knots...?
  think about how many objects correspond to a particular program, and how many programs correspond to a particular object
- how easy is it for another [person/computer] to understand your notation
- is there a COMPLEXITY reason that Conway is better than Dowker? (not space)
  what makes it "eminently suitable for machine enumeration"?
- Princeton theses... I don't know...
- Coq... proofs...
- maybe I should just? write the knot enumeration code? and present it?

* Our idea of a knot: 
[shoelace/overhand knot with loose ends]

Mathematicians' idea of a knot:
[tie ends together -- line embedded nicely in R^3]

Imagine an ideal knot as made of very stretchy string:
[isotopy to trefoil]

* Mathematician John Conway:
  One thing I never figured out was how to tell a knot
  to a nerd over the phone! Keep in mind that the person
  you're talking to is just as much of a nerd as you are! [1]

[1] Knot tables exist

* Let's try some ways for you to tell me this knot over the phone:
[six-crossing knot]

* First: "It has six crossings!"

[1] Is an actual notation

* But that's not enough information! It could be this one. Or this one.
[6_1, 6_2, 6_3...]

* Second: Augment "six crossings" with more information!

----
* More information = a common protocol about how to draw/construct the knot.
(More topological terms?)
The next most obvious thing is to add a *direction* on top of the crossing number
/ each crossing. We do this by traveling around the knot.

Every crossing gets an odd and an even number -- why? (exercise)

* Give me the Dowker notation for 6_1. 
This knot has multiple notations, depending on which crossing I start at.

* Now, you tell me this notation over the phone. 
I try to draw 6_1. There are 4 possibilities. 
This notation corresponds to multiple knots.

[code]

* So, Dowker notation still has ambiguity

----

* Every knot is expressible in Dowker notation. If we systematically write out 
ALL Dowker notations, then we know our knot must be there! [use laziness?]

* Cool, so having a notation lets us ENUMERATE all the knots! e.g. 
for six crossings. 

* One application: Make numbered knot tables: so nerds can just say
"yes, I'm talking about the fifth entry in the Canonical
Recurse Center Knot Atlas 2015", and not have to solve
this problem again!

Other application: DNAngle

* Questions about Dowker and enumeration: 
  - Say you wanted to enumerate all knots of 6 crossings.
    Does the knot you're drawing actually have fewer crossings?
    Is it a link? Is it a duplicate of one you already have?

Let's try this. [code]

---

* Ways to describe complex things are important, because
  they make it easier or harder (along multiple axes) for humans
  to manipulate ideas!
     e.g. ease of writing notation, uniqueness, how easily
     important insights can be encoded and read off it,
     what information is lost and gained...
     like a powerful sort of serialization

* In fact, these is a very interesting notation that John Conway
  invented that encodes many interesting insights and makes enumeration
  easier -- but we don't have time to cover this!

* Thanks!
[I have to give a longer talk on this later, so 
ping me if you're interested in rubber-ducking the rest
of it!]

----

Disclaimer:
[] We usually try to enumerate the *prime*, *alternating* knots first,
   ignoring "mirror-image" ones
[] 

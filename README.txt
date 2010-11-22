ATP (Automated Theorem Prover)
Vasja Volin
vv2205@columbia.edu
===========================


Instructions
===========================
Usage: (atp kb nq)
KB is a list of clauses in the knowledge base.
nq is the negated query in a list of it's own.
See examples below for further details


General Information
===========================

This project is an FOL automated theorem prover built
using the standard CLisp package on Ubuntu/Linux.

The assignment assumed that statements were given in SNF. To
that end, clauses were represented in the following way:

A single clause is represented by the 'compound' struct, which
takes two parameters: 'op' and 'args'. To represent negated
clauses, 'op' is preceded with a '!' character. 'args' can be
any valid arguments : another symbol (constant), a compound
struct (representing a Skolem Function), a variable
(represented by a symbol with '?' as the first character),
or a list containing any of the previous. Example:

The sentence "Jack loves all animals" will be represented by:

	(list (m-c 'Animal '?x) (m-c 'Loves 'Jack '?x))

Since clauses within the knowledge base MUST be in SNF, they are
always separated by ORs. Since this is assumed, there is no
need to implement a direct representation for the OR symbol.
The same goes for the AND symbol in unification.

Clauses with multiple literals are simply lists of compound
structs, as given in the above example.

Knowledge bases are implemented as lists of lists. The external
list contains a list for each full clause in the KB. The internal
lists contain literals built from using the (make-compound ...)
or (m-c ...).

The negated query sohuld be just a single list containing one or
more literals, as above.

The automated theorem prover also implements unit preference and
linear input resolution. This helps in two ways:

-By using unit preference, clauses are reduced in size quickly, 
since unifying with a unit clause will always produce a resolvent
clause with one less literal than the non-unit clause. This is
implemented by sorting the knowledge base and 'list of support'
by the length of the clauses.

-By using linear resolution, checking for the end of a resolution
is simple: if a clause cannot resolve with anything from the
knowledge base or its ancestors, then the negated query cannot
be disproved, and hence the query cannot be proved. This is
implemented by simply choosing the resolvent as the next
query to be resolved. Also, it allows an easier printing of the
results, as follows:

**=====IMPORTANT=====**

Since linear input resolution follows the simple 'spine'
structure, I implemented a slightly different way of returning
resolvents. Since the resolvent query is always used as the
input query in the next resolution, the following format is
used in output:

(( a ) ( b )
   (( c ) ( d )
      (( e ) ( f )
         (( g ) ( nq ) Result))))

The clauses C, E, and G are the results of the resolution
shown on the previous line. I hope this is alright, given
that it is a much simpler way of displaying the resolution.


Functionality
===========================

This project implements the following basic Features:
-ATP (automated theorem proving)
-Linear Input Resolution (Input Resolution modified to be
	complete for non-horn clauses)
-Unit Preference (by sorting the kb and support list)



Sample Resolutions
===========================


1. Curiosity Killed the Cat
---------------------------

This is the knowledge base, given in English:

a. Everyone who loves all animals is loved by someone.
b. Anyone who kills an animal is loved by no one.
c. Jack Loves all animals.
d. Either Jack or Curiosity killed the cat.
e. The cat is named Tuna.

The query is the following:

q. Did Curiosity kill the cat?

This is the representation of the knowledge base and the
negated query, in the proper ATP format:

(defparameter *tuna-kb*
  (list
    (list (m-c 'Animal (m-c 'F '?x))
	      (m-c 'Loves (m-c 'G '?x) '?x))
    (list (m-c '!Loves '?x (m-c 'F '?x))
	      (m-c 'Loves (m-c 'G '?x) '?x))
    (list (m-c '!Loves '?y '?x)
	      (m-c '!Animal '?z)
          (m-c '!Kills '?x '?z))
    (list (m-c '!Animal '?x)
          (m-c 'Loves 'Jack '?x))
    (list (m-c 'Kills 'Jack 'Tuna)
          (m-c 'Kills 'Curiosity 'Tuna))
    (list (m-c 'Cat 'Tuna))
    (list (m-c '!Cat '?x)
          (m-c 'Animal '?x))))

(defparameter *tuna-query*
    (list (m-c '!Kills 'Curiosity 'Tuna)))

This is the result of the proof:

(atp *tuna-kb* *tuna-query*)

((!KILLS(CURIOSITY TUNA)) (KILLS(JACK TUNA) KILLS(CURIOSITY TUNA))
 ((KILLS(JACK TUNA)) (!LOVES(?Y ?X) !ANIMAL(?Z) !KILLS(?X ?Z))
  ((!LOVES(?Y3 JACK) !ANIMAL(TUNA)) (ANIMAL(F(?X)) LOVES(G(?X) ?X))
   ((!ANIMAL(TUNA) ANIMAL(F(JACK))) (!ANIMAL(?X) LOVES(JACK ?X))
    ((!ANIMAL(TUNA) LOVES(JACK F(JACK))) (!LOVES(?X F(?X)) LOVES(G(?X) ?X))
     ((!ANIMAL(TUNA) LOVES(G(JACK) JACK)) (!CAT(?X) ANIMAL(?X))
      ((LOVES(G(JACK) JACK) !CAT(TUNA)) (CAT(TUNA))
       ((LOVES(G(JACK) JACK)) (!LOVES(?Y3 JACK) !ANIMAL(TUNA))
        ((!ANIMAL(TUNA)) (!CAT(?X) ANIMAL(?X))
         ((!CAT(TUNA)) (CAT(TUNA)) CONTRADICTION))))))))))


2. West, The Criminal
---------------------------

This is the knowledge base, given in English:

a. It is a crime for an American to sell weapons to a hostile nation.
b. Nono has some missiles.
c. All of Nono's missles were sold to it by Colonel West.
d. Missiles are weapons.
e. An enemy of America counts as 'hostile'.
f. West is an American.
g. The country of Nono is an enemy of America.

The query is the following:

q. Is West a criminal?

This is the representation of the knowledge base and the
negated query, in the proper ATP format:

(defparameter *west-kb*
  (list
    (list (m-c '!American '?x)
          (m-c '!Weapon '?y)
          (m-c '!Sells '?x '?y '?z)
          (m-c '!Hostile '?z)
          (m-c 'Criminal '?x))
    (list (m-c '!Missle '?x)
          (m-c '!Owns 'Nono '?x)
          (m-c 'Sells 'West '?x 'Nono))
    (list (m-c '!Enemy '?x 'America)
          (m-c 'Hostile '?x))
    (list (m-c '!Missle '?x)
          (m-c 'Weapon '?x))
    (list (m-c 'Owns 'Nono 'M1))
    (list (m-c 'Missle 'M1))
    (list (m-c 'American 'West))
    (list (m-c 'Enemy 'Nono 'America))))

(defparameter *west-query*
    (list (m-c '!Criminal 'West)))

This is the result of the proof:

(atp *west-kb* *west-query*)

((!CRIMINAL(WEST))
 (!AMERICAN(?X) !WEAPON(?Y) !SELLS(?X ?Y ?Z) !HOSTILE(?Z) CRIMINAL(?X))
 ((!AMERICAN(WEST) !WEAPON(?Y12) !SELLS(WEST ?Y12 ?Z12) !HOSTILE(?Z12))
  (AMERICAN(WEST))
  ((!WEAPON(?Y12) !SELLS(WEST ?Y12 ?Z12) !HOSTILE(?Z12))
   (!ENEMY(?X AMERICA) HOSTILE(?X))
   ((!WEAPON(?Y12) !SELLS(WEST ?Y12 ?X14) !ENEMY(?X14 AMERICA))
    (ENEMY(NONO AMERICA))
    ((!WEAPON(?Y12) !SELLS(WEST ?Y12 NONO)) (!MISSLE(?X) WEAPON(?X))
     ((!SELLS(WEST ?X16 NONO) !MISSLE(?X16)) (MISSLE(M1))
      ((!SELLS(WEST M1 NONO)) (!MISSLE(?X) !OWNS(NONO ?X) SELLS(WEST ?X NONO))
       ((!MISSLE(M1) !OWNS(NONO M1)) (OWNS(NONO M1))
        ((!MISSLE(M1)) (MISSLE(M1)) CONTRADICTION)))))))))


3. The Smugglers
---------------------------

This is the knowledge base, given in English:

a. Customs officials search everyone who enteres the country who is not a VIP.
b. Some smugglers entered the country; They were only searched by smugglers.
c. No smuggler was a VIP.

The query is the following:

q. Were some customs officials smugglers?

This is the representation of the knowledge base and the
negated query, in the proper ATP format:

(defparameter *smuggler-kb*
  (list
    (list (m-c '!E '?x)
          (m-c 'V '?x)
          (m-c 'S '?x (m-c 'F '?x)))
    (list (m-c '!E '?x)
          (m-c 'V '?x)
          (m-c 'C (m-c 'F '?x)))
    (list (m-c 'P 'C))
    (list (m-c 'E 'C))
    (list (m-c '!S 'C '?y)
          (m-c 'P '?y))
    (list (m-c '!P '?z)
          (m-c '!V '?z))))

(defparameter *smuggler-query*
    (list (m-c '!P '?w)
          (m-c '!C '?w)))

This is the result of the proof:

(atp *smuggler-kb* *smuggler-query*)

((!P(?W) !C(?W)) (!E(?X) V(?X) C(F(?X)))
 ((!P(F(?X2)) !E(?X2) V(?X2)) (E(C))
  ((!P(F(C)) V(C)) (!S(C ?Y) P(?Y))
   ((V(C) !S(C F(C))) (!P(?Z) !V(?Z))
    ((!S(C F(C)) !P(C)) (P(C))
     ((!S(C F(C))) (!E(?X) V(?X) S(?X F(?X)))
      ((!E(C) V(C)) (E(C))
       ((V(C)) (!P(?Z) !V(?Z)) ((!P(C)) (P(C)) CONTRADICTION)))))))))


4. The Coral Reef Club
---------------------------

This is the knowledge base, given in English:

a. Every member likes water skiing or scuba diving.
b. No scuba diver likes big waves.
c. All water skiers like warm water.
d. Laura dislikes anything Jacob likes and likes anything Jacob dislikes.
e. Jacob likes warm water.
f. Jacob likes big waves.
g. Laura and Jacob belong to the coral reef club.

The query is the following:

q. Is there a member who is a scuba diver but not a water skiier?

This is the representation of the knowledge base and the
negated query, in the proper ATP format:

(defparameter *coral-club-kb*
  (list
    (list (m-c 'WS '?x)
          (m-c 'SD '?x))
    (list (m-c '!SD '?y)
          (m-c '!Likes '?y 'Waves))
    (list (m-c '!WS '?z)
          (m-c 'Likes '?z 'Warm))
    (list (m-c '!Likes 'Laura '?w)
          (m-c '!Likes 'Jacob '?w))
    (list (m-c 'Likes 'Jacob '?w)
          (m-c 'Likes 'Laura '?w))
    (list (m-c 'Likes 'Jacob 'Warm))
    (list (m-c 'Likes 'Jacob 'Waves))))

(defparameter *coral-club-query*
    (list (m-c '!SD '?v)
          (m-c 'WS '?v)))

This is the result of the proof:

(atp *coral-club-kb* *coral-club-query*)

((!SD(?V) WS(?V)) (WS(?X) SD(?X))
 ((WS(?X11) WS(?X11)) (!WS(?Z) LIKES(?Z WARM))
  ((LIKES(?Z12 WARM)) (!LIKES(LAURA ?W) !LIKES(JACOB ?W))
   ((!LIKES(JACOB WARM)) (LIKES(JACOB WARM)) CONTRADICTION))))

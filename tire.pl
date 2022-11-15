	/* cps721, assignment 5, Part 1: how to replace a flat tire */
        
/* The following rules are necessary if rules with the same predicate in the head 
   are not consecutive in your program. Read Handout "How to use Eclipse Prolog 6"
   in labs for details.    */ 
%% :- dynamic inside/3, on/3, tight/3, isClosed/2, fastened/2, inflated/2.
:- discontiguous(inside/3).
:- discontiguous(on/3).
:- discontiguous(tight/3).
:- discontiguous(isClosed/2).
:- discontiguous(fastened/2).
:- discontiguous(inflated/2).

	/* Universal situations and fluents based planner  */

solve_problem(G,L,N)  :-  C0 is cputime,
                   max_length(L,N), 
                   reachable(S,L), goal_state(G,S),
                   Cf is cputime, D is Cf - C0, nl,
                   write('Elapsed time (sec): '), write(D), nl.

reachable(S,[]) :- initial_state(S).

reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1).
/* % remove comments when you test your declarative heuristics %
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1),
                        not useless(M,History).
*/

legal_move([A | S], A, S) :- poss(A,S).

initial_state([]).

max_length([],N).
max_length([_|L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).

	%% Initial situation and a domain specification %%
:- [ 'initTire' ].
/* This is to compile the file initTire.pl  before you
   run a query. Do NOT insert this file here because your program 
   might be tested using different initial and goal states.      */

		/* Precondition Axioms */



		/* Successor State Axioms */

inside(Object, Container, [putAway(Object,Container) | S]) :- is_container(Container).
inside(Object, Container, [A | S]) :- inside( Object, Container, S ),
		not A=fetch(Object,Container).


		/* Declarative  Heuristics */


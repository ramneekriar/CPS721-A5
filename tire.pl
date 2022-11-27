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
poss(open(C), S) :-  is_container(C), 
                     isClosed(C, S).

poss(close(C), S) :- is_container(C), 
                     not isClosed(C, S).

poss(fetch(X, Y), S) :- is_container(Y),
                        inside(X, Y, S), 
                        not isClosed(Y, S).

poss(putaway(X, Y), S) :-  is_container(Y), 
                           not inside(X, Y, S), 
                           not isClosed(Y, S).

poss(loosen(X, Y), S) :-   nuts(X), 
                           hub(Y), 
                           have(wrench, S),
                           on(X, Y, S),
                           on(Y, ground, S),
                           tight(X, Y, S).

poss(tighten(X, Y), S) :-  nuts(X), 
                           hub(Y), 
                           have(wrench, S),
                           on(X, Y, S),
                           on(Y, ground, S),
                           not tight(X, Y, S).

poss(jackUp(Object), S) :- have(jack, S),
                           on(Object, ground, S),
                           not lifted(Object, S).

poss(jackDown(Object), S) :-  lifted(Object, S),
                              not on(Object, ground, S).

poss(putOn(Nuts, Hub), S) :-  nut(Nuts),
                              hub(Hub),
                              have(Nuts, S),
                              have(wrench, S),
                              not (Hub, ground, S),
                              not fastened(Hub, S).

poss(remove(Nuts, Hub), S) :- nut(Nuts),
                              hub(Hub),
                              fastened(Hub, S),
                              have(wrench, S),
                              not on(Hub, ground, S),
                              not on(Nuts, ground, S),
                              not tight(Nuts, Hub, S).

poss(putOn(Wheel, Hub), S) :- wheel(Wheel),
                              hub(Hub),
                              have(Wheel, S),
                              not on(Hub, ground, S),
                              not fastened(Hub, S),
                              not on(Wheel, Hub, S).

poss(remove(Wheel, Hub), S) :-   wheel(Wheel),
                                 hub(Hub),
                                 on(Wheel, Hub, S),
                                 not on(Hub, ground, S),
                                 not fastened(Hub, S).

		/* Successor State Axioms */

inflated(W, [A|S]) :- inflated(W, S).

isClosed(C, [close(C)|S]).
isClosed(C, [A|S]) :- isClosed(C, S), 
                      not (A = open(C)).

inside(Object, Container, [putAway(Object,Container) | S]).
inside(Object, Container, [A | S]) :- inside( Object, Container, S ),
		                                not (A = fetch(Object,Container)).

have(X, [fetch(X,C)|S]).
have(X, [A|S]) :- have(X, S),
                  not (A = putAway(X, C)).
have(X, [remove(X,Y)|S]).
have(X, [A|S]) :- have(X, S),
                  not (A = putOn(X,Y)).
have(X, [jackDown(X)|S]).
have(X, [A|S]) :- have(X, S),
                  not (A = jackUp(X)).

tight(N, H, [tighten(N, H)|S]).
tight(N, H, [A|S]) :- tight(N, H, S), 
                      not (A = loosen(N, H)).

lifted(X, [jackUp(X)|S]).
lifted(X, [A|S]) :- lifted(X, S),
                    not (A = jackDown(X)).

on(X, Y, [jackDown(X)|S]).
on(X, Y, [A|S]) :- on(X, Y, S),
                   not (A = jackUp(X)).

fastened(H, [putOn(N, H)|S]).
fastened(H, [A|S]) :- fastened(H, S), 
                      not (A = remove(N, H)).

free(H, [remove(W, H)|S]).
free(H, [A|S]) :- free(H,S), 
                  not (A = putOn(W, H)).

		/* Declarative  Heuristics */

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

% reachable(S2, [M | History]) :- reachable(S1,History),
%                         legal_move(S2,M,S1).

% remove comments when you test your declarative heuristics %
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1),
                        not useless(M,History).


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

% checked
poss(open(C), S) :-  is_container(C), 
                     isClosed(C, S).

% checked
poss(close(C), S) :- is_container(C), 
                     not isClosed(C, S).

% checked
poss(fetch(X, Y), S) :- is_container(Y),
                        inside(X, Y, S), 
                        not isClosed(Y, S).

% checked
poss(putAway(X, Y), S) :-  is_container(Y), 
                           have(X, S),
                           not isClosed(Y, S).

% checked
poss(loosen(X, Y), S) :-   nut(X), 
                           hub(Y), 
                           have(wrench, S),
                           on(X, Y, S),
                           on(Y, ground, S),
                           tight(X, Y, S).

% checked
poss(tighten(X, Y), S) :-  nut(X), 
                           hub(Y), 
                           have(wrench, S),
                           on(X, Y, S),
                           on(Y, ground, S),
                           not tight(X, Y, S).

% checked
poss(jackUp(Object), S) :- have(jack, S),
                           on(Object, ground, S),
                           not lifted(Object, S).

% checked
poss(jackDown(Object), S) :-  lifted(Object, S),
                              not on(Object, ground, S).

% checked
poss(putOn(Nut, Hub), S) :-   nut(Nut),
                              hub(Hub),
                              have(Nut, S),
                              have(wrench, S),
                              lifted(H, S),
                              not fastened(Hub, S).

% checked
poss(remove(Nut, Hub), S) :-  nut(Nut),
                              hub(Hub),
                              have(wrench, S),
                              fastened(Hub, S),
                              on(Nut, Hub, S),
                              lifted(Hub, S),
                              not tight(Nut, Hub, S).

% checked
poss(putOn(Wheel, Hub), S) :- wheel(Wheel),
                              hub(Hub),
                              have(Wheel, S),
                              lifted(Hub, S),
                              free(Hub, S),
                              not fastened(Hub, S),
                              not on(Wheel, Hub, S).

% checked
poss(remove(Wheel, Hub), S) :-   wheel(Wheel),
                                 hub(Hub),
                                 on(Wheel, Hub, S),
                                 lifted(Hub, S),
                                 not fastened(Hub, S).

		/* Successor State Axioms */

% checked
inflated(W, [A|S]) :- wheel(W), inflated(W, S).

% checked
isClosed(C, [close(C)|S]). % is_container(C) ?
isClosed(C, [A|S]) :- isClosed(C, S), 
                      not (A = open(C)).

% checked
inside(Object, Container, [putAway(Object,Container) | S]) :- is_container(Container).
inside(Object, Container, [A | S]) :- inside( Object, Container, S ),
		                                not (A = fetch(Object,Container)).

% checked
have(X, [fetch(X,C)|S]).
have(X, [remove(X,Y)|S]).  
have(jack, [jackDown(X)|S]).
have(X, [A|S]) :- have(X, S),
                  not (A = putAway(X, C)),
                  not (A = putOn(X,Y)),
                  not (X = jack, A = jackUp(Object)).

% checked
tight(N, H, [tighten(N, H)|S]).
tight(N, H, [A|S]) :- tight(N, H, S), 
                      not (A = loosen(N, H)).

% checked
lifted(X, [jackUp(X)|S]).
lifted(X, [A|S]) :- lifted(X, S),
                    not (A = jackDown(X)).

% checked
on(Object, ground, [jackDown(Object)|S]).
on(Object, ground, [A|S]) :- on(Object, ground, S),
                             not (A = jackUp(Object)).

on(Object, Hub, [putOn(Object, Hub)|S]).

on(W, H, [A|S]) :- wheel(W),
                   hub(H),
                   on(W, H, S),
                   not (A = remove(W, H)).

on(N, H, [A|S]) :- nut(N),
                   hub(H),
                   on(N, H, S),
                   not (A = remove(N, H)).

% checked
fastened(H, [putOn(N, H)|S]) :- nut(N).
fastened(H, [A|S]) :- fastened(H, S), 
                      not (
                      nut(N), 
                      A = remove(N, H)
                      ).

% checked
free(H, [remove(W, H)|S]) :- wheel(W).
free(H, [A|S]) :- free(H,S), 
                  not (
                  wheel(W), 
                  A = putOn(W, H)
                  ).

		/* Declarative  Heuristics */

% Useless to do redundant calls
useless(open(X), [close(X)|S]).
useless(close(X), [open(X)|S]).

% Useless to do redundant calls
useless(tighten(X, Y), [loosen(X, Y)|S]).
useless(loosen(X, Y), [tighten(X, Y)|S]).

% Need to jackUp before we can remove Nuts
useless(remove(N, H), [A|S]) :-  nut(N), 
                                 not (A = jackUp(H)).

% Useless to do redundant calls
useless(loosen(X,Y), [loosen(A,B)|S]).
useless(tighten(X,Y), [tighten(A,B)|S]).

% Need to jackDown before we can tighten
useless(tighten(X, Y), [A|S]) :- not (A = jackDown(Y)).

% Need to putAway an Object before we can close Container
useless(close(Container), [A|S]) :- not (A = putAway(Object, Container)).

% Useless to jackUp and then loosen Nuts since Hub cannot be lifted
useless(loosen(Nut, Hub), [jackUp(Hub)|S]).

% Have to loosen Nuts before we can jackUp
useless(jackUp(Hub), [A|S]) :- not (A = loosen(Nut, Hub)).

% Have to fetch the Wheels before we can loosen the Nuts
useless(loosen(Nuts, Hub), [A|S]) :- not (A = fetch(W, H)).

% Useless to do redundant calls
useless(putOn(X, Y), [remove(X, Y)|S]).
useless(remove(X, Y), [putOn(X, Y)|S]).

% Useless to do redundant calls
useless(putAway(X, _), [fetch(X, _)|S]).
useless(fetch(X, _), [putAway(X, _)|S]).

% Useless to put away Nuts
useless(putAway(X, Y), [A|S]) :- nut(X).

% Useless to do redundant calls
useless(jackUp(X), [jackDown(X)|S]).
useless(jackDown(X), [jackUp(X)|S]).

% Don't put away something and then later fetch it, and vice versa
useless(fetch(X, _), [ _, putAway(X, _) |S]).
useless(putAway(X, _), [ _, fetch(X, _) |S]).

useless(fetch(X, Y), [A|S]) :- not (A=fetch(Z, Y)), 
                               not (A = open(Y)).
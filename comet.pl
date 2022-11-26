	/* cps721, Part 2, NASA domain: taking images from satellites */
        
/* The following is necessary if rules with the same predicate in the head are not
 consecutive in your program. Read handout about Eclipse Prolog 6 for details.*/ 
:- dynamic pointsTo/3.

	/* Universal situations and fluents based planner  */

solve_problem(L,N)  :-  C0 is cputime,
                   max_length(L,N), 
                   reachable(S,L), goal_state(S),
                   Cf is cputime, D is Cf - C0, nl,
                   write('Elapsed time (sec): '), write(D), nl.

reachable(S,[]) :- initial_state(S).
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1).

/*
reachable(S2, [M | History]) :- reachable(S1,History),
                        legal_move(S2,M,S1),
                        not useless(M,History).
*/
legal_move([A | S], A, S) :- poss(A,S).

initial_state([]).

max_length([],N).
max_length([_|L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).

:- [ 'initNASA' ].
/* This is to compile the file initNASA.pl  before you
   run a query. Do NOT insert this file here because your program 
   will be tested using different initial and goal states.      */

		/* Precondition Axioms */

% power up an instrument Ins on a satellite Sat
% only if no other instrument at Sat is powered)
poss(up(Ins, Sat), S) :- powered(Ins, Sat, S), not (powered(OtherIns, Sat, S)), not Ins=OtherIns.

% power down an instrument Ins on a satellite Sat;
poss(down(Ins, Sat), S) :- not (powered(Ins, Sat, S)).

% turn a satellite Sat from a direction Dir1 to another direction Dir2;
poss(turnTo(Sat, Dir1, Dir2), S) :- available(Sat, Dir1), available(Sat, Dir2), pointsTo(Sat, Dir2, S).

% run calibrate procedure using a ground station G for an instrument Ins on a satellite Sat
poss(runCalibrateProc(Ins, Sat, G), S) :- target(Ins, G), calibrated(Ins, Sat, S).

% instrument Ins on a satellite Sat takes image of object in Dir using a mode M
poss(takeImage(Ins, Sat, M, Dir), S) :- supports(Ins, Sat, M), hasImage(Sat, M, Dir, S).


		/* Successor state axioms */


		/* Declarative heuristics */


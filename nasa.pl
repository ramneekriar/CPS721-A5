	/* cps721, Part 3, NASA domain: taking images from satellites */
        
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
                        legal_move(S2,M,S1),
                        not useless(M,History).

legal_move([A | S], A, S) :- poss(A,S).

initial_state([]).

max_length([],N).
max_length([_|L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).

:- [ 'initNASA' ].
/* This is to compile the file initNASA.pl  before you
   run a query. Do NOT insert this file here because your program 
   will be tested using different initial and goal states.      */

%         /* Precondition Axioms */

% power up an instrument Ins on a satellite Sat
% only if no other instrument at Sat is powered)
poss(up(Ins, Sat), S) :-   supports(Ins, Sat, M1),
                           not (
                           powered(Ins, Sat, S),
                           supports(OtherIns, Sat, M2),
                           powered(OtherIns, Sat, S),
                           not OtherIns=Ins).

% power down an instrument Ins on a satellite Sat;
poss(down(Ins, Sat), S) :- powered(Ins, Sat, S).

% turn a satellite Sat from a direction Dir1 to another direction Dir2;
poss(turnTo(Sat, Dir1, Dir2), S) :- available(Sat, Dir1), 
                                    available(Sat, Dir2),
                                    not (pointsTo(Sat, Dir2, S)),
                                    not Dir1 = Dir2.

% run calibrate procedure using a ground station G for an instrument Ins on a satellite Sat
poss(runCalibrateProc(Ins, Sat, G), S) :- pointsTo(Sat, G, S),
                                          target(Ins, G),
                                          powered(Ins, Sat, S),
                                          not (calibrated(Ins, Sat, S)).


% instrument Ins on a satellite Sat takes image of object in Dir using a mode M
poss(takeImage(Ins, Sat, M, Dir), S) :-   supports(Ins, Sat, M),
                                          available(Sat, Dir),
                                          powered(Ins, Sat, S), 
                                          calibrated(Ins, Sat, S), 
                                          pointsTo(Sat, Dir, S),
                                          not (hasImage(Sat, M, Dir, S)).

		/* Successor state axioms */

% Instr on Satell is powered in a situation S
powered(Ins, Sat, [ up(Ins, Sat) | S]).
powered(Ins, Sat, [A|S]) :-   powered(Ins, Sat, S), 
                              not (A = down(Ins, Sat)).

% A satellite Satell points in a direction Dir in a situation S
pointsTo(Sat, Dir, [ turnTo(Sat, OtherDir, Dir) | S]).
pointsTo(Sat, Dir, [A|S]) :-    pointsTo(Sat, Dir, S), 
                                not (A = turnTo(Sat, Dir, OtherDir)).

% Instr on Satell is calibrated in a situation S
calibrated(Ins, Sat, [ runCalibrateProc(Ins, Sat, G) | S]).
calibrated(Ins, Sat, [A|S]) :-   calibrated(Ins, Sat, S),
                                 not (A = up(Ins, Sat)).

% A satellite Sat has an image in mode M of an object in Dir in a situation S
hasImage(Sat, M, Dir, [ takeImage(Ins, Sat, M, Dir) |S]).
hasImage(Sat, M , Dir, [A|S]) :- hasImage(Sat, M, Dir, S).

		/* Declarative heuristics */

% Useless redundant actions
useless(up(Ins, Sat), [ up(Ins, Sat) |S]).
useless(up(Ins, Sat), [ down(Ins, Sat) |S]).

% Useless redundant actions
useless(down(Ins, Sat), [ down(Ins, Sat) |S]).
useless(down(Ins, Sat), [ up(Ins, Sat) |S]).

useless(turnTo(Sat, Dir1, Dir2), [ turnTo(Sat, Dir3, Dir4) |S]).
useless(turnTo(Sat, Dir1, Dir2), [ turnTo(Sat, Dir2, Dir1) |S]).
useless(turnTo(Sat, _, _), [ turnTo(Sat, _, _) |S]).

% Don't take an image of the same thing twice
useless(takeImage(Ins, Sat, M, Dir), [ takeImage(Ins, Sat, M, Dir) |S]).

useless(takeImage(Ins, Sat, M, Dir), [ down(Ins, Sat) | S]).

useless(runCalibrateProc(Ins, Sat, G), [ down(Ins, Sat) | S]).
useless(down(Ins, Sat), [ runCalibrateProc(Ins, Sat, G) | S]).
useless(up(Ins, Sat), [ runCalibrateProc(Ins, Sat, G) | S]).

% Don't turn unless it isn't the ground station
useless(up(Sat, Ins), [turnTo(Sat, OtherDir, Dir)|S]) :- target(Ins, G), not (G=Dir).
useless(turnTo(Sat, OtherDir, Dir), [ down(Ins, Sat) |S]) :- target(Ins, G), not (G=Dir).


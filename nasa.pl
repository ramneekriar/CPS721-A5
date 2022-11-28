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

        /* Precondition Axioms */

% power up an instrument Ins on a satellite Sat
% only if no other instrument at Sat is powered)
poss(up(Ins, Sat), S) :-   supports(Ins, Sat, _),
                           not (
                           powered(Ins, Sat, S),
                           powered(OtherIns, Sat, S), 
                           not Ins=OtherIns).

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
                                          powered(Ins, Sat, S).

% instrument Ins on a satellite Sat takes image of object in Dir using a mode M
poss(takeImage(Ins, Sat, M, Dir), S) :-   supports(Ins, Sat, M),
                                          available(Sat, Dir),
                                          powered(Ins, Sat, S), 
                                          calibrated(Ins, Sat, S), 
                                          pointsTo(Sat, Dir, S).


		/* Successor state axioms */

% Instr on Satell is powered in a situation S
powered(Ins, Sat, [ up(Ins, Sat) | S]).
powered(Ins, Sat, [A|S]) :-   powered(Ins, Sat, S), 
                              not (A = down(Ins, Sat)).

% A satellite Satell points in a direction Dir in a situation S
pointsTo(Sat, Dir, [ turnTo(Sat, Dir1, Dir) | S]).
pointsTo(Sat, Dir, [A|S]) :-    pointsTo(Sat, Dir, S), 
                                not (A = turnTo(Sat, Dir1, Dir2)),
                                not Dir = Dir2.

% Instr on Satell is calibrated in a situation S
calibrated(Ins, Sat, [ runCalibrateProc(Ins, Sat, G) | S]).
calibrated(Ins, Sat, [A|S]) :-   calibrated(Ins, Sat, S),
                                 not (A = runCalibrateProc(Ins, Sat, G)).

% A satellite Sat has an image in mode M of an object in Dir in a situation S
hasImage(Sat, M, Dir, [ takeImage(Ins, Sat, M, Dir) |S]).
hasImage(Sat, M , Dir, [A|S]) :- hasImage(Sat, M, Dir, S),
                                 not (A = takeImage(Ins, Sat, M, Dir)).



		/* Declarative heuristics */
useless(up(Ins, Sat), [A|S]) :- A = up(Ins, Sat).
useless(down(Ins, Sat), [A|S]) :- A = down(Ins, Sat).

useless(up(Ins, Sat), [A|S]) :- A = down(Ins, Sat).
useless(down(Ins, Sat), [A|S]) :- A = up(Ins, Sat).

useless(turnTo(Sat, Dir1, Dir2), [A|S]) :- A = turnTo(Sat, Dir3, Dir4).

useless(takeImage(Ins, Sat, M, Dir), [A|S]) :- A = takeImage(Ins, Sat, M, Dir).

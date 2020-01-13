% -*- Mode: Prolog -*-
% The expert system from the Ulf Nilsson--Jan Maluszynski book.
% See http://www.ida.liu.se/~ulfni/lpp
%
% Antti J Ylikoski 2012-06-27.
%
% Has to repair this program somewhat to get it working.  Nilsson et
% al. evidently have not attempted to run their program!
%
% Run the system by consulting this file:
% ?- ['C:\\prolog\\expert1.pro'].
% 
% and executing the predicate 'expert':
% ?- expert.
%
% I have written this for the SWI-Prolog:
% http://www.swi-prolog.com
% https://www.swi-prolog.org/download/stable




/*
:- unknown(_, fail). % Fail rather than raise exceptions for undef pred
                     % This one is there in the SWI-Prolog -- but it
                     % cannot be used because of the idiosyncracies of
                     % the development system!
*/

:- dynamic(known/3). % So make known/3 dynamic.

:- op(960, xfx, +++).  % AND of two individual proofs.
:- op(959, xfx, if).   % this one is not implication -- it builds rules
:- op(958, xfy, &&&).  % conjunction
:- op(957, fy,  ~).    % negation


% Aux predicate: retract what is known, but always succeed.

forget :-
    (retract(known(_, _, _))
    ;
     true
    ).

% The 'main routine' so to speak.

expert :-
    forget,
    exp_shell(malfunctions(car), X),
    nl,
    print_proof(X).


% The rule base.

% The system X is faulty if it is imediately faulty, trivially.
malfunctions(X) if immediately_faulty(X).

% The subsystem X will malfunction if X needs the subsubsystem Y
% and the subsubsystem Y malfunctions.
malfunctions(X) if needs(X, Y) &&& malfunctions(Y).

% The subsystem X is immediately faulty if it exhibits the symptom Y
% and it is not indirectly faulty.
immediately_faulty(X) if symptom(Y, X) &&& ~ indirect(X).

% Subsystem X is indirectly faulty if it needs the subsubsystem Y and
% the subsubsystem Y malfunctions.
indirect(X) if needs(X, Y) &&& malfunctions(Y).

% The "subsystem A needs the subsystem B" relationships.
needs(car, ignition_system) if true.
needs(car, fuel_system) if true.
needs(car, electric_system) if true.
needs(ignition_system, starting_engine) if true.
needs(ignition_system, spark_plugs) if true.
needs(electric_system, fuse) if true.
needs(electric_system, battery) if true.
needs(fuel_system, fuel_pump) if true.
needs(spark_plugs, battery) if true.
needs(starting_engine, battery) if true.
needs(fuel_pump, fuel) if true.


% The inference engine.

exp_shell(true, void).  % The proof of 'true' is empty.

% To prove a conjunction of two facts X and Y, create two proofs for
% both X, and Y individually.
exp_shell(X &&& Y, P1 +++ P2) :-
    exp_shell(X, P1), exp_shell(Y, P2).

% To prove the negation of a fact X, prove the negation of its proof.
exp_shell(~ X, proof(~ X, void)) :-
    \+ exp_shell(X, P).

% To prove that X exhibits the symptom Y, prove the 'confirm'
% relation.
% To say that "symptom(X, Y)" denotes that, Y exhibits the symptom X.

exp_shell(symptom(X, Y), proof(symptom(X, Y), void)) :-
    confirm(X, Y).

% To prove X, look for a rule 'X if Y' and attempt to prove Y.
exp_shell(X, proof(X, P)) :-
    (X if Y), exp_shell(Y, P).


% The user inferface predicates.
% To say that "ask(A, B)" denotes that, B has the symptom A.
ask(worn_out, spark_plugs) :-
    write('Do any of the spark plugs fail to produce a spark? ').
ask(out_of, fuel) :-
    write('Does the fuel gauge indicate an empty tank? ').
ask(broken, fuel_pump) :-
    write('Does the fuel pump fail to feed the engine any fuel? ').
ask(broken, fuse) :-
    write('Is any one of the fuses out of order? ').
ask(discharged, battery) :-
    write('Is the battery voltage under 11 volts? ').
ask(broken, starting_engine) :-
    write('Is the starting engine silent? ').


% The confirm predicate to confirm that a state of facts is true.
% to say that "confirm(A, B)" denotes that, "ask(A, B)" is true, which
% in turn denotes that, B has the symptom A.
confirm(X, Y) :-
    known(X, Y, true).

confirm(X, Y) :-
    \+ known(X, Y, Z),
    nl,
    ask(X, Y),
    read(A),
    remember(X, Y, A).



% Memoize the answers given by the user.

remember(X, Y, yes) :-
    assertz(known(X, Y, true)).

remember(X, Y, no) :-
    assertz(known(X, Y, false)).

% The negation.

~ X :-
    \+ X.


% The "output the proof" predicate print_proof().

% If the proof is empty, print nothing.
print_proof(void).

% print the proof of the union of two proofs:
print_proof(X +++ Y) :-
    print_proof(X), nl, print_proof(Y).

% Print the proof of something which is trivially known so that its
% proof is empty:
print_proof(proof(X, void)) :-
    write(X), nl.

% Now let the proof of the fact X be Y.  Print these in the general
% case:
print_proof(proof(X, Y)) :-
    \+ ((Y = void)),
    write(X), write(' BECAUSE '), nl,
    print_children(Y), nl, print_proof(Y).

% Now let the proof of X be Y, and we want to print the facts of a
% union of two proofs ('+++').  Therefore:
print_children(proof(X, Y) +++ Z) :-
	      tab(8), write(X), write(' AND '), nl, print_children(Z).

% Print a singleton fact X with the proof Y.
print_children(proof(X, Y)) :-
    tab(8), write(X), nl.


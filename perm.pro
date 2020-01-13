% -*- Mode: Prolog -*-
% Permutations in Prolog.
%
% Very handy.  AJY 2012-06-24.

del0(X, [X|T], T).
del0(X, [Y|T], [Y|T1]) :-
    del0(X, T, T1).

insert(X, L, I) :-
    del0(X, I, L).

perm([], []).
perm([X|L], P) :-
    perm(L, P1),
    insert(X, P1, P).


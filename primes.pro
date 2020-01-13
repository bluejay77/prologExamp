% -*- Mode: Prolog -*-
% Primes smaller than N with the bagof() predicate.
%
% AJY 2012-07-01.
% This file is primes.pro
%

prime(N) :-
    divis(N, L),
    forall(member(D, L), N rem D =\= 0).

% General set of integers [m, m+1, m+2, ...., n].
%
% AJY 2012-07-01.
int_set(M, N, []) :-
    M > N, !.
int_set(M, N, [M]) :-
    M =:= N, !.
int_set(M, N, List) :-
    Aux is N - 1,
    int_set(M, Aux, ListAux),
    append(ListAux, [N], List).

% Divisors of N to try to test its primeness.
divis(N, List) :-
    Upper is N div 2,
    int_set(2, Upper, List).

intAndPrime(Intg, St) :-
    member(Intg, St),
    prime(Intg).

primes(Hi) :-
    int_set(1, Hi, SET),
    setof(Integ, intAndPrime(Integ, SET), PrimesSet),
    nl, write(PrimesSet), nl.

time1000 :-
    time(primes(1000)).


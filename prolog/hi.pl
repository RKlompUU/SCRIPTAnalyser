:- use_module(library(clpfd)).

s1 :-
T1bs in 32,
T0bs in 2,
T1bs #= T0bs,
true.

test(X2) :-
  X in 0..10 \/ 51..15000000,
  true.

isT(X) :- X.

:- use_module(library(clpfd)).

s1 :-
  R in 1..1293321,
  F in 0,
  (R /\ F) #\= 0.

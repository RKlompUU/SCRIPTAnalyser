:- use_module(library(clpfd)).


b(X) :-
  X in 1..1293321,
  D #> 0,
  fd_size(X,D).

s1 :-
  R in 1..1293321,
  F in 2,
  b(R),
  b(F).

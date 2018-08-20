:- use_module(library(clpfd)).

s(T10ints) :-
(T10ints in (-2147483647)..(2147483647)),
T10ints #= 0,
T10ints #= 1,
(#\ 0).

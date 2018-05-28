:- use_module(library(clpfd)).


b(X) :-
  X in 0,
  D #> 0,
  fd_size(X,D).

increaseK(+E) :-
  b_getval(knowledge,K),
  KNew is K + 1,
  b_setval(knowledge,KNew),
  E.

test(R) :-
  increaseK,
  R is 1,
  true.

s1 :-
  R in 1..129321,
  F in 130000,
  OK in 1..10,
  b(R),
  b(F),
  R #= F #\/ 1.

s2 :-
  b_setval(knowledge,0),
  s1,
  b_getval(knowledge,KNew),
  write(KNew).



s3 :-
(T5bs in 0),
(T5ints in 0),
(T17bs in 0),
(T17ints in 0),
(T15bs in (0)..(1)),
(T15ints in (0)..(1)),
(T8bs in 32),
(T11bs in (0)..(520)),
(T11ints in (-2147483647)..(2147483647)),
(T3bs in 32),
(T16bs in (0)..(1)),
(T16ints in (0)..(1)),
(T14bs in (0)..(1)),
(T14ints in (0)..(1)),
(T7bs in (0)..(4)),
(T7ints in (0)..(520)),
(T11bs in (0)..(520)),
(T11ints in (-2147483647)..(2147483647)),
(T4bs in 1),
(T4ints in 67),
(T13bs in (0)..(1)),
(T13ints in (0)..(1)),
(T2bs in 1),
(T2ints in 64),
(T7bs in (0)..(4)),
(T7ints in (0)..(520)),
(T11bs in (0)..(520)),
(T11ints in (-2147483647)..(2147483647)),
b(((T8bs #= T3bs) #\/ 0) #/\
(T7ints #< T4ints) #/\
(T2ints #=< T7ints) #/\
  (#\ 0)),
1.

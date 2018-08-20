:- use_module(library(clpfd)).

s :-
(T10bs in (0)..(4)),
(T10ints in (-2147483647)..(2147483647)),
(T8bs in (0)..(4)),
(T8ints in 4194549),
(T2bs in (0)..(4)),
(T2ints in (-2147483647)..(2147483647)),
(T9bs in (0)..(4)),
(T9ints in (-2147483647)..(2147483647)),
(T1bs in (0)..(4)),
(T1ints in (-2147483647)..(2147483647)),
(T3bs in 0),
(T3ints in 0),
(T6bs in (0)..(520)),
(T6ints in (-549755813887)..(549755813887)),
(T0bs in 1),
(T0ints in 0),
(T10ints #= (T8ints /\ 0x00400000)),
(T9ints #= (T8ints /\ 0x0000FFFF)),
(T6ints #\= 0),
(T0ints #\= 0),
(#\ 0).

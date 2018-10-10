:- use_module(library(clpfd)).

s(N, M) :-
(Xs = [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42]),
(Xs ins 0..520),
(MAX_PUBS in 20),

(N #=< MAX_PUBS),
(N #> 0),
(M #=< N),
(M #> 0),

(PosPUBS #= N),
(PosNPRIVS #= N + 1),
(PosPRIVS #= N + 1 + M),

(X0 in 0..4),
(X3 in 20),
(X5 in 20),
(X7 in 20),

(element(PosPUBS, Xs, P)),
(P in 20),

(element(PosNPRIVS, Xs, NPRIVS)),
(NPRIVS in 0..4),

(element(PosPRIVS, Xs, PRIV)),
(PRIV in 33\/67),

(#\ 0).

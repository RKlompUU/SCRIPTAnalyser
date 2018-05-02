isT(X0) :-
  X0 #= 1.
isF(X0) :-
  X0 #= 0.

r(X0,X1) :-
  isT(X0), 1 \= 0,\+ (5 > 5 - 2 * 0),X0 #>= 5,isT(X1);false.

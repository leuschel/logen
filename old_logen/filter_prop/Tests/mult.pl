p(Z1,Z2) :-
	mult(X,Y,Z1),
	mult(X,Y,Z2),
	neq(Z1,Z2).

mult(0,_,0).
mult(s(X),Y,Z) :-
	mult(X,Y,XY),
	plus(XY,Y,Z).

plus(0,X,X).
plus(s(X),Y,s(Z)) :-
	plus(X,Y,Z).

neq(0,s(_)).
neq(s(_),0).
neq(s(X),s(Y)) :-
	neq(X,Y).
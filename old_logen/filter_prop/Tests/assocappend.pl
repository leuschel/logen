app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

g(W1,W2) :-
	app(X,Y,U),
	app(U,Z,W1),
	app(Y,Z,V),
	app(X,V,W2),
	neq(W1,W2).

/*
neq([],[_|_]).
neq([_|_],[]).
neq([X|Y],[X1|Y1]) :-
	neq(X,X1).
neq([X|Y],[X1|Y1]) :-
	neq(Y,Y1).
*/

neq([],[_|_]).
neq([_|_],[]).
neq([X|Y],[X1|Y1]) :-
	neq(X,X1),
	anyterm(Y),
	anyterm(Y1).
neq([X|Y],[X1|Y1]) :-
	neq(Y,Y1),
	anyterm(X),
	anyterm(X1).


anyterm(_).
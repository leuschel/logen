:- dynamic subset/2.

subset(X,Y) :- forall(U,el(U,Y) <= el(U,X)).

:- dynamic el/2.

el(X,Y) :- mem(X,Y).

:- dynamic mem/2.

mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).
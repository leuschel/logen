memb(X,[X|_]).
memb(X,[_|Xs]) :-
	memb(X,Xs).

unsafe :- 
	listb(X), memb(b,X).

listb([]).
listb([a|Bs]) :-
	listb(Bs).

:- filter(unsafe).

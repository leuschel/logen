rev(L,R) :-
	revacc(L,[],R).

revacc([],A,A).
revacc([H|X],A,R) :-
	revacc(X, [H|A], R).

rev2(L,R) :-
	revacc2(L,[],R).
revacc2([],A,A).
revacc2([H|X],A,R) :-
	revacc2(X, [H|A], R).
	
rev3(L,R) :-
	revacc3(L,[],R).
revacc3([],A,A).
revacc3(XX,A,R) :-
	(XX=[H|X] -> revacc3(X, [H|A], R) ;  fail).

a:- a,a.

rec(X) :- findall(X, rec(X), R).

b(X,Y) :- a, '=..'(X,Y).

inf_nr(X) :- X1 is X+1, inf_nr(X1).
inf_nr2(X) :- X1 is X+1, inf_nr2(X1).

/* memo arg is not static */
filter_err(X) :- X=Y, filter_err(Y).
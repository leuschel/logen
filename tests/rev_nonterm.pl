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


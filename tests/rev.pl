rev(L,R) :-
	rev(L,[],R).

rev([],A,A).
rev([H|X],A,R) :-
	rev(X, [H|A], R).

rev2(L,R) :-
	rev2(L,[],R).
rev2([],A,A).
rev2([H|X],A,R) :-
	rev2(X, [H|A], R).


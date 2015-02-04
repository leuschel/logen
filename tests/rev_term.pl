rev(L,R) :-
	revacc(L,[],R).

revacc([],A,A).
revacc([H|X],A,R) :-
	revacc(X, [H|A], R).


test(R) :-
  length(L,10000),
  rev(L,R).
  
  
length([],0).
length([_|T],N) :- N>0, N1 is N-1, length(T,N1).

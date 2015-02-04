match1([],_,_,_) :-
	 true.
match1([A|_],[B|_],C,[_|D]) :-
	 A\==B,
	 match1(C,D,C,D).
match1([A|B],[A|C],D,E) :-
	 match1(B,C,D,E).
	 
match(A,B) :-
	 match1(A,B,A,B).
	 
%:- filter(match(list,dynamic)).

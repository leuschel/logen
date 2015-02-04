match(Pat,T) :- match1(Pat,T,Pat,T).

match1([],Ts,P,T).
match1([A|Ps],[B|Ts],P,[X|T]) :-
	A\==B,
	match1_m(P,T,P,T).
match1([A|Ps],[A|Ts],P,T) :-
	match1(Ps,Ts,P,T).

match1_m(_,_,_,_).
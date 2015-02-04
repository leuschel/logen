:- module(cartprod, [cartprod/2]).


cartprod([S1,S2|Ss],P) :-
	cartprod([S2|Ss], P1),
	times(S1,P1,P,[]).
cartprod([S],S1) :-
	singletons(S,S1).
cartprod([],[]).

singletons([],[]).
singletons([A|As],[[A]|Bs]) :-
	singletons(As,Bs).

times([A|As],B,C0,C2) :-
	element_prod(B,A,C0,C1),
	times(As,B,C1,C2).
times([],_,C,C).

element_prod([],_,C,C).
element_prod([B|Bs],A,[[A|B]|C0],C1) :-
	element_prod(Bs,A,C0,C1).

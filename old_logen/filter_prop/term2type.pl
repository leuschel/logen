:- module(term2type,[term2type/2]).

term2type(A,Def) :-
	term2type6(A,_,Def,[],0,_).

term2type6(T, dynamic,As,As,K,K) :-
        var(T),
        !.
term2type6(T, R,[(L -> R)|As],As1,K,K2) :-
        newname(K,R),
        K1 is K+1,
        T =.. [F|Xs],
        argtypes(Xs,Qs,As,As1,K1,K2),
        L =.. [F|Qs].

argtypes([],[],As,As,K,K).
argtypes([T|Ts],[Q|Qs],As0,As2,K0,K2) :-
        term2type6(T,Q,As0,As1,K0,K1),
        argtypes(Ts,Qs,As1,As2,K1,K2).
        
newname(K,P) :-
	name(q,QQ),
	name(K,KN),
	append(QQ,KN,QKN),
	name(P,QKN).


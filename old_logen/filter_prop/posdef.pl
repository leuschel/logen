:- module(posdef, [pgen/1]).

:- use_module(library(lists)).

% program to generate the worst-case programs for Def analysis
% See Codish-Genaim paper

pgen(N) :-
	open('postest.pl',write,S),
	genBaseClause(N,S),
	genRecClauses(N,N,S),
	close(S).
	
genBaseClause(N,S) :-
	nEqVars(N,Xs,_),
	H =.. [p|Xs],
	numbervars(H,0,_),
	write(S,H),
	write(S,'.'),
	nl(S).
	
genRecClauses(0,_,_).
genRecClauses(J,N,S) :-
	J > 0,
	J1 is J-1,
	nDifferentVars(J1,Ys),
	J2 is N-J,
	nEqVars(J2,Xs,X1),
	append(Ys,[c|Xs],Zs),
	H =.. [p|Zs],
	nEqVars(J2,Cs,c),
	append(Ys,[X1|Cs],Ws),
	reverse(Ws,Ws1),
	numbervars(Ws1,0,_),
	B =.. [p|Ws],
	write(S,(H :- B)),
	write(S,'.'),
	nl(S),
	genRecClauses(J1,N,S).
	
nEqVars(0,[],_).
nEqVars(N,[X|Xs],X) :-
	N>0,
	N1 is N-1,
	nEqVars(N1,Xs,X).
	
nDifferentVars(0,[]).
nDifferentVars(N,[_|Xs]) :-
	N>0,
	N1 is N-1,
	nDifferentVars(N1,Xs).
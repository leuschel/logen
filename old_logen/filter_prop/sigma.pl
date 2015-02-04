:- module(sigma, [sigma/2, fsigma/2]).

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(canonical).

% fsigma(F,S): extracts the set of functions S in a program file F
% E.g. 
% ?- fsigma('../Exs/dnf.pl',S).
% Finished reading ../Exs/dnf.pl
% Number of clauses: 33
% 
% 
% S = [a/2,o/2,n/1,z9/0,z8/0,z7/0,z6/0,z5/0,z4/0,z3/0,z2/0,z1/0,z0/0] ? 
%

	
fsigma(F,Sig) :-
	readprog(F,Cls),
	stripLogenAnnotations(Cls,Cls1),
	sigma(Cls1,Sig).
	
sigma([predicates(_)|Cls], Sig) :-
	sigma3(Cls,[],Sig).
	
sigma3([clause((filter(_) :- _),_)|Cls], Sig0,Sig2) :-
	!,
	sigma3(Cls,Sig0,Sig2).
sigma3([clause((H :- B),_)|Cls], Sig0,Sig3) :-
	traverseAtom(H,Sig0,Sig1),
	traverseBody(B,Sig1,Sig2),
	sigma3(Cls,Sig2,Sig3).
sigma3([],Sig,Sig).

traverseAtom(H,S0,S1) :-
	H =..[_|Xs],
	traverseArgs(Xs,S0,S1).
	
traverseBody(true,S,S) :-
	!.
traverseBody((B,Bs),S0,S2) :-
	!,
	traverseAtom(B,S0,S1),
	traverseBody(Bs,S1,S2).
traverseBody(B,S0,S1) :-
	traverseAtom(B,S0,S1).
	
traverseArgs([],S,S).
traverseArgs([X|Xs],S0,S2) :-
	traverse(X,S0,S1),
	traverseArgs(Xs,S1,S2).
	
traverse(T,S,S) :-
	variable(T),
	!.
traverse(T,S0,S2) :-
	functor(T,F,N),
	addFunctor(F/N,S0,S1),
	T =..[F|Xs],
	traverseArgs(Xs,S1,S2).
	
addFunctor(X,Ys,Ys) :-
	member(X,Ys),
	!.
addFunctor(X,Ys,[X|Ys]).

writelist([]).
writelist([X|Xs]) :-
	write(user_error,X),
	nl(user_error),
	writelist(Xs).
	
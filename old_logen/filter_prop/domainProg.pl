:- module(domainProg, [domainProg/2]).

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(flatnames).
:- use_module(canonical).

% builds a domain program, replacing non-variable arguments by
% calls to a "denotes" predicate.  The latter can be given by
% an abstract interpretation.
	
domainProg([predicates(Ps)|Cls], [predicates(Ps)|Rs]) :-
	domainProg(Cls,Rs).
domainProg([clause(Cl,Ws)|Cls], [clause((H :- R1),Ws1)|Rs]) :-
	melt(clause(Cl,Ws),clause(Cl1,Ws1)),
	flatten_args(Cl1,(H :- B),Ds),
	revgoals(Ds,Ds1,true),
	joingoals1(B,Ds1,R),
	removetrue(R,R1),
	canonical(clause((H :- R1),Ws1)),
	domainProg(Cls,Rs).
domainProg([],[]).

flatten_args(X,Y,(Den,Ps)) :-
	inner_clause_subterm(X,T,X1,U),
	!,
	flatten_denotes(denotes(T,U),Den),
	flatten_args(X1,Y,Ps).
flatten_args(X,X,true).

revgoals((B,Bs),Ds0,Ds1) :-
	!,
	revgoals(Bs,Ds0,(B,Ds1)).
revgoals(true,Ds,Ds) :-
	!.
revgoals(B,(B,Ds),Ds).

% subterm(T,S,T1,S1):  S is a subterm of T;  
% S1 is a variable that occurs in T1 at the place where S was in T.

inner_clause_subterm((H :- B),T,(X1 :- B),T1) :-
	inner_atom_subterm(H,T,X1,T1).
inner_clause_subterm((H :- B),T,(H :- X1),T1) :-
	inner_body_subterm(B,T,X1,T1).

inner_atom_subterm(H,T,H1,T1) :-
	H =.. [P|Xs],
	inner_arg_subterm(Xs,T,Xs1,T1),
	H1 =.. [P|Xs1].

inner_body_subterm((B,Bs),T,(X,Bs),T1) :-
	inner_atom_subterm(B,T,X,T1).
inner_body_subterm((B,Bs),T,(B,X),T1) :-
	!,
	inner_body_subterm(Bs,T,X,T1).
inner_body_subterm(B,T,X,T1) :-
	inner_atom_subterm(B,T,X,T1).

inner_arg_subterm([X|Xs],T,[X1|Xs],T1) :-
	inner_subterm(X,T,X1,T1).
inner_arg_subterm([X|Xs],T,[X|Xs1],T1) :-
	inner_arg_subterm(Xs,T,Xs1,T1).

inner_subterm(X,T,X1,T1) :-
	subterm(X,T,X1,T1),
	nonvar(T),
	T =.. [_|Zs],
	allvars(Zs).

allvars([]).
allvars([X|Xs]) :-
	var(X),
	allvars(Xs).

subterm(T,T,X,X) :-
	nonvar(T).
subterm(T,R,X,Y) :-
	nonvar(T),
	T =.. [F|Xs],		
	replace(Xs,R1,Ys,Y1),
	subterm(R1,R,Y1,Y),
	X =.. [F|Ys].
	
replace([X|Xs],X,[Y|Xs],Y).
replace([X|Xs],U,[X|Ys],Y) :-
	replace(Xs,U,Ys,Y).


removetrue((B,true),B) :-
	!.
removetrue((B,Bs),(B,Bs1)) :-
	!,
	removetrue(Bs,Bs1).
removetrue(B,B).

	
flatten_denotes(denotes(T,V),Den) :-
	T =.. [F|Xs],
	name(denotes,Dn),
	name(F,Fn),
	append(Dn,[95|Fn],DFn),
	name(DF,DFn),
	append(Xs,[V],Ys),
	Den =.. [DF|Ys].
	

joingoals1(true,Xs,Xs) :-
	!.
joingoals1(Xs,true,Xs) :-
	!.
joingoals1((true,Xs),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((Xs,true),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((X,Xs),Ys,(X,Zs)) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1(X,Xs,(X,Xs)) :-
	X =.. [F|_],
	F \== ','.

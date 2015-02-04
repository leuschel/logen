:- module(flatnames, [flat_name/2,
		flat_flatname/2,
		flat_flatnames/2,
		indexed_flatname/4,
		initial_flatquery/2]).

:-use_module(library(lists)).
:-use_module(builtins).

flat_name(A,A) :-
	sp_builtin(A),
	!.
flat_name(A,A1) :-
	nonvar(A),
	A =.. [F,As],
	nonvar(As),
	!,
	As =.. [G|Args],
	name(F,Fn),
	name(G,Gn),
	append(Gn,[95|Fn],An),
	name(H,An),
	A1 =.. [H|Args].
flat_name(A,A).

flat_flatname(ans(true),true) :-	
	!.
flat_flatname(ans(call(A)),A) :-	% calls to builtins are treated differently
	!.
flat_flatname(X,Y) :-
	flat_name(X,Z),
	flat_name(Z,Y).

flat_flatnames((B,Bs),(B1,Bs1)) :-
	!,
	flat_flatname(B,B1),
	flat_flatnames(Bs,Bs1).
flat_flatnames(B,B1) :-
	flat_flatname(B,B1).
	
indexed_flatname(Q,I,J,Qij) :-
	Q1 =.. [Q,J],
	flat_name(Q1,Qj),
	Q2 =.. [Qj,I],
	flat_name(Q2,Qij).

initial_flatquery(Q,QQ) :-
	indexed_flatname(query,0,0,Q0),
	Qinit =.. [Q0,Q],
	flat_name(Qinit,QQ).

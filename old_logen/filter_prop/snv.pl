:- module(snv, [snv/2,snvq/3]).

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(flatnames).

	
snv(Cls,[predicates(AllPs)|DomainProg]) :-
	snvTrans(Cls,DomainProg,AllPs).
	
snvq(Cls, Q, [predicates(AllPs),clause((Q2 :- true),[])|DomainProg]) :-
	snvTrans(Cls,DomainProg,AllPs),
	initial_flatquery(Q,Q2).
	
snvTrans([predicates(Ps),clause((_ :- true),[])|Cls],DomainProg,AllPs) :-
	modelize(Cls,Prog1,L,BI),
	generate_int(L,Int),
	append(Int,Prog1,Prog2),
	builtin_list(BI,BIs),
	predicateList(Int,[],Ps1),
	predicateList(BIs,Ps1,Ps2),
	append(Ps2,Ps,AllPs),
	append(BIs,Prog2,DomainProg).

modelize([clause(Cl,Ws)|Cls], [clause((H :- R),Ws)|Rs],L,BI) :-
	flatten_args(Cl,(H :- B),Ds,L),
	joingoals1(B,Ds,R),
	collect_builtins(B,BI),
	modelize(Cls,Rs,L,BI).
modelize([],[],L,BI) :-
	closedList(L),
	closedList(BI).

collect_builtins(true,_) :-
	!.
collect_builtins((B,Bs),BI) :-
	builtin(B),
	!,
	functor(B,F,N),
	addToSet(F/N,BI),
	collect_builtins(Bs,BI).
collect_builtins((_,Bs),BI) :-
	!,
	collect_builtins(Bs,BI).
collect_builtins(B,BI) :-
	builtin(B),
	!,
	functor(B,F,N),
	addToSet(F/N,BI).
collect_builtins(_,_).

addToSet(X,S) :-
	member(X,S),
	!.

flatten_args(X,Y,(Den,Ps),L) :-
	inner_clause_subterm(X,T,X1,U),
	!,
	flatten_denotes(denotes(T,U),Den),
	functor(T,F,N),
	addToSet(F/N,L),
	flatten_args(X1,Y,Ps,L).
flatten_args(X,X,true,_).


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

builtin_list(BI,Cs) :-
        findall(clause((H :- true),[]),
        	(member(F/N,BI),
        	functor(H,F,N),
        	builtin_mode(H)),
        	Cs).


removetrue((B,true),B) :-
	!.
removetrue(true,true).
removetrue((B,Bs),(B,Bs1)) :-
	removetrue(Bs,Bs1).

closedList([]) :-
	!.
closedList([_|X]) :-
	closedList(X).

generate_int(L,Int) :-
	gen_each_symb(L,Int).

gen_each_symb([F/N|L],Int) :-
	gen_ground_int(N,F/N,Int, Restint),
	gen_each_symb(L,Restint).
gen_each_symb([],[]).

gen_ground_int(0,F/N,[clause((Gfact :- true),[])|Int],Int) :-
	g_args(N,Gs),
	T =.. [F|Gs],
	flatten_denotes(denotes(T,static),Gfact).
gen_ground_int(J,F/N,[clause((NGfact1 :- true),[]),
                      clause((NGfact2 :- true),[])|Int],RestInt) :-
	J > 0,
	functor(T1,F,N),
	arg(J,T1,nonvar),
	flatten_denotes(denotes(T1,nonvar),NGfact1),
	functor(T2,F,N),
	arg(J,T2,var),
	flatten_denotes(denotes(T2,nonvar),NGfact2),
	J1 is J-1,
	gen_ground_int(J1,F/N,Int,RestInt).

g_args(0,[]).
g_args(J,[static|Gs]) :-
	J > 0,
	J1 is J-1,
	g_args(J1,Gs).
	
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

builtin_mode(static=<static).
builtin_mode(static<static).
builtin_mode(static>=static).
builtin_mode(static>static).
builtin_mode(X = X).
builtin_mode(_ == _).
builtin_mode(_ \== _).
builtin_mode(!).
builtin_mode(static =:= static).
builtin_mode(static =\= static).
builtin_mode(_ @< _).
builtin_mode(_ @=< _).
builtin_mode(_ @> _).
builtin_mode(_ @>= _).
builtin_mode(static =.. static).
builtin_mode(nonvar =.. nonvar).
builtin_mode(compound(_)).
builtin_mode(display(_)).
builtin_mode(listing).
builtin_mode(listing(_)).
builtin_mode(nl).
builtin_mode(nonvar(static)).
builtin_mode(nonvar(nonvar)).
builtin_mode(print(_)).
builtin_mode(portray_clause(_)).
builtin_mode(read(_)).
builtin_mode(repeat).
builtin_mode(true).
builtin_mode(var(_)).
builtin_mode(write(_)).
builtin_mode(writeq(_)).
builtin_mode(atom(static)).
builtin_mode(atomic(static)).
builtin_mode(atomic(nonvar)).
builtin_mode(compare(static,_,_)).
builtin_mode(float(static)).
builtin_mode(ground(static)).
builtin_mode(integer(static)).
builtin_mode(number(static)).
builtin_mode(length(_,static)).
builtin_mode(statistics(static,static)).
builtin_mode(keysort(static,static)).
builtin_mode(keysort(nonvar,nonvar)).
builtin_mode(sort(static,static)).
builtin_mode(sort(nonvar,nonvar)).
builtin_mode(tab(static)).
builtin_mode(put(static)).
builtin_mode(arg(static,static,static)).
builtin_mode(arg(static,nonvar,_)).
builtin_mode(name(static,static)).
builtin_mode(functor(nonvar,static,static)).
builtin_mode(functor(static,static,static)).

predicateList([clause((H :- true),_)|Cls],Ps0,Ps1) :-
	functor(H,P,N),
	member(P/N,Ps0),
	!,
	predicateList(Cls,Ps0,Ps1).
predicateList([clause((H :- true),_)|Cls],Ps0,Ps1) :-
	functor(H,P,N),
	!,
	predicateList(Cls,[P/N|Ps0],Ps1).
predicateList([],Ps,Ps).
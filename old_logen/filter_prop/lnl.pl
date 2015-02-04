:- module(lnl, [lnl/2, lnlq/3]).

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(flatnames).

lnl(Cls,[predicates(AllPs)|DomainProg]) :-
	lnlTrans(Cls,DomainProg,AllPs).
	
lnlq(Cls, Q, [predicates(AllPs),clause((Q2 :- true),[])|DomainProg]) :-
	lnlTrans(Cls,DomainProg,AllPs),
	initial_flatquery(Q,Q2).
	
lnlTrans([predicates(Ps),clause((_ :- true),[])|Cls],DomainProg,AllPs) :-
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

gen_each_symb(['.'/2|L],Int) :-
	!,
	gen_cons_denote(Int, Restint),
	gen_each_symb(L,Restint).
gen_each_symb([[]/0|L],Int) :-
	!,
	gen_nil_denote(Int, Restint),
	gen_each_symb(L,Restint).
gen_each_symb([F/N|L],Int) :-
	gen_nonlist_denote(F/N,Int, Restint),
	gen_each_symb(L,Restint).
gen_each_symb([],[]).

gen_nonlist_denote(F/N,[clause((Dfact :- true),[])|Int],Int) :-
	functor(T,F,N),
	flatten_denotes(denotes(T,nonlist),Dfact).

gen_cons_denote([clause((F1 :- true),[]),clause((F2 :- true),[])|Int], Int) :-
	flatten_denotes(denotes([_|list],list),F1),
	flatten_denotes(denotes([_|nonlist],nonlist),F2).
	
gen_nil_denote([clause((F1 :- true),[])|Int], Int) :-
	flatten_denotes(denotes([],list),F1).
	
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

builtin_mode(nonlist=<nonlist).
builtin_mode(nonlist<nonlist).
builtin_mode(nonlist>=nonlist).
builtin_mode(nonlist>nonlist).
builtin_mode(X = X).
builtin_mode(nonlist is nonlist).
builtin_mode(X == X).
builtin_mode(_ \== _).
builtin_mode(!).
builtin_mode(nonlist =:= nonlist).
builtin_mode(nonlist =\= nonlist).
builtin_mode(_ @< _).
builtin_mode(_ @=< _).
builtin_mode(_ @> _).
builtin_mode(_ @>= _).
builtin_mode(_ =.. list).
builtin_mode(compound(_)).
builtin_mode(display(_)).
builtin_mode(listing).
builtin_mode(listing(_)).
builtin_mode(nl).
builtin_mode(nonvar(_)).
builtin_mode(print(_)).
builtin_mode(portray_clause(_)).
builtin_mode(read(_)).
builtin_mode(repeat).
builtin_mode(true).
builtin_mode(var(_)).
builtin_mode(write(_)).
builtin_mode(writeq(_)).
builtin_mode(atom(g)).
builtin_mode(atomic(g)).
builtin_mode(compare(nonlist,_,_)).
builtin_mode(float(nonlist)).
builtin_mode(ground(nonlist)).
builtin_mode(integer(nonlist)).
builtin_mode(number(nonlist)).
builtin_mode(length(list,nonlist)).
builtin_mode(statistics(nonlist,nonlist)).
builtin_mode(keysort(list,list)).
builtin_mode(sort(list,list)).
builtin_mode(tab(nonlist)).
builtin_mode(put(nonlist)).
builtin_mode(arg(nonlist,_,_)).
builtin_mode(name(nonlist,list)).
builtin_mode(functor(_,nonlist,nonlist)).

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
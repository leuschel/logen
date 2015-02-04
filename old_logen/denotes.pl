:- module(denotes, [denotes_clause1/4, denotes_test/1]).

:- use_module(library(clpq)).
:- use_module(library(terms)).

:- dynamic append/3.
append([],B,B).
append([X|Xs], Y, [X|Zs]) :- append(Xs,Y,Zs).


:- dynamic bin_solve_atom__0/2.
bin_solve_atom__0(app([A|B],C,[A|D]), app(B,C,D)).
bin_solve_atom__0(app([A|B],C,[A|D]), E) :-
        bin_solve_atom__0(app(B,C,D), E).




denotes_test(Head) :-
	clause(Head, Body), denotes_clause1(Head, Body, DH, DB), portray_clause((DH:-DB)),fail.
denotes_test(_).


denotes_clause(Head, Body, Dhead, (Dbody,Body)) :-
	get_denotes_args(Head,Args,Dhead,NewArgs),
	do_denotesL(Args, NewArgs, Dbody).

get_denotes_args(Head,Args, Dhead, NewArgs) :-
	Head =.. [Func|Args],
	length(Args, Arity),
	length(NewArgs, Arity),
	Dhead =.. [Func|NewArgs].

denotes_clause1(H,B,DH,(B,DBS)) :-
	get_denotes_args1(H,_,DH,_,DB),
	term_variables((H,B,DH), Vars),
	
	simplify(DB, DBS,Vars).


get_denotes_args1(Head,Args,Dhead,NewArgs,Body) :-
	Head =.. [Func|Args],
	length(Args, Arity),
	length(NewArgs, Arity),
	get_denotes_args1L(Args,NewArgs, Body),
	Dhead =.. [Func|NewArgs].	

get_denotes_args1L([],[],true).
get_denotes_args1L([A], [B], DBody) :-!,
	get_denotes_args1Call(A,B,DBody).
get_denotes_args1L([A|As],[B|Bs],(DBody,Body)) :-
	get_denotes_args1Call(A,B,DBody),
	get_denotes_args1L(As,Bs,Body).

get_denotes_args1Call(A,B,DBody) :-
	(nonvar(A) ->
	    (
	      get_denotes_args(A,Aargs,B,Bargs),
	      do_denotesL(Aargs,Bargs,DBody)	    
	    )
	;
	    %% Free var but not in call position so unify and leave
	    A =B, DBody = true

	).	
	

do_denotesL([],[], true).
do_denotesL([A], [B], D) :- !,denotes(A,B,D).
do_denotesL([A|As], [B|Bs], (D,DR)) :-
	denotes(A,B,D),
	do_denotesL(As,Bs,DR).


%% Denotes domain below...
denotes(A,B, {A=B}) :- var(A), !.
denotes([], C,{C=0}) :- !.
denotes([_|T], C, ({C=B+1},R)) :-!, denotes(T,B,R).

%% unrecognised type so set to 0
denotes(_,B, {B=0}).


%simplify(C,C,_) :- !.


simplify(Cstr, Simple, VarList) :-
%	portray_clause(simplify(Cstr, Simple, VarList)),
%	term_variables(Cstr, VarList),
	call_residue((call(Cstr)
	              ,
		      dump(VarList,Vars, SimpleL),
		      VarList = Vars,
		      list_to_clp(SimpleL, Simple)
		     ),_).

list_to_clp([],true) :- !.
list_to_clp(L,{CLP}) :- list_to_clp1(L,CLP).

list_to_clp1([C], C).
list_to_clp1([C|Cs], (C,CB)) :- list_to_clp1(Cs,CB).





parseResidue([],[]).
parseResidue([[_|_]-{CALL} | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([[_|_]-(clpr:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([[_|_]-(clpq:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([UNKNOWN|TAIL], NTAIL) :-
	print('Unknown residue in clprmemo:'),print(UNKNOWN), nl,
	parseResidue(TAIL, NTAIL).


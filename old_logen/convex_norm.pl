:- module(convex_norm,[unify_pred/3,set_convex_norm/1,abstract_clause/5,convex_norm/1]).
:- use_module(library(lists)).

:- dynamic convex_norm/1.

set_convex_norm(T) :-
	retractall(convex_norm(_)),
	
	set_convex_norm1(T),
	portray_clause(user_error, using_norm(T)).

set_convex_norm1(term) :- assert(convex_norm(term)).
set_convex_norm1(list) :- assert(convex_norm(list)).
set_convex_norm1(both) :- assert(convex_norm(both)).

convex_norm(list).
%convex_norm(term).







unify_pred(C,Cnew,ResC) :-
	(convex_norm(list) ->
	    list_unify_pred(C,Cnew,ResC)
	;
	    term_unify_pred(C,Cnew,ResC)).




%%% Term Size Norm

term_unify(V,R,[V=R,V>=0]) :- var(V),!.
term_unify(F,R,[R=0]) :-
	%% anything with arity 0 is 1 or 0? I choose 0 as used in other analyser
	functor(F,_Func,0),
	!.
term_unify(F,R,[R=1+R1|C]) :-
	F =.. [_Func|Args],
	term_arg_unify(Args,R1,C).
	

term_arg_unify([A], R,C) :-
	!,
	term_unify(A,R,C).
term_arg_unify([A|T], X,[X=R+R1|Cs]) :-
	term_unify(A,R,C),
	term_arg_unify(T,R1,C1),
	append(C,C1,Cs).

	
term_unify_pred(C,Cnew,ResC) :-	
	C =.. [F|Args],	
	Cnew =.. [F|NArgs],
	term_l_unify(Args,NArgs, ResC).

term_l_unify([],[],[]).
term_l_unify([C|Cs],[N|Ns],Constr) :-
	term_unify(C,N,C1),
	term_l_unify(Cs,Ns,C2),
	append(C1,C2,Constr).	




% List Size Norm



list_unify(V,R,[V=R,V>=0]) :- var(V),!.
list_unify([],R,[R=0]) :- !.
list_unify([_|X],R,[R=SizeX+1|Cs]) :- !,list_unify(X,SizeX,Cs).
list_unify(_,X,[X=0]).



%% take pred and *copy of*pred returns Constraints in ResC mapped to Cnew vars
list_unify_pred(C,Cnew,ResC) :-	
	C =.. [F|Args],	
	Cnew =.. [F|NArgs],
	list_l_unify(Args,NArgs, ResC).

list_l_unify([],[],[]).
list_l_unify([C|Cs],[N|Ns],Constr) :-
	list_unify(C,N,C1),
	list_l_unify(Cs,Ns,C2),
	append(C1,C2,Constr).


%my_clause(app([],B,B),[]).
%my_clause(app([A|As], B,[A,Cs]),[app(As,B,Cs)]).


abstract_clause(Head,Body,AbsHead,AbsBody,Constraints) :-
	copy_term((Head,Body),(CHead,CBody)),
	functor(CHead,F,A),
	functor(AbsHead,F,A),
	unify_pred(CHead,AbsHead,HeadC),
	abstract_body(CBody,AbsBody,BodyC),

	append(HeadC,BodyC,Constraints).

abstract_body([],[],[]).
abstract_body([Call|Calls], [AbsCall|ACs],Constraints) :-
	functor(Call,F,A),
	functor(AbsCall,F,A),
	unify_pred(Call,AbsCall,BodyC),
	abstract_body(Calls,ACs,Cs),
	append(BodyC,Cs,Constraints).

%:- use_module(library(clpq)).


%clp_list([]).
%clp_list([C|Cs]) :- {C}, clp_list(Cs).

%%% simplify_clp(CLP,NewClp) :-
%%% 	call_residue(clp_list(CLP),Res),
%%% 	parseResidue(Res,NewClp).


%%% parseResidue([],[]).
%%% parseResidue([[_|_]-{CALL} | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
%%% parseResidue([[_|_]-(clpr:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
%%% parseResidue([[_|_]-(clpq:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
%%% parseResidue([UNKNOWN|TAIL], NTAIL) :-
%%% 	print('Unknown residue in clprmemo:'),print(UNKNOWN), nl,
%%% 	parseResidue(TAIL, NTAIL).





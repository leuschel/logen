:- module( simple_bta, [annotate_file/4,annotate_memo_clause/2]).
:- use_module('../gximports/safe_call').
:- use_module(builtin_db).

:- use_module(annmeta,[declmeta/3]).
:- use_module('../tools/error_manager.pl', [add_error/3,add_message/4]).

%main([PL,ANN]) :-
%	annotate_file(PL,ANN,[]).


:- dynamic safe_check/0.
:- dynamic myclause/1.

:- dynamic simple_bta_data/2.

annotate_file(Type, PlFile, AnnFile, Options) :-
    add_message(cogen_run,3,"Annotate File: ~w, ~w, ~w, ~w.~n",[Type, PlFile, AnnFile, Options]),
	retractall(simple_bta_data(_,_)),
	process_option(Options),
	open(PlFile, read, In),
	read_in_clauses(In),
	close(In),
	%OutS =  user,
	ann_clauses(Type, FilterType),
	open(AnnFile, write, OutS),
	write_clauses(OutS),
	write_filters(OutS, FilterType),
	close(OutS),
	true.

:- dynamic clause_read/1.

is_defined_user_predicate(Call) :- functor(Call,A,N), functor(C,A,N),
   (clause_read(C) ; clause_read((C :- _B))).

read_in_clauses(In) :- 
	read_term(In, Term,[]),
	(Term = end_of_file ->
	 true
	; (assert(clause_read(Term)),
	   read_in_clauses(In))
	).

ann_clauses(memo, dynamic) :- !,ann_clauses_as_memo.
ann_clauses(unfold, static) :- !,ann_clauses_as_unfold.
ann_clauses(debug, online) :- !,ann_clauses_as_unfold. /* mark as unfold/call + allow any entry */
ann_clauses(safecall, static) :- !,assert(safe_check),
	ann_clauses_as_unfold.
ann_clauses(X,_) :- add_error(simple_bta,"Unknown Simple BTA Type: ~w~n",[X]),
    fail.

ann_clauses_as_memo :-
	clause_read(Term),
	annotate_memo_clause(Term,Ann),
	(Ann == logen_ignore_this ->
		  true
	;
		  (assert(myclause(Ann)),
		   add_message(simple_bta,4, "Annotated Memo Clause: ~w~n",[Ann]))
	),
	fail.
ann_clauses_as_memo.

ann_clauses_as_unfold :-
	clause_read(Term),
	annotate_unfold_clause(Term,Ann),
	(Ann == logen_ignore_this ->
		  true
	;
		  (assert(myclause(Ann)),
		   add_message(simple_bta,4, "Annotated Unfold Clause: ~w~n",[Ann]))
	),
	fail.
ann_clauses_as_unfold.


% annotate_memo predicates are for memo/rescall all


annotate_memo_clause((:-Decl), (:-Decl)) :-
  declmeta(Decl,_,_),!.
annotate_memo_clause((:-_), logen_ignore_this) :- !.
annotate_memo_clause((H:-B), (AH:-AB)) :-
	!,
	annotate_head(H,AH),
	annotate_memo_body(B,AB).
annotate_memo_clause(H, AH) :-
	annotate_head(H,AH).


% this is used by both unfold and memo, write_filters is the bit that actually
% decides whether the args are dynamic or static
annotate_head(H,logen(ID/Arity, H)) :-
	functor(H,ID, Arity),
	(simple_bta_data(filter, ID/Arity) ->
	 true
	;  (add_message(simple_bta,2, "Added filter: ~w/~w.~n",[ID,Arity]),
	    assert(simple_bta_data(filter, ID/Arity))
	   )
	).



annotate_memo_body(A, _) :- var(A), !,
    throw('Free variable annotated directly. Use call(Var) instead.').

annotate_memo_body((A,As), (AA,AAs)) :- !,
	annotate_memo_body(A,AA),
	annotate_memo_body(As,AAs).
	
annotate_memo_body(findall(V,A,S), resfindall(V,AA,S)) :- !,
	annotate_memo_body(A,AA).
annotate_memo_body(\+(A), resnot(AA)) :- !,
	annotate_memo_body(A,AA).
annotate_memo_body((A->T;E),resif(AA,TA,ET)) :- 
	!,
	annotate_memo_body(A,AA),
	annotate_memo_body(T,TA),
	annotate_memo_body(E,ET).

annotate_memo_body((A;As), resdisj(AA,AAs)) :- !,
	annotate_memo_body(A,AA),
	annotate_memo_body(As,AAs).
	
annotate_memo_body(L,AL) :- annotate_memo_literal(L,AL).

annotate_memo_literal(L,logen(memo,L)) :- is_defined_user_predicate(L),!.
annotate_memo_literal(L,logen(rescall,L)) :- is_built_in_literal(L),!.
annotate_memo_literal(L,logen(rescall,L)). 
  /* either unknown built-in or predicate defined in other module */



% annotate_unfold predicates are for memo/rescall all
annotate_unfold_clause((:-Decl), (:-Decl)) :-
  declmeta(Decl,_,_),!.
annotate_unfold_clause((:-_), logen_ignore_this) :- !.
annotate_unfold_clause((H:-B), (AH:-AB)) :-
	!,
	annotate_head(H,AH),
	annotate_unfold_body(B,AB).
annotate_unfold_clause(H, AH) :-
	annotate_head(H,AH).

annotate_unfold_body(A, _) :- var(A), !,
    throw('Free variable annotated directly. Use call(Var) instead.').

annotate_unfold_body((A,As), (AA,AAs)) :- !,
	annotate_unfold_body(A,AA),
	annotate_unfold_body(As,AAs).
annotate_unfold_body(findall(V,A,S), findall(V,AA,S)) :- !,
	annotate_unfold_body(A,AA).
annotate_unfold_body(\+(A), not(AA)) :- !,
	annotate_unfold_body(A,AA).

annotate_unfold_body((A->T;E),if(AA,TA,ET)) :- 
	!,
	annotate_unfold_body(A,AA),
	annotate_unfold_body(T,TA),
	annotate_unfold_body(E,ET).
annotate_unfold_body((A;As), disj(AA,AAs)) :- !,
	annotate_unfold_body(A,AA),
	annotate_unfold_body(As,AAs).
annotate_unfold_body(L,AL) :- annotate_unfold_literal(L,AL).

annotate_unfold_literal(L,logen(unfold,L)) :- is_defined_user_predicate(L),!.
annotate_unfold_literal(L,logen(Ann,L)) :- is_built_in_literal(L), !,
	((\+safe_check ; white_list(L))
	  -> (never_call(L) -> Ann=rescall ; Ann = call)
	  ;  Ann = rescall).
annotate_unfold_literal(L,logen(Ann,L)) :- 
  /* either unknown built-in or predicate defined in other module */
  (safe_check -> Ann=rescall ; Ann=call).

never_call(X) :- var(X),!.
never_call(print(_)).
never_call(nl).
never_call(write(_)).
never_call(write(_,_)).
never_call(write_term(_)).
never_call(write_term(_,_)).
never_call(print(_,_)).
never_call(nl(_)).
never_call(format(_,_,_)).
never_call(format(_,_)).
never_call(tell(_)).
never_call(told).
never_call(see(_)).
never_call(seen).
never_call(read(_)).
never_call(open(_,_,_)).
never_call(close(_)).
never_call(call(X)) :- never_call(X).

write_filters(OutS, Filter) :-
	simple_bta_data(filter, ID/Arity),
	make_filter_list(Filter, Arity, Args),
	Fil =.. [ID|Args],
	portray_clause(OutS, (:-filter(Fil))),
	fail.
write_filters(_, _).

make_filter_list(_, 0,[]).
make_filter_list(Filter, X,[Filter|R]) :-
	X > 0,
	X1 is X - 1,
	make_filter_list(Filter, X1, R).



write_clauses(OutS) :-
	myclause(Term),
	portray_clause(OutS,Term),
	fail.
write_clauses(_).

process_option(_).


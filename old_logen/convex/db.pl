%% ---------------------------------------------
%% db.pl - database module - conditional assert 
%% ---------------------------------------------

:- module(db,                 [cond_assert_size/1, fact/1, get_prev_fact/2]).

:- use_module(constraints,    [more_general_constraints/2,
                               equiv_constraints/2]).

:- use_module(library(terms), [variant/2]).
:- use_module(library(lists), [member/2, delete/3]).


:- dynamic fact/1.


%% -------------------------------------------------------
%% get_prev_fact:
%%
get_prev_fact(Head,(HeadCopy :- PrevCs)) :-
    copy_term(Head,HeadCopy),
    fact((HeadCopy :- PrevCs)).



%% --------------------------------------------------------------------
%% delete_variants(L1,L2,L3) : L3 contains all the elements
%% from L1 that are not variants of elements in L2

delete_variants(List,[],List).
delete_variants(List, [Elt|Elts], Rest) :-
    member(Elt, List),!,
    delete(List, Elt, Rest1),
    delete_variants(Rest1, Elts, Rest).
delete_variants(List, [_|Elts], Rest) :-
    delete_variants(List, Elts, Rest).


in_database_size(fact((F:-D))) :- !,
	functor(F,N,A), functor(F1,N,A),
	fact((F1:-D1)),
	variant((F,D),(F1,D1)).


%% --------------------------------------------------------------------
%% cond_assert_size(+F) : conditional assertion of a
%% size syntactic object to db.

cond_assert_size(F) :-
	in_database_size(F),!.

cond_assert_size(F) :- !,
    findall(Fact1, compare_size(F, Fact1, more), More),
    findall(Fact2, compare_size(F, Fact2, less), Less),
    delete_variants(Less, More, StrictLess),
    retract_list(StrictLess),
    (More == [] -> assert(F), assert(user:flag); true),!.

compare_size(fact((P:-Dep1)), fact((P:-Dep2)),M) :-
    fact((P:-Dep2)), compare_constraints(Dep2,Dep1,M).

compare_constraints(C1,C2,more) :- more_general_constraints(C1,C2).
compare_constraints(C1,C2,less) :- more_general_constraints(C2,C1).
compare_constraints(C1,C2,same) :- equiv_constraints(C1,C2).


retract_list([]).
retract_list([F|Fs]) :- safe_retract(F), retract_list(Fs).

safe_retract(F) :-
    numbervars(F,0,_),
    retract(F).

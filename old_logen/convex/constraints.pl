%% --------------------------------------------------------------------
%% constraints.pl - general clpr operations
%% --------------------------------------------------------------------

:- module(constraints, [non_neg/2, consistent_constraints/1,
    list2constraints/2, more_general_constraints/2,
    equiv_constraints/2, argument_equations/3]).

:- use_module(library(lists)).
%:- use_module(library(clpr)).
:- use_module(library(clpq)).
:- use_module(library(terms), [term_variables/2]).

%% --------------------------------------------------------------------
%% non_neg(+Clist, -AllList) : Add non negative constraints to Clist
%% and return the whole list AllList.

non_neg(Clist, AllList) :-
    term_variables(Clist, Vars),
    vars_non_neg(Vars,Clist,AllList).

%% --------------------------------------------------------------------
%% consistent_constraints(+L) : Check consistency of list of
%% constraints L.

consistent_constraints([]) :- !.
consistent_constraints(ConList) :-
    non_neg(ConList, AllList),
    list2constraints(AllList, Constraints),
    \+ \+ Constraints.

%% --------------------------------------------------------------------
%% list2constraints(+L,-C) : convert a list L to conjunction of
%% constraints C.

list2constraints([], true) :- !.
list2constraints(List, Constraints) :-
    Constraints =.. [{}, Conj], list2conj(List,Conj).

list2conj([A], (A)) :- !.
list2conj([A|Bs],(A,NBs)) :- !,list2conj(Bs, NBs).

%% --------------------------------------------------------------------
%% more_general_constraints(+L1,+L2), equiv_constraints(+L1,+L2)
%% L1 is more genral/equivalent to L2

more_general_constraints(L1,L2) :- constraints_entailment(L2,L1).

equiv_constraints(L1,L2) :-
    constraints_entailment(L2,L1),
    constraints_entailment(L1,L2).

%% --------------------------------------------------------------------
%% argument_equations(+Vs,+Ts,-Res) : Res is a list which constains
%% equalities between corresponding elements in Vs and Ts.

argument_equations([], [], []).
argument_equations([A|As], [NA|NAs], [A = NA|ArgEqs]) :-
    argument_equations(As, NAs, ArgEqs).

%% --------------------------------------------------------------------
%% vars_non_neg(+V,+Tail,-D) : For each variable X in V, D constains
%% a constraint of the form X>=0, appended to Tail.

%% vars_non_neg([],T,T).
%% vars_non_neg([V|Vs],T,[V>=0|Cons]) :- vars_non_neg(Vs,T,Cons).

vars_non_neg(Vars,Cons,All) :-
    vars_non_neg1(Vars,Cons1), append(Cons,Cons1,All).
vars_non_neg1([],[]).
vars_non_neg1([V|Vs],[V>=0|NGVs]) :- vars_non_neg1(Vs,NGVs).

%% --------------------------------------------------------------------
%% constraints_entailment(+L1,+L2) : List of constraints L1 entail a
%% list of constraints L2.

constraints_entailment(_Clist1, []) :- !.
constraints_entailment([], _Clist2) :- !, fail.
constraints_entailment(Clist1, Clist2) :-
    non_neg(Clist1, AllList),
    term_variables(Clist2, Vars2),
    vars_non_neg(Vars2, AllList, Clist),
    list2constraints(Clist, Constraints),
    \+ \+ (Constraints, entailed_list(Clist2)).

entailed_list([]).
entailed_list([C|Cs]) :- entailed(C), entailed_list(Cs).

%% --------------------------------------------------------------------

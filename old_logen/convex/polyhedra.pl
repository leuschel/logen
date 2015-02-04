%% --------------------------------------------------------------------
%% polyhedra.pl - linear inter-argument relations with convex-hulls
%% --------------------------------------------------------------------
        
:- module(polyhedra, [projection/2, 
                      hull/3,
                      widen/3]).

:- use_module(constraints,[consistent_constraints/1,
    argument_equations/3, list2constraints/2, non_neg/2]).

%:- use_module(library(clpr)).
:- use_module(library(clpq)).
:- use_module(library(lists), [append/3]).



%% --------------------------------------------------------------------
%% projection( (+Atom :- +Constraints), (-NAtom :- -NConstraints)) 
%%  : (-NAtom :- -NConstraints) is a clean copy of the projection of   Constraints
%%    on the args of  Atom.
%%    Size dependecies are given in a form of poyhedra.

projection((Atom:- Constraints),(NAtom :- NConstraints)) :-
    consistent_constraints(Constraints),
    copy_term(Atom,NAtom), Atom =.. [_|Args], NAtom =.. [_|NArgs],
    argument_equations(Args,NArgs, ArgEqList),
    append(Constraints, ArgEqList, NewConstraints),
    non_neg(NewConstraints,All),
    project(NArgs, All, NConstraints).

%% --------------------------------------------------------------------
%% hull(+Ys1,+C1,+Ys2,+C2,-Xs,-Hull) : compute the convex hull of 2
%% polyhedra given in Ys1,Ys2 (list of variables) C1,C2 (polyhedra).
%% The result is the polyhedra in Hull, using the variables in Xs.

hull((Atom1:-C1),(Atom2:-C2),(Atom3:-Hull)) :-
    hull_normalize_list(C1, S1, Lin1),
    hull_normalize_list(C2, S2, Lin2),
    sum_atoms(Atom1,Atom2,Atom3,Sums),
    append(Lin1,Lin2,Lin),
    append(Sums,[S1 + S2 = 1|Lin],All),
    non_neg(All,All1),
    Atom3 =.. [_|Xs],
    project(Xs,All1,Hull).

%% --------------------------------------------------------------------
%% project(+Vars, +Clist, -Res) : Res is the result of the projection
%% of the constraints in Clist on Vars.    

project([],_,[]) :- !.
project(Vars, Clist, Result) :-
    copy_term((Vars,Clist),(Vars1,Clist1)),
    split_cons(Clist1,Equalities,Inequalities),
    list2constraints(Equalities,C1),
    list2constraints(Inequalities,C2),
    C1,C2,
    unify_eq_vars(Vars1,Vars,Eqs,Rest1,Rest),
    dump(Rest1,Rest,NonEqs),    
    append(Eqs,NonEqs,Result).

%% I split the constraints to put the equations first because adding
%% the equations to the constraint store before the inequalities
%% helps the sicstus solver in getting a better result.

split_cons([],[],[]).
split_cons([A=B|Rest],[A=B|Eqs],NEqs) :- split_cons(Rest,Eqs,NEqs).
split_cons([A=<B|Rest],Eqs,[A=<B|NEqs]) :- split_cons(Rest,Eqs,NEqs).
split_cons([A>=B|Rest],Eqs,[A>=B|NEqs]) :- split_cons(Rest,Eqs,NEqs).

%%
%%

widen((H1 :- Cs1), (H2 :- Cs2), (H3 :- Cs3)) :-
   H2 = H1,
   polyhedra_widen(H1,Cs2,Cs1,H3,Cs3).


%% --------------------------------------------------------------------
%% polyhedra_widen(+Atom,+Clist1,+Clist2,-WidenAtom,-ClistWiden) : Clist1,
%% Clist2 are 2 lists of cosntraints on the args of Atom. WidenAtom is a fresh
%% copy of Atom and ClistWiden is the result of selecting the constraints
%% from Clist1 which are entailed by Clist2.

polyhedra_widen(Atom,_ ,[],Atom,[]) :- !.
polyhedra_widen(Atom,[],_ ,Atom,[]) :- !.
polyhedra_widen(Atom,Cons1,Cons2,NAtom,ClistWiden) :-
    copy_term((Atom,Cons1,Cons2),(Atom1,Clist1,Clist2)),
    copy_term((Atom1,Clist1),(NAtom,ClistCopy)),
    list2constraints(Clist2,Constraints),
    Constraints,
    entailed_constraints(Clist1,ClistCopy,ClistWiden).

entailed_constraints([],[],[]).
entailed_constraints([C|Cs],[CC|CCs],[CC|En]) :-
    entailed(C),!, entailed_constraints(Cs,CCs,En).
entailed_constraints([_|Cs],[_|CCs],En) :-
    entailed_constraints(Cs,CCs,En).

%% --------------------------------------------------------------------
%% sum_atoms(+A1,+A2,-A3,-L) : arguments of A3 are of the form 
%% X = Y1+Y2 for corresponding arguments of A1 and A2.

sum_atoms(Atom1,Atom2,Atom3,Sums) :- 
    Atom1=..[P|Ys1],Atom2=..[P|Ys2],
    sum_args(Ys1,Ys2,Xs,Sums),
    Atom3=..[P|Xs].

sum_args([],[],[],[]).
sum_args([Y1|Ys1],[Y2|Ys2],[X|Xs], [X = Y1+Y2|Ls]) :- 
    sum_args(Ys1,Ys2,Xs,Ls).

%% --------------------------------------------------------------------
%% hull_normalize_list(+Cs,+Sigma,-Ls) : convert each constraint on
%% the list Cs to normal form : the inequalities are =< the variables
%% are in lhs and the numbers in rhs are multiplied by Sigma.

hull_normalize_list([],_,[]).
hull_normalize_list([C|Cs], Sigma,[Lin|Ls]) :-
    hull_normalize(C,Sigma,Lin),!,
    hull_normalize_list(Cs,Sigma,Ls).

%% --------------------------------------------------------------------
%% hull_normalize(+Constraint,+Sigma,-Lin) : convert each constraint of
%% the form E1 eq E2, where eq is any comparison, to a constraint where
%% all the variables are moved to the left hand side, all the numbers
%% are moved to the right hand side and multiplied by Sigma.
%% Inequalities of the form >= are converted to =<.

hull_normalize(E1 =  E2,Sigma,Lhs =  Rhs) :- !,
    normalize_exp(E1,E2,Sigma,Lhs,Rhs).
hull_normalize(E1 =< E2,Sigma,Lhs =< Rhs) :- !,
    normalize_exp(E1,E2,Sigma,Lhs,Rhs).
hull_normalize(E1 >= E2,Sigma,Lhs =< Rhs) :- !,
    normalize_exp(E2,E1,Sigma,Lhs,Rhs).
hull_normalize(E1 < E2,Sigma,Lhs < Rhs) :- !,
    normalize_exp(E1,E2,Sigma,Lhs,Rhs).
hull_normalize(E1 > E2,Sigma,Lhs < Rhs) :- !,
    normalize_exp(E2,E1,Sigma,Lhs,Rhs).

normalize_exp(E1,E2,Sigma,Lhs,Rhs) :-
    parse_exp(E1,Vs1,Ns1), parse_exp(E2,Vs2,Ns2),
    Ns is Ns2 - Ns1,
    get_lhs(Vs1,Vs2,Lhs), get_rhs(Ns,Sigma,Rhs).


get_lhs(Vs1,Vs2,-Vs2) :- ground(Vs1),!.
get_lhs(Vs1,Vs2,Vs1) :-  ground(Vs2),!.
get_lhs(Vs1,Vs2,Vs1-Vs2).

get_rhs(Ns,_Sigma,0) :- Ns =:= 0,!.
get_rhs(Ns,Sigma,Sigma*Ns).

%% --------------------------------------------------------------------
%% parse_exp(+Exp,-Vs,-N) : Vs contains all the components of Exp which
%% contain variables and N is the sum of all components that contain
%% only numbers.

my_number(X,X) :- !,number(X).

my_number(rat(X,1),X) :- !.
my_number(rat(X,Y),Z) :- Z is X/Y,
	print_message(error,'ERROR! line 161 polyhedra: non integer rational number'),
	print_message(error,rat(X,Y)).
my_number(X,X) :- number(X).


parse_exp(V,V,0) :- var(V),!.
%parse_exp(N,0,N) :- number(N),!.
parse_exp(N,0,N1) :- my_number(N,N1),!.
%parse_exp(*(N,V),*(N,V),0) :- number(N),var(V),!.
%parse_exp(*(V,N),*(V,N),0) :- number(N),var(V),!.
parse_exp(*(N,V),*(N,V),0) :- my_number(N,_),var(V),!.
parse_exp(*(V,N),*(V,N),0) :- my_number(N,_),var(V),!.
parse_exp(E1+E2,Vs1+Vs2,Ns) :-
    parse_exp(E1,Vs1,Ns1),
    parse_exp(E2,Vs2,Ns2),
    Ns is Ns1+Ns2.
parse_exp(E1-E2,Vs1-Vs2,Ns) :-
    parse_exp(E1,Vs1,Ns1),
    parse_exp(E2,Vs2,Ns2),
    Ns is Ns1-Ns2.
parse_exp(-E1,-Vs1,Ns) :-
    parse_exp(E1,Vs1,Ns1),
    Ns is -Ns1.

%% --------------------------------------------------------------------
%% unify_eq_vars(+Xs,+Ys,-Eqs,-RestX,-RestY) : check which variables
%% in Xs are ground or aliased to other variables and create the
%% appropriate equations (for the Ys) in Eqs, the inequalities are
%% returned in RestX and RestY resp.

unify_eq_vars([],[],[],[],[]).
unify_eq_vars([X|Xs],[Y|Ys],[X=Y|Eqs],RestX,RestY) :-
    ground(X),!,
    unify_eq_vars(Xs,Ys,Eqs,RestX,RestY).
unify_eq_vars([X|Xs],[Y|Ys],Eqs,[X|RestX],[Y|RestY]) :-
    eq_var(X,Y,Xs,Ys,Eqs1,RX,RY),
    unify_eq_vars(RX,RY,Eqs2,RestX,RestY),
    append(Eqs1,Eqs2,Eqs).

eq_var(_,_,[],[],[],[],[]).
eq_var(X1,Y1,[X2|Xs],[Y2|Ys],[Y1=Y2|REqs],RXs,RYs) :-
    X1 == X2,!,
    eq_var(X1,Y1,Xs,Ys,REqs,RXs,RYs).
eq_var(X1,Y1,[X2|Xs],[Y2|Ys],Eqs,[X2|RXs],[Y2|RYs]) :-
    eq_var(X1,Y1,Xs,Ys,Eqs,RXs,RYs).

%% --------------------------------------------------------------------

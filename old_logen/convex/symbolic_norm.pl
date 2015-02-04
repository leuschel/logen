%% --------------------------------------------------------------------
%% symbolic_norm.pl - convert terms to term size expressions
%%
%% --------------------------------------------------------------------

:- module(symbolic_norm, [term_size_exp/2,list_size_exp/2]).

:- use_module(library(lists)).
:- use_module(library(terms)).


%% --------------------------------------------------------------------
%% term_size_exp(+Term,-Norm)

term_size_exp(Term,Norm) :-
    termsize(Term,N,V,MinV),
    varset_diff(V,MinV,PlusVars,MinVars),
    varlist2varsum(PlusVars, VarSum),
    varlist2varsum(MinVars, MinVarSum),
    sum(N,VarSum,Norm1),
    diff(Norm1,MinVarSum, Norm).

sum(0,X,X) :- !.
sum(X,Y,X) :- Y == 0,!.
sum(X,Y,X+Y).

diff(N,X,0) :- N == 0,X==0,!.
diff(N,X,-X) :- N == 0,!.
diff(N,X,N) :- X == 0,!.
diff(X,Y,X-Y).

varset_diff(Ps,[],Ps,[]).
varset_diff(Ps,[M|Ms],Pl,Mi) :-
    var_member(M,Ps,RP),!,
    varset_diff(RP,Ms,Pl,Mi).

varset_diff(Ps,[M|Ms],Pl,[M|Mi]) :-
    varset_diff(Ps,Ms,Pl,Mi).

var_member(Var,[V|Vs],Vs) :-
    Var == V.

var_member(Var,[V|Vs],[V|R]) :-
    var_member(Var,Vs,R).        

%% --------------------------------------------------------------------
%% varlist2varsum(A,B) : convert list of variables A, to sum of
%% variables B.

varlist2varsum([],0).
varlist2varsum([V],V) :- !.
varlist2varsum([V|Vs], V+SVs) :- varlist2varsum(Vs,SVs).

%% --------------------------------------------------------------------
%% termsize(+Term,-Number,-Vars) return the term size symbolic norm in
%% 2 parts : the constant Number and the list of variables Vars.

termsize(Term,0,[Term],[]) :- var(Term),!.    
termsize(Term,N,Vars, MinVars) :-
    functor(Term,_,Arity),
    Term =.. [_|TermArgs],
    term_args_norm(TermArgs, NormNumber, Vars, MinVars),
    N is Arity + NormNumber.

%% --------------------------------------------------------------------
%% term_args_norm(+Args,-Number,-Varlist,-MinVarList) : Number is the integer
%% part of the symbolic norm of Args, and Varlist is the list of variables.

term_args_norm([],0,[],[]).
term_args_norm([Arg], Number, Varlist, MinVarlist) :-
    !, termsize(Arg, Number, Varlist, MinVarlist).
term_args_norm([Arg|Args], Number, Varlist,MinVarlist) :-
    termsize(Arg, N1, Vlist1, MinVlist1),
    term_args_norm(Args, N2, Vlist2, MinVlist2),
    Number is N1 + N2,
    append(Vlist1, Vlist2, Varlist),
    append(MinVlist1, MinVlist2, MinVarlist).


list_size_exp(Term,NormA) :-
	denotesL(Term,NormA).


denotesL(A,A) :- var(A), !.
denotesL([],0) :- !.
denotesL([_|T],1+B ) :-!, denotesL(T,B).

denotesL(_,0).

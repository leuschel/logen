%:- module(analyse, [go/1]).

:- use_module(input,[load_file/1, my_clause/2,remember_clause/1]).
:- use_module(symbolic_norm,[term_size_exp/2,list_size_exp/2]).
:- use_module(db,[ get_prev_fact/2,
                   cond_assert_size/1,
                   fact/1 ]).

:- use_module(polyhedra,       [ projection/2,
	                         widen/3,
			         hull/3]).

:- use_module(library(lists),[append/3]).

:- dynamic convex_norm/1.

%convex_norm(list) :-!.
convex_norm(term) :-!.


:- dynamic flag/0.
%% interface
go(File) :-
      retractall(fact((_:-_))),
      load_file(File),
      iterate(1),
      showfacts.




reset_analysis :-
	retractall(fact((_:_))).
%% control (count iterations so as to know when to widen)
iterate(I) :- operator(I), fail.
iterate(I) :- flag, retractall(flag), I1 is I+1,  iterate(I1).
iterate(_).

%% logic
operator(I) :-
    my_clause(Head,Body),
    prove(Body,[],Cs),
    projection((Head :- Cs),(ProjHead :- ProjCs)),
    ( % "is there a previous relevant fact already derived?"
     get_prev_fact( Head, (PrevHead :- PrevCs) ) -> 
        ( % "to widen? or just to lub?"
	  I mod 3 =:= 0 ->
	    lub((PrevHead :- PrevCs), (ProjHead :- ProjCs), (NewHead1 :- NewCs1)),
            widen((NewHead1 :- NewCs1), (PrevHead :- PrevCs), (NewHead :- NewCs))
        ;
	    lub((PrevHead :- PrevCs), (ProjHead :- ProjCs), (NewHead :- NewCs))
        )
    ; 
        NewHead=ProjHead, NewCs=ProjCs
    ),
    cond_assert_size(fact((NewHead :- NewCs))).



prove([B|Bs],ConstraintsIn,ConstraintsOut) :-
	fact((B :- ConstraintsB)),
	meet(ConstraintsIn,ConstraintsB,ConstraintsNew),
	prove(Bs,ConstraintsNew,ConstraintsOut).

prove([unify(A,B)|Bs],ConstraintsIn,ConstraintsOut) :-
	unify(A,B,ConstraintAB),
	meet(ConstraintsIn,ConstraintAB,ConstraintsNew),
	prove(Bs,ConstraintsNew,ConstraintsOut).

prove([],Constraints,Constraints).
 

%%%
%%% The MEET
%%%
meet(Constraints1,Constraints2,Meet) :-
	append(Constraints1,Constraints2,Meet).


%%%
%%% Abstract Unification
%%% (abstract the Term and take the new constraint)

unify(Var, Term, [Var = Expr]) :-
%	term_size_exp(Term,Expr).
	(convex_norm(term) ->
	    term_size_exp(Term,Expr)
	;	    
	    list_size_exp(Term,Expr)
	).

%%%
%%% The LUB of A and B is C
%%% (call the convex hull package)
 
lub(A,B,C) :- 
    hull(A,B,C).

showfacts :-
    format(user_error,"facts:~n",[]),
    fact(F),
%    numbervars(F,0,_),
%    print(F), nl,
    format(user_error, "~@",[portray_clause(F)]),
    fail ; true.



:- module('flatten_pp', [flatten_clauses/2, reset_flattenpp/0, flat_clause/2]).

:- dynamic flat_clause/2.

reset_flattenpp :-
	retractall(flat_clause(_,_)).

flatten_clauses(_Module, []).
flatten_clauses(Module, [C|Cs]) :-
	flatten_clause(Module,C),
	flatten_clauses(Module,Cs).



flatten_clause(Module, C) :-
	use_module(Module),
	functor(C, _F,2),
	arg(1,C,Head),
	arg(2,C,Body),
	call(C),
	flatten(Body,FlatBody),
	assert(flat_clause(Head,FlatBody)),
	fail.
flatten_clause(_,_).


	
flatten(X,Res) :- var(X),!,Res=X.
flatten((X,Y),Res) :- !,
	flatten(X,FX),
	((FX==fail) -> (Res=fail) 
	 ;
	 (flatten(Y,FY),
	  ((FX==true) -> (Res=FY); ((FY==true) -> (Res = FX) ; (Res = (FX,FY))))
	 )).
flatten((I->T;E), Res) :- !,
  flatten(I,FI),
  ((FI==fail) -> flatten(E,Res) ;
            ((FI==true) -> flatten(T,Res) ;
                (Res = (FI->FT;FE), flatten(T,FT), flatten(E,FE))
  )).
flatten(X,X).



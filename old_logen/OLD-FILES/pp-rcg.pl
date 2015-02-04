/* file: pp-rcg.pl */
:- use_module('cogen-ml').

rcg_call(Call) :-
	cogen:add_extra_argument("_m",Call,SpecCall,MemoCall),
	MemoCall,
	SpecCall.


/* Pretty Printer */


reset_pp.

flush_pp.



pp([]).
pp([Clause|Rest]) :-
	pp_clause(Clause),
	pp(Rest).


pp_clause(clause(Head,Body)) :-
	flatten(Body,Flatbody),
	assert((Head :- Flatbody)).


/* pp_consults(FileList)  prints a list of consult statements */
pp_consults([]).
pp_consults([File|Rest]) :-
	consult(File),
	pp_consults(Rest).



flatten(X,Res) :- var(X),!,Res=X.
flatten((X,Y),Res) :- !,
	flatten(X,FX),flatten(Y,FY),
	((FX==true) -> (Res=FY); ((FY==true) -> (Res = FX) ; (Res = (FX,FY)))).
flatten(X,X).

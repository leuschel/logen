%%
%% CLP module for clp helper functions.  

:- module(clp, [
		parseResidueComma/2,
	        parseResidue/2,
 	       is_clp_var/1,    %% checks if a variable is CLP
 	        copy/3,          %% like copy_term but for clp, see below
		listToComm/2
	       
	       ]
	 ).

:- use_package(.(sicsignore)).
:- use_package(assertions).


%%clp rational module
sicstus((:- use_module(library(clpq)))).
:- use_package(library(clpq)).
:- use_module(library(terms)).







% Is the variable flagged for the clp rational domain? (unsupported function call...)
is_clp_var(X) :-
	var(X),
	clpq:get_atts(X,L),
	L \= [],!.
% Must also check if it is involved in non linear constraints.
is_clp_var(X):-
	is_non_linear_clp_var(X).

is_non_linear_clp_var(X) :-
	dump([X],[_],[_]).

parseResidueComma(Residue, Constraints) :-
	parseResidue(Residue, ConstList),
	(ConstList == [] -> Constraints = true; listToComm(ConstList, CSTR), Constraints = {CSTR}).

%% Parse the call_residue residue into a list of constraints
parseResidue([],[]).
parseResidue([[_|_]-{CALL} | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([[_|_]-(clpr:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([[_|_]-(clpq:{CALL}) | TAIL], [CALL| NTAIL]) :- !,parseResidue(TAIL, NTAIL).
parseResidue([UNKNOWN|TAIL], NTAIL) :-
	print('Unknown residue in clprmemo:'),print(UNKNOWN), nl,
	parseResidue(TAIL, NTAIL).


%%% this is due to no CLP support in CIAO
ciao(copy(Call,CopyCall,_)) :- copy_term(Call, CopyCall).

:- discontiguous sicstus/1.

sicstus(copy(Call,CopyCall,Constraints)) :- clpq:copy_term(Call,CopyCall,Constraints).





listToComm([A],A).
listToComm([A|Tail], AND) :- AND =.. [',', A, NewAnd], listToComm(Tail, NewAnd).


:- comment(version_maintenance,off).


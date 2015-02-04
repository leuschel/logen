:- module(clprmemo,	  [
	   delete_table_clp/0,		   
	   insert_clp_pattern/2,
	   insert_clp_pattern_with_filter_types/3,
	   find_clp_pattern/2,
	   printCLP/1,
	   constraintTest/2,
	   resetAll/0,
	   parseResidueComma/2,
%	   simplifyBody/2,
	   simplifyResidue/2
			  
	   ]).

:- use_package(.(sicsignore)).
%% Module Imports

ciao(parseResidueComma(X,Y)) :- 
	clp:parseResidueComma(X,Y).

%:- use_package(clpr).           %% CLP LIBS For Ciao

%%System
sicstus((:- use_module(library(clpq)))).
%:- use_package(library(clpq)).

:- use_module(library(terms)).  %% FOR 
:- use_module(library(lists)).  %% For append

%% Logen
:- use_module('pp.pl').
:- use_module('clp.pl').
:- use_module('memo.pl').
:- use_module('annfile.pl').    %% Access to annotation file predicates
:- use_module('cogen-tools.pl').




%%Print module still doesnt print CLP formulae directly so quick workaround
printCLP(CODE) :- local_debug(CODE), copy(CODE, COPY, _), pp:direct_pp(COPY).	     

local_debug(_CODE) :-true.
%local_debug(CODE) :- print('Attempting to Print:::'), print(CODE).


%delete_table_clp :- retractall(memo_table(_,_,_,_)).
delete_table_clp :- retractall(memo_table(_,_,_,_,_)).


%% Managing the memo table		
%:- dynamic memo_table/4.
%memo_table(ID,Call,Cstr,Res) :- memo_table(ID,Call,Cstr,Res,_).
:- dynamic memo_table/5.

:- initialization(delete_table_clp).
%:- delete_table_clp.

%insert_pattern(Call, ResidualCall) :-
insert_clp_pattern_with_filter_types(Call, FilterTypes, ResidualCall) :-
	filter_atom_with_filter_types(Call, FilterTypes, ResidualCall, ID),
	copy((Call,ResidualCall), (CopyCall,CopyResidualCall), Constraints),
	Out = memo_table(ID, CopyCall, Constraints,CopyResidualCall,internal),
	assert(Out).


insert_clp_pattern(Call, ResidualCall) :-
	%filter atom and generate 
%	filter_atom(Call, ResidualCall, ID),
	memo:filter_atom(Call,ResidualCall, ID),
%	copy(Call, CopyCall, Constraints,Env),
%	copy(ResidualCall, CopyResidualCall, _,Env),
	copy((Call,ResidualCall), (CopyCall,CopyResidualCall), Constraints),
	Out = memo_table(ID, CopyCall, Constraints,CopyResidualCall,internal),
	assert(Out).

find_clp_pattern(CallPattern, ResidualCall) :-
	%need to match pattern that CallPattern is an instance of
	copy(CallPattern, Copy, _),
	numbervars(Copy, 0 , _),
	memo_table(ID, Copy, _, _,_),
	memo_table(ID, CallPattern,Constraints, ResidualCall,_),
	checkEntailed(Constraints).


% resetAll :- resetNum, delete_table_clp.


%%% Simplify the Residual, checking for entailment
%%% should return comma seperated constraints
simplifyResidue(Residual, Constraints) :-
	parseResidue(Residual, CSTR),
	removeRedundantCstr(CSTR,NewCSTR),
	(NewCSTR == [] ->
	    Constraints = true
	;
	    listToComm(NewCSTR, CommaCSTR), Constraints = {CommaCSTR}
	).

removeRedundantCstr([],[]).
removeRedundantCstr([Cstr|Tail], New) :-
	(entailed(Cstr) ->
	    New = NTail
	;
	    New = [Cstr|NTail]
	),
	removeRedundantCstr(Tail,NTail).
	



%% Entailment checking for find pattern
checkEntailed([]).
checkEntailed(Constraints) :- Constraints \== [], listToComm(Constraints, AndList), call(entailed(AndList)).

%constraintTest(true, Call) :- call(Call).

constraintTest(true,Call) :-
	listToComm(Call, And),
	call({And}).


%%%% dead code, will remove soon, When I am sure it is not needed STEVE
%%%%% Entailment checking for calculating the residual constraints for unfolding
%%%%constraintTest(Resid, Constraints) :-
%%%%	cTest(R, Constraints),
%%%%	(R == [] ->
%%%%	    Resid = true;
%%%%	    listToComm(R, AND),
%%%%	    Resid =.. ['{}',AND]
%%%%	).

%%%cTest([],[]).
%%%cTest(Resid, [Constraint|CTail]) :-
%%%	(entailed(Constraint) ->
%%%	    Resid = Tail;
%%%	    % if the constraint is not entailed by the current clp state
%%%	    % then we want to save it in the residual and call it before continuing
%%%	    %Added by steve to try and add a bit of simplification...
%%%%%%	    Resid = [Constraint|Tail], call({Constraint})
%%%	    simplify(Constraint, Simpler),
%%%	    append(Simpler, Tail, Resid),
%%%	    call({Constraint}) % of course here it would be better to call simpler but its wrong format....
	
	
%%%	), cTest(Tail, CTail).


%%%%%% This is very sketchy and MUST be changed, will flatten if statements too far...
%%%flattenC(X, R) :-
%%%	X =.. [',',A,B],!,
%%%	flattenC(A,R1),
%%%	flattenC(B,R2),
%%%	append(R1,R2,R).
%%%flattenC(true,[]) :- !.
%%%flattenC(X,[X]) :- !.

%%%simplifyList([{C1},{C2}|Tail], New) :-!, simplifyList([{C1,C2}|Tail], New).
%%%simplifyList([{C1}|Tail], New) :- !,
%%%	simplifyComm(C1, SIMPLE),
%%%	(SIMPLE == true -> New = NTail ; New = [SIMPLE|NTail]),
%%%	simplifyList(Tail, NTail).	
%%%simplifyList(X,X) :- !.





%%%simplifyBody(Body, SimpleBody) :- flattenC(Body, New), !,simplifyList(New, SimpleList), listToComm(SimpleList, SimpleBody),!.

%%%simplifyComm(CLPR,SIMPLE) :- 
%%%	simplify(CLPR, SIMPLELIST),
%%%	(SIMPLELIST == [] -> SIMPLE = true ; listToComm(SIMPLELIST, SIMPLE1), SIMPLE = {SIMPLE1}).
	     
%%%% testing, to see if simplify is causing ordering errors
%%%%simplify(C,C).
%%%simplify(CLPR, SIMPLE) :-
%%%	call_residue(call({CLPR}), SIMPLE1),
%%%	parseResidue(SIMPLE1,SIMPLE).











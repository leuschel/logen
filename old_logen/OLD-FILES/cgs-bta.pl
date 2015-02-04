:- module(cgsbta,[perform_bta/3,
		  get_status_clause/2,
		  get_status_constructor/2]).
:- use_module(library(system)).
:- use_module('cgs-tools').
:- environ(bta_directory,DirBTA),
	string_concatenate(DirBTA,'bta',BTA_File),
	use_module(BTA_File).

:- dynamic db_status_constructor/1.
:- dynamic db_status_clause/1.

%% --- perform the BTA analysis from wim
perform_bta(File,Norm,Query) :-
	retractall(db_status_clause(_)),
	retractall(db_status_constructor(_)),
	bta:bta_interface(File,Norm,Query,ResStatusCons,ResStatusClause),
	assert(db_status_clause(ResStatusClause)),
	assert(db_status_constructor(ResStatusCons)).

%% --- retrieve a status clause by given its number (or position)
get_status_clause(NumClause,[(NumClause,StatusClause)|Tail],StatusClause) :- !.
get_status_clause(NumClause,[_AStatus|OtherStatus],Result) :- !,
	get_status_clause(NumClause,OtherStatus,Result).

get_status_clause(NumClause,StatusClause) :-
	db_status_clause(Status),
	get_status_clause(NumClause,Status,Result).

%% --- get the status of clause constructor
get_status_constructor(Functor,Arity,[(Functor,Arity,Status)|_Tails],Status) :- !.
get_status_constructor(Functor,Arity,[_|Tail],Status) :- !,
	get_status_constructor(Functor,Arity,Tail,Status).


get_status_constructor(Clause,Status) :-
	db_status_constructor(ClauseStatusList),
	functor(Clause,Functor,Arity),
	get_status_constructor(Functor,Arity,ClauseStatusList,Status).

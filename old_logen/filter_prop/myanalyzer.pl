
:- module(myanalyzer,[],[assertions]).

:- use_module(program(clidlist),[ inverse_rewrite_source_program/2 ]).
:- use_module(infer(infer_db),[ inferred/3 ]).

:- use_module(library(aggregates),[ setof/3 ]).
:- use_module(library(hiordlib),[ map/3 ]). % instrumental
:- use_module(library(lists),[ length/2 ]).

% ------------------------------------------------------------------------
:- comment(module,"This module implements a naive analyzer using the
	CiaoPP interface.").
:- comment(author,"F. Bueno").

:- comment(bug,"Currently, the analysis is a bit too naive!").

% ------------------------------------------------------------------------
:- comment(analysis/1,"Declares that this analysis exists.").
:- multifile analysis/1.

analysis(myinfer).

:- comment(analysis/4,"Hook for the user entry point.").
:- multifile analysis/4.

analysis(myinfer,Cls0,_Ds0,_Info):-
	% transform program to my own format
	ciaopp_to_mine(Cls0,Cls1),
	% my program analysis
	infer(Cls1).

:- comment(infer/1,"Analyzes the program and stores the information in
	CiaoPP's DB.").

infer(Cls):-
	clauses_of_each_predicate(Cls,Result),
	% store in CiaoPP's DB
 	map(Result,store,_).

store(clnum(Pred,NumCls),_):-
	assertz_fact(inferred(myinfer,Pred,NumCls)).

:- comment(inferred_to_property/6,"Allows CiaoPP to turn my information
	into a property for the output of assertions.").

:- multifile inferred_to_property/6.

inferred_to_property(myinfer,_Goal,NumCls,[],[],[num_of_clauses(NumCls)]).

% NumCls in inferred_to_property/6 and in store/2 correspond to the same 
% class of things: what my analysis has inferred for a predicate

% ------------------------------------------------------------------------
% This is my own very nice analysis:

clauses_of_each_predicate(Cls,Result):-
	setof(F/A, H^B^( member((H:-B),Cls), functor(H,F,A) ), Heads),
	map(Heads,clauses(Cls),Result).

clauses(F/A,Cls,clnum(Pred,NumCls)):-
	functor(Pred,F,A),
	setof(Pred, B^member((Pred:-B),Cls), Heads),
	length(Heads,NumCls).

% ------------------------------------------------------------------------

ciaopp_to_mine(Cls0,Cls2):-
	% take all keys out
	inverse_rewrite_source_program(Cls0,Cls1),
	% ciaopp representation to mine
	map(Cls1,ciaopp2mine,Cls2).

ciaopp2mine(clause(H,B),(H:-B)).

% ------------------------------------------------------------------------
% That's it!

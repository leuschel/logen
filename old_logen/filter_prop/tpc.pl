:- module(tpc, [],[assertions]).

:- use_module(readprog).
:- use_module(domainProg).
%:- use_module(tbta).
:- use_module(tp).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
%:- use_module(inferred_db).
:- use_module(infer(infer_db),[ inferred/3 ]).
%:- use_module(library(infer)).
:- use_module(library(lists)).
%:- use_module(library(ciaopp)).

:- comment(analysis/1,"Declares that this analysis exists.").
:- multifile analysis/1.

analysis(tp(_)).

:- comment(analysis/4,"Hook for the user entry point.").
:- multifile analysis/4.

% so that your analysis can be called from CiaoPP, and, additionally:

:- comment(inferred_to_property/6,"Allows CiaoPP to turn my information
	into a property for the output of assertions.").
:- multifile inferred_to_property/6.

% so that CiaoPP printer can print the results of your analysis.

% Have a look at /home/clip/Systems/ciaopp/myanalyzer.pl

% Paco

% This is the interface to ciaopp

analysis(tp(Int),Cs,D,M2) :-
	jpg_program_format(Cs,D,Cls),
	domainProg(Cls,MCls),
	readprog(Int,ICls),
	tpr(ICls,[],M1),
	tpr(MCls,M1,M2),
	%assertAnalysisResults(M2,tp(Int)).
	displayClauses(user_output,M2).
	


assertAnalysisResults([],_).
assertAnalysisResults([(H :- _)|Cls],An) :-
	denotes_pred(H),	% skip analysis results for denotes preds.
	!,
	assertAnalysisResults(Cls,An).
assertAnalysisResults([(H :- B)|Cls],An) :-
	asserta_fact(inferred(An,H,B)),
	assertAnalysisResults(Cls,An).
	
denotes_pred(H) :-
	name(H,Hn),
	name('denotes_', Dn),
	append(Dn,_,Hn).

inferred_to_property(tp(_),H,[],H,disjointType([]),fails) :-
	!.
inferred_to_property(tp(_),H,B,H,disjointType(B),_).

:- module(autobta, [test/0, bta/2]).

:- use_module(queryGen).
:- use_module(readprog).
:- use_module(domainProg).
%:- use_module(annotate).
:- use_module(library(lists)).
:- use_module(tp).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(filters).



% bta(Prog, Int, Query):  
% Prog:  source program
% Int:   a pre-interpretation (a complete deterministic tree automaton,
%        derived by determinizing a regular type)

% writes a file containing the derived filter declarations.



bta(Prog,Int) :-
	start_time,
	readprog(Prog,Cls),
	removeLogenAnnotations(Cls,Cls1),
	domainProg(Cls1,MCls),
	readprog(Int,ICls),
	tpr(ICls,[],M1),		% model of the type predicates Int
	tpr(MCls,M1,M2),		% abstract model of the program Prog
	queryGen(Cls,QCls),
	domainProg(QCls,QMCls),
	userFilters(Cls,ICls,Fs),
	queryFilters(Fs,QMCls,QMCls1),
	tpr(QMCls1,M2,M3),		% abstract model of queries
	genFilters(M3,Filters),
	outfileName(Prog,Int,Outfile),
	open(Outfile,write,S),
	writeFilters(S,Filters),
	close(S),
	end_time(user_output),
	write(user_output,'----------------'),
	nl(user_output).
	
outfileName(F,I,G) :-
	name(F,FN),
	name(I,IN),
	strip_path(IN,IN1),
	name('.pl',PL),
	strip_ext(FN,PL,FN1),
	strip_ext(IN1,PL,IN2),
	append(FN1,[95|IN2],GN1),
	name('.filters',Filters),
	append(GN1,Filters,GN),
	name(G,GN).
	
% strips the extension from the filename
strip_ext(F,Ext,F1) :-
	append(F1,Ext,F),
	!.
strip_ext(F,_,F).

% strips off the path up to the file name itself
strip_path(F,F1) :-
	name('/',[Slash]),
	append(_,[Slash|F1],F),
	\+ member(Slash,F1),
	!.
strip_path(F,F).

queryFilters(Fs,[predicates(Ps)|QMCls],[predicates(Ps)|QMCls1]) :-
	append(Fs,QMCls,QMCls1).
	

test :-
	bta('Tests/rev.pl', 'Tests/sd.pl'),
	bta('Tests/rev.pl', 'Tests/logenbta.pl'),
	bta('Tests/parser.pl', 'Tests/logenbta.pl'),
	bta('Tests/match.pl', 'Tests/logenbta.pl'),
	bta('logen_examples/matchnew.ann','Tests/logenbta.pl'),
	bta('logen_examples/mapnew.ann','Tests/logenbta.pl'). 	

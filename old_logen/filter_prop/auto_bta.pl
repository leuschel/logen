:- module(auto_bta, [main/1, test/1,filter_prop/3]).

:- use_module(queryGenIndexed).
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
:- use_module(makelogenbta).

:- use_module('logen/filter',[translate_list_of_filters/2]).
:- use_module(abstractBuiltins).


% bta(Prog, Int, Query):  
% Prog:  source program
% Int:   a pre-interpretation (a complete deterministic tree automaton,
%        derived by determinizing a regular type)

% writes a file containing the derived filter declarations.

main([Prog,Int,Outfile]) :-
	bta(Prog,Int,Outfile).

filter_prop(P,I,O):-	
	bta(P,I,O).


bta(Prog,Int,Outfile) :-
	%start_time,
	%print('Opening annotation file: '), print(Prog), nl,
	readprog(Prog,Cls),
	%print('Removing Logen Annotations'),nl,
	removeLogenAnnotations(Cls,Cls1),
	%print('Starting Propagation'),nl,
	domainProg(Cls1,MCls),
	readprog(Int,ICls),
	
	portray_clause(defined_types_int(ICls)),	
	
	allTypes(ICls,[],Ts),
	%portray_clause(defined_types(Cls,Ts,Fs)),	
	userFilters(Cls,Ts,Fs),
	%portray_clause(defined_types(Cls,Ts,Fs)),	

	abstractCallSuccess(Ts,ACalls,ASuccs),	
	
	tpr(ICls,ASuccs,M1),		% model of the type predicates Int
	tpr(MCls,M1,M2),		% abstract model of the program Prog

	queryGen(Cls,QCls),
	write(user_error,'Query program generated.'), nl(user_error),

	domainProg(QCls,QMCls),

	queryFilters(Fs,QMCls,QMCls1),

	tpr(QMCls1,M2,M3),		% abstract model of queries
	checkBuiltinCalls(M3, ACalls, Ts, ProblemCalls),		% added by JG
	%portray_clause(M3),

	%portray_clause(user_error,calling(genFilters(M3,Filters))),
	genFilters(M3,Filters),
	%portray_clause(user_error,exiting(genFilters/2)),
	

	%outfileName(Prog,Int,Outfile),
	open(Outfile,write,S),
	print(translating_test(Filters)),nl,nl,
	%print('Outputting New Filter Declarations'),nl,
	%translate_list_of_filters(user_output,Filters),

	
	
	translate_list_of_filters(S,Filters),
	translate_list_of_filters(user,Filters),

	print(prob(ProblemCalls)),nl,
	
	writeInvalidCalls(S,ProblemCalls), 	% added by JG

	%print('In DIT format:'),nl,
	%writeFilters(user_output,Filters),
	
	close(S).
	
test(match) :-
	makelogenbta('./match.pl','logen/logenbta.pl','listtype.pl'),
	main(['./match.pl.ann','logen/logenbta.pl',match1out]).
test(err) :-
	makelogenbta('./int_env.pl','logen/logenbta.pl','listtype.pl'),
	main(['./int_env.pl.ann','logen/logenbta.pl',errout]).	
test(1) :-
	makelogenbta('logen_examples/simple.pl','logen/logenbta.pl','listtype.pl'),
	main(['logen_examples/simple.pl.ann','logen/logenbta.pl',simpleout]).

test(2) :-
	makelogenbta('logen_examples/match.pl','logen/logenbta.pl','listtype.pl'),
	main(['logen_examples/match.pl.ann','logen/logenbta.pl',matchout]).
test(3) :-
      makelogenbta('logen_examples/regexp.pl','logen/logenbta.pl','listtype.pl'),
	main(['logen_examples/regexp.pl.ann','logen/logenbta.pl',regexpout]).
test(4) :-
	makelogenbta('~/Research/LP/APE/sppic2',
	             'logen/logenbta.pl',
	             '~/Research/Systems/bta/Tests/pictype.pl'),
	main(['~/Research/LP/APE/sppic2','logen/logenbta.pl',pic2out]).
test(5) :-
	makelogenbta('~/Research/LP/APE/sppic2_2',
	             'logen/logenbta.pl',
	             '~/Research/Systems/bta/empty.pl'),
	main(['~/Research/LP/APE/sppic2_2','logen/logenbta.pl',pic2out]).
test(6) :-
	makelogenbta('~/Research/LP/APE/sppic2_2',
	             'logen/logenbta.pl',
	             '~/Research/Systems/bta/listtype.pl'),
	main(['~/Research/LP/APE/sppic2_2','logen/logenbta.pl',pic2out]).
test(7) :-
	makelogenbta('logen_examples/transpose.pl','logen/logenbta.pl','Tests/matrixtype.pl'),
	main(['logen_examples/transpose.pl.ann','logen/logenbta.pl',transout]).
test(8) :-
	makelogenbta('logen_examples/lookup.pl','logen/logenbta.pl','listtype.pl'),
	main(['logen_examples/lookup.pl.ann','logen/logenbta.pl',lookupout]).

queryFilters(Fs,[predicates(Ps)|QMCls],[predicates(Rs)|QMCls1]) :-
	append(Fs,QMCls,QMCls1),
	queryPreds(Fs,Qs),
	setunion(Qs,Ps,Rs).
	
queryPreds([],[]).
queryPreds([clause((Q :- true),[])|Fs],[P/N|Ps]) :-
	functor(Q,P,N),
	queryPreds(Fs,Ps).
	


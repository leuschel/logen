

:-module('trial_tests.pl', [test/0]).


:- use_module('trial.pl').

:- use_module(library(lists)).

	
test :-
	%Advisor = [time],
	%Match = [time],
	%Advisor = [size],
	%Relative = [size],
	%Mission = [size],
	Vanilla = [time, size, size_time],
	
	Advisor = [time,size, size_time],
	Match = [time,size, size_time],
  	%GUnify = [size_time],
  	RegExp = [time,size, size_time],
  	%SSUPPLY = [],
  	Mission = [time,size,  size_time],
  	Relative = [time,size,  size_time],
  	MemberGlob = [time,size,  size_time],
 	Index = [time,size,  size_time],
	

	test_set(Advisor, advisor, AdvisorResult),
	print_set(Advisor, AdvisorResult),
	
	test_set(Match, match, MatchResult),
	print_set(Match, MatchResult),
		
	test_set(MemberGlob, member_glob, MemberResult),
	print_set(MemberGlob,  MemberResult),
	
	test_set(RegExp, regexp, RXResult),
	print_set(RegExp,  RXResult),
	
	test_set(GUnify, gunify, GUResult),
	print_set(GUnify,  GUResult),
	
	test_set(SSUPPLY, ssupply, SSResult),
	print_set(SSUPPLY, SSResult),
	
	test_set(Mission, mission, MIResult),
	print_set(Mission, MIResult),
	
	test_set(Relative, relative, REResult),
	print_set(Relative, REResult),

	test_set(Index, index_test, IndexResult),
	print_set(Index, IndexResult),

	test_set(Vanilla, vanilla, VanResult),
	print_set(Vanilla, VanResult),

	print_set(Match, MatchResult),
	print_set(Advisor, AdvisorResult),		
	print_set(MemberGlob,  MemberResult),
	print_set(RegExp,  RXResult),
	print_set(GUnify,  GUResult),
	print_set(SSUPPLY, SSResult),
	print_set(Mission, MIResult),
	print_set(Relative, REResult),
	print_set(Index, IndexResult),
	print_set(Vanilla, VanResult).





	
testcase(match, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 100,	Norm = term,	Iter = 100000,
	File =    'trial/testcases/match.pl',
	AnnFile = 'trial/testcases/match.pl.ann',
	Query = match([a,a,c], [a,a,a,a,a,a,a,a,a,a,a,a,a,a,d,a,a,a,a,c]).

testcase(regexp, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 1000000,
	File =    'trial/testcases/regexp.pl',
	AnnFile = 'trial/testcases/regexp.pl.ann',
	Query = match(*(.(a,+(b,c))), [a,c,a,b,a,c,a,b]).

testcase(advisor, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 100,	Norm = term,	Iter = 1000000,
	File =    'trial/testcases/advisor.pl',
	AnnFile = 'trial/testcases/advisor.pl.ann',
	Query = what_to_do_today( first_of_may, windy, _Program ).

testcase(member_glob, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = list,	Iter = 500000,
	File =    'trial/testcases/member_glob_explosion.pl',
	AnnFile = 'trial/testcases/member_glob_explosion.pl.ann',
	Query = test(h, [a,b,c,d,e,f,g,h]).

testcase(ssupply, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 1000000,
	File =    'trial/testcases/ssupply.pl',
	AnnFile = 'trial/testcases/ssupply.pl.ann',
	Query = ssupply( s3, p1, _quantity ).

testcase(gunify, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 100000,
	File =    'trial/testcases/groundunify.pl',
	AnnFile = 'trial/testcases/groundunify.pl.ann',
	Query = unify(struct(f,[var(1),var(2)]),struct(f,[var(3),var(3)]),_S).

testcase(mission, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 100000,
	File =    'trial/testcases/missionaries.pl',
	AnnFile = 'trial/testcases/missionaries.pl.ann',
	Query = go(_).

testcase(relative, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 1000000,
	File =    'trial/testcases/relative.pl',
	AnnFile = 'trial/testcases/relative.pl.ann',
	Query = go(_).

testcase(index_test, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 10000000,
	File =    'trial/testcases/index_test.pl',
	AnnFile = 'trial/testcases/index_test.pl.ann',
	Query = index_test(f(_), j,_).

testcase(vanilla, Timeout, Norm, Iter, File, AnnFile, Query) :-
	Timeout = 1000,	Norm = term,	Iter = 100000,
	File =    'trial/testcases/vanilla.pl',
	AnnFile = 'trial/testcases/vanilla.pl.ann',
	%gen1(F,200000),	
	%Query = solve([p(0,F)]).
	Query = test(_).












test(Test, Answer, Time, Score,Count) :-
	testcase(Test, Timeout, Norm, Iter, File, AnnFile, Query),
	(Score = size ->
	    Iterations = 0  %%% no need to benchmark just for size...
	;
	    Iterations = Iter
	),
	statistics(walltime, [W1,_]),	
	run(File,AnnFile, Query, Iterations, Norm, Timeout, Answer, Score,Count),
	statistics(walltime, [W2, _]),
	Time is W2-W1,
	portray_clause(completed(walltime, Time)).


	
print_result(Format) :-
	format(user, "~n~n Testing ~w with score function ~w, completed in ~wms tried ~w mutations with answers:~n~@", Format).


	
print_set(Flag, [Format1, Format2, Format3]) :-
	
	(member(time, Flag) ->

	    print_result(Format1)
	;
	    true
	),
	(member(size, Flag) ->
	    print_result(Format2)
	;
	    true
	),
	(member(size_time, Flag) ->
	    print_result(Format3)
	;
	    true
	).


test_set(Flag, TestCase, [OutTime,OutSize,OutBoth]) :-
	(var(Flag) -> Flag = []; true),
	(member(time, Flag) ->
	    run_testcase(TestCase, time, OutTime)

	  
	;
	    true
	),
	(member(size, Flag) ->
	    run_testcase(TestCase, size, OutSize)
	;
	    true
	),
	(member(size_time, Flag) ->
	    run_testcase(TestCase, size_time, OutBoth)
	;
	    true
	).
	
	

run_testcase(Test, ScoreFunc, [Test, ScoreFunc, Time,Count, print_answer_set(Answer)]) :-
	
	test(Test, Answer, Time, ScoreFunc,Count),
		    flush_output(user),flush_output(user_error).
	
	


print_answer_set([]).
print_answer_set([A|As]) :-
	portray_clause(A),
	print_answer_set(As).


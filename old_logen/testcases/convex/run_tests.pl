:- use_module('../../convex_analyser.pl').

run_tests :-
	run_test(append_test).

run_test(Test) :-	
	call(Test),
	!,
	write_term(Test-ok,[numbervars(true)]).

run_test(Test) :-
	write_term(Test-failed,[numbervars(true)]).




append_test :-
	go('append.pl'),
	set_convex_norm(list),
	findall((A-B),convex_analyser:current_sol(A,B),Bag),	
	Bag = [app(A,B,C)-[A>=0,B>=0,C>=0,B-C=<rat(0,1),B>=rat(0,1),A= -(B)+C]].



	
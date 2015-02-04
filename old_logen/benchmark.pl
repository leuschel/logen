:- module(benchmark, [create_benchmark/5]).

:- use_module(library(lists)).

create_benchmark(Pl,Goal,Spec,ResGoal, Iter) :-
	create_benchmark(Pl, Goal, Iter),
	create_benchmark(Spec,ResGoal,Iter).


create_benchmark(Include, Call, Iter) :-
	name(Include, String),
	append(String,".bench", SOut),
	name(Out,SOut),
	tell(Out),
	portray_clause(term_expansion(':-'(module(_,_)), (:-true))),
	portray_clause(':-'(include(Include))),
	portray_clause(':-'(run,
			    (
			      statistics(runtime,_),
			      run_bench,
			      statistics(runtime,[_,RT]),
			      format("~n*******************************~n~w~n~w ~w times~nBenchmark Completed in ~wms~n~n",[Include,Call,Iter,RT])
			    ))),
	make_clause(run_bench,Call, Iter,Clause),
	portray_clause(Clause),			    
	told,
	true.




make_clause(Head, Call, Iter, Clause) :-
	Interval = 100,
	Gap = 10000,
	Iter > Gap,
	Iter =< Gap *Interval, %1000000,
	X is Iter / Gap,
	name(Gap, GapS),
	append("run", GapS,NewHeadS),
	name(NewHead,NewHeadS),	
	make_clause(NewHead, Call, Gap, (H:-B)),
	portray_clause((H:-B)),
	make_clause(Head, H,X,Clause).

make_clause(Head, Call, Iter, Clause) :-
	Interval = 100,
	Gap = 100,
	Iter > Gap,
	Iter =< Gap *Interval, %1000000,
	X is Iter / Gap,
	name(Gap, GapS),
	append("run", GapS,NewHeadS),
	name(NewHead,NewHeadS),	
	make_clause(NewHead, Call, Gap, (H:-B)),
	portray_clause((H:-B)),
	make_clause(Head, H,X,Clause).


%%% make_clause(Head, Call, Iter, Clause) :-
%%% 	Iter > 100,
%%% 	Iter =< 10000,
%%% 	X is Iter / 100,
%%% 	make_clause(run100, Call, 100, (H:-B)),
%%% 	portray_clause((H:-B)),
%%% 	make_clause(Head, H,X,Clause).
	

	
make_clause(Head,Call,Iter,(Head:-Body)) :-
        Iter =< 100,
	make_body(Call,Iter,Body).
%%%%%	portray_clause((Head:-Body)).


%make_body(Goal, X,X-Goal) :- portray_clause(make_body(Goal,X,Goal)), !.

make_body(Goal, 0, Goal).
make_body(Goal, 0.0, Goal).

make_body(Goal, X, (CGoal, Rest)) :-
	X > 0,
	copy_term(Goal, CGoal),
	X1 is X -1,
	make_body(Goal,X1, Rest).





	
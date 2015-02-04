/* A simple entry point file; just to allow to run .gx files */

:- module(run_gx,[specialise_plfile/4,
		  specialise_plfile/5,
		   specialise_plfile/7
		 
		  ]).
:- use_module(library(timeout)).

:- use_module(moduledriver,[run_from_pattern/3,
                            rebuild_memo/0, rebuild_everything/0]).

specialise_plfile(File,Goal,Rebuild,ResidualGoal, SpecTime, Timeout,Result) :-
	Call = specialise_plfile(File,Goal,Rebuild,ResidualGoal, SpecTime),
	(Timeout \= -1 ->	    
	    time_out(
		     Call,
		    Timeout,
		     Result
		    )
	;
	    Call,
	    Result = success
	).

		 

specialise_plfile(File,Goal,Rebuild,ResidualGoal, SpecTime) :-
   % nl,print(starting_specialisation),nl,
	statistics(runtime,[T1,_]),
	specialise_plfile(File,Goal,Rebuild,ResidualGoal),
	statistics(runtime,[T2,_]),
	SpecTime is T2 - T1,
	portray_clause(user_error,specialisation_and_load_time(SpecTime)).



specialise_plfile(File,Goal,Rebuild,ResidualGoal) :-
	(Rebuild=rebuild_everything ->
	    rebuild_everything
	;
	    (Rebuild=rebuild_memo -> rebuild_memo ; true)
	),
	run_from_patterns(File,Goal,ResidualGoal).


run_from_patterns(File,(G1,G2),(RG1,RG2)) :- !,
  run_from_patterns(File,G1,RG1),
  run_from_patterns(File,G2,RG2).
  
run_from_patterns(File,Goal,ResidualGoal) :-
  %print(run_from_pattern(File,Goal,ResidualGoal)),nl,
  run_from_pattern(File,Goal,ResidualGoal).
  %print(done_run_from_pattern(File,Goal,ResidualGoal)),nl.
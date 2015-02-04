
:- ensure_loaded('sicstus.pl').
:- ensure_loaded('socket.pl').
:- ensure_loaded('logen_main.pl').

:- use_module('logen_benchmark').
:- use_module('logen_benchmark_loop').
:- use_module('logen_dispatch').

:- use_module(library(lists)).

%runtime_entry(start) :- prolog_socket.
runtime_entry(start) :- main_entry.



main_entry :-
	prolog_flag(argv,ArgV),
	length(ArgV, X),
	(X = 0 ->
	    prolog_socket
	;
	    on_exception(E,cmd_line_version(ArgV),portray_clause(exception(E)))
	),
	!.

main_entry :-
	prolog_flag(argv,ArgV),
	portray_clause(pylogen_failed(args(ArgV))).


:- use_module(library(charsio)).

cmd_line_version(Argv) :-
	
	get_options(Argv,Options,_Args),

	%portray_clause(user,Options),
	
	(memberchk(benchmark(File,Goal, Times),Options) ->
	    portray_clause(user, running_benchmarks),
	    name(Goal, GoalS),
	    name(Times,TimesS),
	    append(GoalS, ".", GoalStop),
	    append(TimesS, ".", TimesStop),
	    read_term_from_chars(GoalStop, GoalAtom,[]),
	    read_term_from_chars(TimesStop, TimesAtom,[]),	    
	    run_benchmark(File, GoalAtom, TimesAtom) 
	;
	    true
	),
	(memberchk(benchmarkspec(File,Goal, Times),Options) ->
	    portray_clause(user, running_benchmarks),
	    name(Goal, GoalS),
	    name(Times,TimesS),
	    append(GoalS, ".", GoalStop),
	    append(TimesS, ".", TimesStop),
	    read_term_from_chars(GoalStop, GoalAtom,[]),
	    read_term_from_chars(TimesStop, TimesAtom,[]),	    
	    run_benchmark_spec_only(File, GoalAtom, TimesAtom,_)
	;
	    true
	),
	(memberchk(benchmarkpl(File,Goal, Times),Options) ->
	    portray_clause(user, running_benchmarks),
	    name(Goal, GoalS),
	    name(Times,TimesS),
	    append(GoalS, ".", GoalStop),
	    append(TimesS, ".", TimesStop),
	    read_term_from_chars(GoalStop, GoalAtom,[]),
	    read_term_from_chars(TimesStop, TimesAtom,[]),	    
	    run_benchmark_pl_only(File, GoalAtom, TimesAtom,_)
	;
	    true
	),
	(memberchk(benchmarkloop(File,Goal, Times),Options) ->
	    portray_clause(user, running_benchmarks),
	    format(user, "Running benchmark for ~w~n", [File]),
	    name(Goal, GoalS),
	    append(GoalS, ".", GoalStop),	    
	    read_term_from_chars(GoalStop, GoalAtom,[]),
	    atom_codes(Times,TimesCode), number_codes(TimesNum, TimesCode),
	    run_benchmark_loop(File,GoalAtom,TimesNum,_TT)
	;
	    true
	),	
	(memberchk(pp(Spec,Memo,Opt), Options) ->
	    %portray_clause(user_error, pp(Spec,Memo,Opt)),
	    name(Opt,OptS),
	    append(OptS,".",OptStop),
	    read_term_from_chars(OptStop, OptList,[]),
	    run_pp(Spec,Memo,OptList)
	
	;
	    true
	),
	(memberchk(dispatch(Path,Spec,Memo,File,Out), Options) ->
	    %portray_clause(user_error, pp(Spec,Memo,Opt)),
	    build_dispatch(Path,Spec,Memo,File,Out)

	
	;
	    true
	).
	
	
	       
	
	

	




recognised_option('-benchmark',[File, Goal, Times],benchmark(File, Goal, Times)).
recognised_option('-benchmarkloop',[File, Goal, Times],benchmarkloop(File, Goal, Times)).
recognised_option('-benchmarkspec',[File, Goal, Times],benchmarkspec(File, Goal, Times)).
recognised_option('-benchmarkpl',[File, Goal, Times],benchmarkpl(File, Goal, Times)).
recognised_option('-pp', [Spec,Memo,Options], pp(Spec,Memo,Options)).
recognised_option('-dispatch', [Path,Spec,Memo,File,Out], dispatch(Path,Spec,Memo,File,Out)).

get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,As,Opt) ->
       (
	 append(As,T1,T),
	 %portray_clause(Opt),
	 Options = [Opt|OT],
	 Args = AT
       )
   ;
      (
	T1 = T,
	Options = OT,
	Args = [X|AT]
      )
   ),
   get_options(T1,OT,AT).


save :- save_program('pylogen_main.sav').


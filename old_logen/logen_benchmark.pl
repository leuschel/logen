
:- module('logen_benchmark', [run_benchmark/3,run_benchmark_spec_only/4,
			      run_benchmark_pl_only/4]).


:- use_module(moduledriver).
:- use_module('prob/logen_preferences.pl').

:- use_module('logen_codesize.pl',[get_code_size/2]).

:- use_module('run_cogen.pl').
:- use_module('run_gx.pl').
:- use_module('cogen-tools', [application_path/1]).
%%% This module should benchmark a specialised program
%%% against the original program.

run_benchmark_spec_only(File,Call,Times,TT) :-	
	application_path(Dir),
	atom_concat(Dir, '/benchmark/benchmark.pl', BENCHMARK),	
	GOAL = benchmark_spec_only(File,Call,Times,TT),	
	run(BENCHMARK,GOAL,_Memo,Spec),	
	ensure_loaded(Spec),
	'benchmark.spec':GOAL,	
	atom_concat(Base,'.pl', File),
	atom_concat(Base,'.spec', SpecFile),	
	get_code_size(SpecFile,CodeSize),	
	format(user, "Specialised Compiled Code Size: ~d bytes~n", [CodeSize]).

run_benchmark_pl_only(File,Call,Times,TT) :-	
	application_path(Dir),
	atom_concat(Dir, '/benchmark/benchmark.pl', BENCHMARK),	
	GOAL = run_benchmark(File,Call,Times,TT),	
	run(BENCHMARK,GOAL,_Memo,Spec),	
	ensure_loaded(Spec),
	'benchmark.spec':GOAL,	
	get_code_size(File,CodeSize),	
	format(user, "Original Compiled Code Size: ~d bytes~n", [CodeSize]).



run_benchmark(File,Call,Times) :-
	application_path(Dir),
	atom_concat(Dir, '/benchmark/benchmark.pl', BENCHMARK),
	GOAL = benchmark_specialised_file(File,Call,Times),
	run(BENCHMARK,GOAL,_Memo,Spec),
	ensure_loaded(Spec),
	'benchmark.spec':GOAL,
	get_code_size(File,OrigCodeSize),
	atom_concat(Base,'.pl', File),
	atom_concat(Base,'.spec', SpecFile),
	get_code_size(SpecFile,SpecCodeSize),	
	
	format(user, "Original Compiled Code Size: ~w bytes~n", [OrigCodeSize]),
	format(user, "Specialised Compiled Code Size: ~w bytes~n", [SpecCodeSize]).
	
	
	
%% This should load Logen and run a Goal in a file
%% File is PL file
%% Goal is goal to execute
%% Memo, and Spec are unified with out put files
run(File, Goal,Memo,Spec) :-
	init_preferences,
	run_cogen_if_necessary(File),
	specialise_plfile(File, Goal,rebuild_memo,_R),			    
	moduledriver:getattr(Module, file(spec), Spec),
	moduledriver:getattr(Module, file(memo), Memo).
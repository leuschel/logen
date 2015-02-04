:- module(run_binsolve, [run_binsolve/3,run_binsolve/1,bin_clause/2,run_binsolve_halt/1]).

:-dynamic bin_clause/2.
:- use_module('logen_main.pl').
:- use_module(moduledriver).
:- use_module('prob/logen_preferences.pl').

:- use_module('run_cogen.pl').
:- use_module('run_gx.pl').



:- use_module('cogen-tools', [application_path/1]).

run_binsolve(File) :-
	%portray_clause(run_binsolve(File)),
	run_binsolve(File,_,_).

run_binsolve(File,Memo,Spec) :-
	%run('../examples/bin_via.pl',solve_file(File),Memo,Spec).
	application_path(Dir),
	atom_concat(Dir, '/bta_files/bin_via.pl', BINVIA),	
	run(BINVIA,solve_file(File),Memo,Spec).
%	portray_clause(files(Memo,Spec)).

%% This should load Logen and run a Goal in a file
%% File is PL file
%% Goal is goal to execute
%% Memo, and Spec are unified with out put files
run(File, Goal,Memo,Spec) :-
%	restore('logen_main.sav'),
	init_preferences,

	run_cogen_if_necessary(File),

	specialise_plfile(File, Goal,rebuild_memo,_R),

			    
	%moduledriver:rebuild_everything,
	%moduledriver:request_pattern(File,Goal),
	%moduledriver:specialize,
	moduledriver:getattr(Module, file(spec), Spec),
	moduledriver:getattr(Module, file(memo), Memo).



run_binsolve_halt(File) :- run_binsolve(File),halt.



:- module(run_binmemo, [run_binmemo/1]).

%:-dynamic bin_clause/2.
:- use_module('logen_main.pl').

run_binmemo(File) :-
	portray_clause(run_binsolve(File)),
	run_binmemo(File,_,_).

run_binmemo(File,Memo,Spec) :-
	run('../examples/bin_memo.pl',solve_file(File),Memo,Spec),
	portray_clause(files(Memo,Spec)).

%% This should load Logen and run a Goal in a file
%% File is PL file
%% Goal is goal to execute
%% Memo, and Spec are unified with out put files
run(File, Goal,Memo,Spec) :-
%	restore('logen_main.sav'),
	moduledriver:rebuild_everything,
	moduledriver:request_pattern(File,Goal),
	moduledriver:specialize,
	moduledriver:getattr(Module, file(spec), Spec),
	moduledriver:getattr(Module, file(memo), Memo).



run_binsolve_halt(File) :- run_binsolve(File),halt.



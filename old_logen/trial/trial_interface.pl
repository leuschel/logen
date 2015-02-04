:- module('trial_interface.pl', [run_bta/4, run_spec/4, run_bench/4,run_filterprop/3,build/0]).


:- use_module(library(system)).

:- use_module(library(charsio)).

:- use_module(library(lists)).


%%% Should the output of commands be shown?
debug_output([null, null, null]) :- !.
%debug_output([std, null, std]).

build :-
	working_directory(Old, '..'),
	system('sicstus -l logen_cli.pl --goal "save,halt."'),
	system('sicstus --goal "set_prolog_flag(single_var_warnings,off), ensure_loaded(\'pylogen_main.pl\'),save_program(\'pylogen_main.sav\'),halt."'),
	system('sicstus --goal "set_prolog_flag(single_var_warnings,off),ensure_loaded(\'run_bta.pl\'),save,halt."'),
	working_directory(_, Old).
	
	
	


run_filterprop(Filename, Output, AnnFile) :-
	filterprop_cmd(Filename,  Output, AnnFile, Cmd),
	run_logen_cmd(Cmd,_s).



run_bta(Filename, Norm, Output, AnnFile) :-	
	bta_cmd(Filename, Norm, Output, AnnFile, Cmd),	
	run_logen_cmd(Cmd,_s).


delete_if_exists(File) :-
	atom_concat('../', File, UPFile),
	(file_exists(UPFile) ->
	    portray_clause(deleting(UPFile)),
	    delete_file(UPFile)
	;
	    
	    true
	).
run_spec(File, Query,SpecFile, Timeout) :-	
	name(File, FileS),
	append(BASE, ".pl", FileS),
	append(BASE, ".spec", SpecFileS),
	name(SpecFile, SpecFileS),	
	spec_cmd(File, Query, Cmd,Timeout),
	run_logen_cmd(Cmd,_s),

	%%% check for error_in_filter errors!
	error_in_filter_cmd(SpecFile, ErrCmd),
	system(ErrCmd, S),
	(file_exists('../err.txt') ->
	    fail
	;
	    true
	),
	%%portray_clause(status(S)),
	(S == 0 ->
	    fail %%% Specialisation failed!
	;	    
	    true
	).
	

	

run_bench(File, Query, Times, Result) :-
	%portray_clause(user, File),
	OUTFILE = '../bench_output.tmp',
	
	(file_exists(OUTFILE) ->
	    delete_file(OUTFILE)
	;
	    true
	),
	remove_ql_file(File),
	bench_cmd(File, Query, Times, Cmd),
	run_logen_cmd(Cmd,_s),
	!,
	get_bench_results(OUTFILE, Result).


remove_ql_file(File) :-
	name(File, FileS),

	(append(BASE, ".pl", FileS) ->
	    true
	;
	    append(BASE, ".spec", FileS)
	),

	append(BASE, ".ql", QLS),name(QL, QLS),
	(file_exists(QL) ->
	    delete_file(QL)
	;
	    true
	).

get_bench_results(OUTFILE, Result) :-
	file_exists(OUTFILE),
	!,
	open(OUTFILE, read,InS),
	read_term(InS, bench(Time, Iter, Size), []),
	close(InS),
	Result = bench(Time, Iter, Size).

get_bench_results(_, fail) :-
	portray_clause(user, no_bench_file).
	%halt.





run_logen_cmd(Cmd,Status) :-
	working_directory(Old, '..'),
	%system(Cmd),
	debug_output(Streams),
	exec(Cmd, Streams, P),
	wait(P, Status),	
	working_directory(_, Old).

filterprop_cmd(Filename, Output, AnnFile, Cmd) :-
	format_to_chars(
	 "sicstus -r run_bta.sav --goal \"runtime_entry(start), halt.\" -a \"~a\" -o \"~a\" -i \"~a\" -gui -filter",
	 [Filename, Output, AnnFile], Chars),
	name(Cmd, Chars).

bta_cmd(Filename, Norm, Output, AnnFile, Cmd) :-
	format_to_chars(
	 "sicstus -r run_bta.sav --goal \"runtime_entry(start), halt.\" -a \"~a\" -a -norm ~a -o \"~a\" -i \"~a\" -gui",
	 [Filename, Norm, Output, AnnFile], Chars),
	name(Cmd, Chars).


spec_cmd(File, Query, Cmd, Timeout) :-
	format_to_chars("sicstus -r run_gx.sav --goal \"go, halt.\" -a \"~a\" \"~w\" -t ~w -errorlog err.txt", [File, Query, Timeout], Chars),
	name(Cmd, Chars).

error_in_filter_cmd(SpecFile, Cmd) :-
	format_to_chars("grep -q error_in_filter \"~w\"", [SpecFile], Chars),
	name(Cmd, Chars).

bench_cmd(File, Query, Times, Cmd) :-
	format_to_chars("sicstus -r pylogen_main.sav --goal \"runtime_entry(start), halt.\" -a -benchmarkloop \"~a\" \"~w\" ~w", [File, Query, Times], Chars),
	name(Cmd, Chars).




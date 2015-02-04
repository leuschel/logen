
:- module(tools,[
		 %ann_clause/3,
		 %clear_ann_clauses/0,
		     clear_stacks/0,
		     consult_without_redefine_warning/1,
		     copy/2,
		     cputime/1,
		     ensure_consulted/1,
		     examples_reconsult/1,
		     examples_see/1,
		     examples_tell/1,
		 %filter/2,
		     get_env_var/2,
		     is_inf/1,
		     logen_examples_directory/1,
		     logen_source_directory/1,
		     logen_reconsult/1,
		     namevars/4,
		     not/1,
		     reconsult_without_redefine_warning/1,
		 %residual/1,
		     stack_size/3,
		 %static_consult/1,
		     stop/0,
		     string_concatenate/3,
		 %table/1,
		     time/2,
		     time/1,
		 %tools_call/1,
		     varlist/2
		    ]).

:- use_module('cgs-io').
:- consult('cgs-sicstool.pl').
:- use_module(memo).
:- use_module(pp).

% allow access to ann file, at the moment this is just for compatibility
:- use_module('annfile').

examples_reconsult(File) :-
	logen_examples_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	reconsult_without_redefine_warning(CF).

examples_tell(File) :-
	logen_examples_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	cgs_set_output(CF).

examples_see(File) :-
	logen_examples_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	cgs_set_input(CF).

/* put call function here to get access to consulted data/program e.g. *.gx and *.ann */
%Steve
%This shouldnt be needed anymore with the stub ann file
tools_call(Command) :-
  call(Command).

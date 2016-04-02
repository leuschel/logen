:- module(ciao_tools, [ read_from_chars/2, environ/2,
	write_to_chars/2, format_to_chars/3]).


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).
:- use_module(library(compiler)).
:- use_module(library(dec10_io)).
:- use_module(library(prolog_sys)).
:- use_module(library(terms_check)).
:- use_module(library(dynamic)).
:- use_module(library(system)).

:- use_module(library(strings)).


%:- use_module(library(system),[current_env/2]). % corresponds to environ/2 in SICStus
environ(Key,Value) :-
	getenvstr(Key,Value).


format_to_chars(FString, FArgs, String) :-
	mktemp('/tmp/readatomXXXXXX',TmpFile),
	open(TmpFile, write, TmpOut),
	format(TmpOut, FString, FArgs),
	close(TmpOut),
	open(TmpFile, read, TmpIn),
        %read(TmpIn, Term),
	get_line(TmpIn, String),
        close(TmpIn),
	delete_file(TmpFile).

write_to_chars(Term, String) :-
	copy_term(Term, Copy),
	prettyvars(Copy),
	mktemp('/tmp/readatomXXXXXX',TmpFile),
	open(TmpFile, write, TmpOut),
        write_term(TmpOut, Copy, [quoted(true),numbervars(true)]),
	close(TmpOut),
	open(TmpFile, read, TmpIn),
        %read(TmpIn, Term),
	get_line(TmpIn, String),
        close(TmpIn),
	delete_file(TmpFile).

read_from_chars(String, Term) :-
        mktemp('/tmp/readatomXXXXXX',TmpFile),
        open(TmpFile, write, TmpOut),
        display(TmpOut, String),
        display(TmpOut, ' .\n'),
        close(TmpOut),
        open(TmpFile, read, TmpIn),
        read(TmpIn, Term),
        close(TmpIn),
	delete_file(TmpFile).
        %print(read_from_chars(String,Term)),nl.



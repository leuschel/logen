:- module(ciao_tools, [string_concatenate/3, read_from_chars/2, environ/2, print_error/1,
   print_message/1,same_length/2, is_list_skel/1,
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


is_list_skel(X) :- nonvar(X), (X=[] -> true ; (X=[_|T], is_list_skel(T))).

same_length([],[]).
same_length([_|T],[_|T2]) :- same_length(T,T2).


environ(Key,Value) :-
	getenvstr(Key,Value).


print_error(Msg) :-
	format(user_error, "!~w~n", [Msg]).

print_message(Msg) :-
	format(user, "%~w~n", [Msg]).

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


convert_cli_into_atom(CLIGOAL,Atom) :- read_from_chars(CLIGOAL,Atom).



string_concatenate(S1, S2, S3) :-
	name(S1, S1S),
	name(S2, S2S),
	append(S1S,S2S,S3S),
	name(S3,S3S).


%:- module(reportgen_xml,[reportgen/2]).

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(strings)).
:- endif.

reportgen(S, Tag) :-
	% delimit the XML with << and >> on new lines. >> is used because it can
	% never appear in valid XML.
	nl(S), write(S, '<<'), nl(S),
	rgen(Tag, S),
	nl(S), write(S, '>>'),  nl(S).

%write_string(_, []).
%write_string(S, [C|Cs]) :- put_code(S, C), write_string(S, Cs).

rgen(Tag, Out) :-
	Tag =.. [H,A], !,
	format(Out, '<~w>', [H]),
	rgen_args(A, Out),
	format(Out, '</~w>', [H]).

rgen(Tag, Out) :-
	Tag =.. [H,Attrs,A], !,
	format(Out, '<~w', [H]),
	output_attributes(Attrs, Out),
	write(Out, '>'),
	rgen_args(A, Out),
	format(Out, '</~w>', [H]).

output_attributes([], _).
output_attributes([A|As], Out) :- A =.. [L,R], format(Out, ' ~w="~w"', [L,R]),
								  output_attributes(As, Out).

rgen_args([], _) :- !.
rgen_args([A|As], Out) :-
	!, rgen(A, Out),
	rgen_args(As, Out).
rgen_args(Text, Out) :-
    (atom(Text); number(Text)), !,
    name(Text, OutCodes),
	my_html_quoted_chars(OutCodes, OutString, []),
	write_string(Out, OutString).
rgen_args(Text, Out) :-
	write_to_chars(Text, String),
	my_html_quoted_chars(String, OutString, []),
	write_string(Out, OutString).

my_html_quoted_chars([]) --> [].
my_html_quoted_chars([C|T]) -->
        my_html_quoted_char(C),
		my_html_quoted_chars(T).

my_html_quoted_char(0'>) --> !, "&gt;".
my_html_quoted_char(0'<) --> !, "&lt;".
my_html_quoted_char(0'&) --> !, "&amp;".
my_html_quoted_char(0'") --> !, "&quot;".
my_html_quoted_char(C)   --> [C].

:- if(current_prolog_flag(dialect, ciao)).
write_to_chars(Term, String) :-
        copy_term(Term, Copy),
        prettyvars(Copy),
        mktemp('/tmp/readatomXXXXXX',TmpFile),
        open(TmpFile, write, TmpOut),
        write_term(TmpOut, Copy, [quoted(true),numbervars(true)]),
        close(TmpOut),
        open(TmpFile, read, TmpIn),
        get_line(TmpIn, String),
        close(TmpIn),
        delete_file(TmpFile).
:- else.
:- use_module(library(codesio),         [write_term_to_codes/3]).
write_to_chars(Term, String) :-
   write_term_to_codes(Term,String,[quoted(true)]). %,numbervars(true)
:- endif.

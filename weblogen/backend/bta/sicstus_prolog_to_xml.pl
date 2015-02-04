:- module('prolog_to_xml.pl', [sort_highlights/2,
							   output_file_as_xml_element/3,
							   output_string_as_xml_element/3,
							   output_xml_decl/1,
							   my_html_quoted_chars/3,
							   output_file_as_html/1]).

:- use_module('../../../old_logen/annotation/match_ann.pl').
:- use_module('path.pl').
:- use_module(library(lists)).
:- use_module(library(pillow)).

output_xml_decl(Stylesheet) :-
	output_html([xmldecl(['version="1.0"','encoding="UTF-8"']), nl,
	'<?xml-stylesheet type="text/xsl" href="', Stylesheet, '"?>', nl]).

output_file_as_xml_element(Element, Filename, Highlights) :-
	open(Filename, read, Stream),
	output_html([begin(Element)]),
	output(Stream, Highlights, 1, []),
	output_html([end(Element), nl]),
	close(Stream).

output_string_as_xml_element(Element, String, Highlights) :-
	output_html([begin(Element), nl]),
	output_from_string(Highlights, 1, String),
	output_html([end(Element), nl]).

output(Stream, [], _, Previous) :-
    read_chars_to_eof(Stream, Previous, Output),
	output_quoted_lines(Output).

output(Stream, [ann(Start, Stop, Ann)|Format], Pos, Previous) :-
    N is Start - Pos,
	read_chars(Stream, N, Previous, Output, Remaining),
	output_verb_lines(Output),
	N2 is Stop - Start,
	read_chars(Stream, N2, Remaining, Output2, Remaining2),
	output_quoted_ann_lines(Ann, Output2),
	output(Stream, Format, Stop, Remaining2).

output_verb_lines(Output) :- whitespace(Output), !,
							 output_quoted_ann_lines(verb, Output).
output_verb_lines(Output) :- output_quoted_lines(Output).

whitespace([]).
whitespace([L|R]) :- whitespace_line(L), whitespace(R).

whitespace_line([]).
whitespace_line([C|L]) :- whitespace_char(C), whitespace_line(L).

whitespace_char(10).
whitespace_char(32).
whitespace_char(9).

output_quoted_lines([]).
output_quoted_lines([L|Lines]) :-
        output_quoted_line(L),
		output_quoted_lines(Lines).

output_quoted_line([]).
output_quoted_line(L) :-
        my_html_quoted_chars(L, Chars, []),
		output_html(Chars).

my_html_quoted_chars([]) --> [].
my_html_quoted_chars([C|T]) -->
        my_html_quoted_char(C),
		my_html_quoted_chars(T).

my_html_quoted_char(0'>) --> !, "&gt;".
my_html_quoted_char(0'<) --> !, "&lt;".
my_html_quoted_char(0'&) --> !, "&amp;".
my_html_quoted_char(0'") --> !, "&quot;".
%my_html_quoted_char(0' ) --> !, "<nbsp/>".
%my_html_quoted_char(10) --> !, "<br/>".
my_html_quoted_char(C)   --> [C].

output_quoted_ann_lines(_, []).

output_quoted_ann_lines(Ann, Output) :-
        output_html(begin(Ann)),
		output_quoted_lines(Output),
		output_html(end(Ann)).

read_chars(_, 0, Remaining, [], Remaining) :- !.

read_chars(Stream, N, [], Output, Remaining) :-
    read_line(Stream, Line2), !, append(Line2, [10], Line),
    read_chars(Stream, N, Line, Output, Remaining).

read_chars(_, N, Previous, [Output], Remaining) :-
    length(Previous, Np), N =< Np, !,
    split_string(Previous, Output, Remaining, N).

read_chars(Stream, N, Previous, [Previous|MoreOut], Remaining) :-
    length(Previous, Np), N2 is N - Np,
    read_chars(Stream, N2, [], MoreOut, Remaining).

read_chars_to_eof(Stream, Previous, [Previous|Output]) :-
    read_chars_to_eof(Stream, Output).

read_chars_to_eof(Stream, []) :- at_end_of_stream(Stream), !.
read_chars_to_eof(Stream, [Line|Output]) :-
    read_line(Stream, Line2), append(Line2, "\n", Line),
    read_chars_to_eof(Stream, Output).

output_from_string([], _, Previous) :-
    output_quoted_lines([Previous]).

output_from_string([ann(Start, Stop, Ann)|Format], Pos, Previous) :-
    N is Start - Pos,
    read_chars_from_string(N, Previous, Output, Remaining),
    output_verb_lines(Output),
    N2 is Stop - Start,
    read_chars_from_string(N2, Remaining, Output2, Remaining2),
    output_quoted_ann_lines(Ann, Output2),
    output_from_string(Format, Stop, Remaining2).

read_chars_from_string(0, Remaining, [], Remaining) :- !.
read_chars_from_string(_, [], [], []).
read_chars_from_string(N, Previous, [Output], Remaining) :-
        split_string(Previous, Output, Remaining, N).

get_tuples([], []).
get_tuples([Start, Stop, Ann|Anns], [ann(Start, Stop, Ann)| NAnns]) :-
    get_tuples(Anns, NAnns).

sort_highlights(Anns, NAnns) :- get_tuples(Anns, N), sort(N, N2),
								move_forward_hide_nfs(N2, NAnns).

move_forward_hide_nfs([], []).
move_forward_hide_nfs([ann(S, E1, A), ann(S, E2, hide_nf)|Anns],
					  [ann(S, E2, hide_nf), ann(S, E1, A)|NAnns]) :-
	move_forward_hide_nfs(Anns, NAnns).
move_forward_hide_nfs([A|Anns], [A|NAnns]) :-
	move_forward_hide_nfs(Anns, NAnns).
	
split_string(Orig, Prefix, Suffix, Length) :-
        length(Prefix, Length), append(Prefix, Suffix, Orig).

output_file_as_html(Filename) :-
    ann_file(Filename, Annfile),
    request_ann(Filename, Annfile, Format, Syntax, Filter, FilSyntax),
    append(Syntax, Format, UnsortedAnns),
    sort_highlights(UnsortedAnns, SortedAnns),
    sort_highlights(FilSyntax, SortedFil),
    name(Filter, FilterChars),

    output_file_as_xml_element(source, Filename, SortedAnns),

    output_string_as_xml_element(filters, FilterChars, SortedFil).


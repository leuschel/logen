:- module('prolog_to_xml_swi', [sort_highlights/2,
								output_file_as_xml_element/3,
								output_string_as_xml_element/3,
								output_xml_decl/1,
								my_html_quoted_chars/3,
								output_file_as_html_no_ann/1,
								output_file_as_html/3]).

:- use_module('matcher_swi.pl').
:- use_module('pillow/pillow.pl').

output_xml_decl(Stylesheet) :-
	output_html([xmldecl(['version="1.0"','encoding="UTF-8"']), nl,
	'<?xml-stylesheet type="text/xsl" href="', Stylesheet, '"?>', nl]).

output_string_as_xml_element(Element, String, Highlights) :-
	output_html([begin(Element), nl]),
	gen_html_from_string(Highlights, String),
	output_html([end(Element), nl]).

my_html_quoted_chars([]) --> [].
my_html_quoted_chars([C|T]) -->
        my_html_quoted_char(C),
		my_html_quoted_chars(T).

my_html_quoted_char(0'>) --> !, "&gt;".
my_html_quoted_char(0'<) --> !, "&lt;".
my_html_quoted_char(0'&) --> !, "&amp;".
my_html_quoted_char(0'") --> !, "&quot;".
my_html_quoted_char(C)   --> [C].

get_tuples([], []).
get_tuples([Start, Stop, Ann|Anns], [ann(Start, Stop, Ann)| NAnns]) :-
    get_tuples(Anns, NAnns).

sort_highlights(Anns, NAnns) :- predsort(ann_compare, Anns, NAnns).

% same ann (shouldn't ever happen)!
ann_compare(=, ann(S, E, A), ann(S, E, A)) :- !.
ann_compare(>, ann(S1, _, _), ann(S2, _, _)) :- S1 > S2, !.
ann_compare(<, ann(S1, _, _), ann(S2, _, _)) :- S1 < S2, !.
% At this point the starts must be equal
ann_compare(<, ann(_, E1, _), ann(_, E2, _)) :- E1 > E2, !.
ann_compare(>, ann(_, E1, _), ann(_, E2, _)) :- E1 < E2, !.
% starts and ends are the same
ann_compare(<, ann(_, _, hide_nf), ann(_, _, _)) :- !.
ann_compare(>, ann(_, _, _), ann(_, _, hide_nf)) :- !.
ann_compare(>, ann(_, _, atom), ann(_, _, _)) :- !.
ann_compare(<, ann(_, _, _), ann(_, _, atom)) :- !.
ann_compare(=, _, _).

output_file_as_html(Filename, Annfile, Extra) :-
	get_ann_positions(Filename, Annfile, UnsortedAnns, Extra), !,
	collect_filters(FiltStream, FilSyntax, Extra),
    sort_highlights(UnsortedAnns, SortedAnns),
    sort_highlights(FilSyntax, SortedFil),

    output_file_as_xml_element(source, Filename, SortedAnns),

    output_stream_as_xml_element(filters, FiltStream, SortedFil).


output_file_as_html_no_ann(Filename) :-
    get_syntax_positions(Filename, Syntax), !,
	sort_highlights(Syntax, SortedSyn),

	output_file_as_xml_element(source, Filename, SortedSyn).

read_bytes(_, 0, []) :- !.
read_bytes(Stream, _, []) :-
	at_end_of_stream(Stream), !.
read_bytes(Stream, rest, [B|Bs]) :-
	!, get_byte(Stream, B),
	read_bytes(Stream, rest, Bs).
read_bytes(Stream, N, [B|Bs]) :-
	get_byte(Stream, B),
	N1 is N - 1,
	read_bytes(Stream, N1, Bs).

read_bytes(Stream, S, E, Bs) :-
	N is E - S,
	read_bytes(Stream, N, Bs).

output_stream_as_xml_element(Element, Stream, Syntax) :-
	output_html([begin(Element)]),
	gen_html(Stream, 0, _, Syntax, _),
	output_html([end(Element), nl]).

output_file_as_xml_element(Element, Filename, Syntax) :-
	open(Filename, read, Stream),
	output_stream_as_xml_element(Element, Stream, Syntax),
	close(Stream).

convert_tag(';', disj) :- !.
convert_tag(A,A).

% no terms remaining
gen_html(Stream, Pos, NPos, [], []) :-
	nonvar(NPos), !,
	read_bytes(Stream, Pos, NPos, Read),
	my_html_quoted_chars(Read, Output, []),
	output_html(Output).

gen_html(Stream, _, _, [], []) :-
	!, read_bytes(Stream, rest, Read),
	add_comment_markup(Read, Output),
	output_html(Output).
		
gen_html(Stream, Pos, NPos, [ann(S,E,Tag)|Tags], [ann(S,E,Tag)|Tags]) :-
	nonvar(NPos), NPos =< S, !,
	read_bytes(Stream, Pos, NPos, Read),
	my_html_quoted_chars(Read, Output, []),
	output_html(Output).

gen_html(Stream, Pos, NPos, [ann(S, E, atom)|Tags], NTags) :-
	!, read_bytes(Stream, Pos, S, Read),
	add_comment_markup(Read, Out1),
	output_html(Out1),
	gen_html(Stream, S, E, Tags, NTags2),
	gen_html(Stream, E, NPos, NTags2, NTags).

gen_html(Stream, Pos, NPos, [ann(S, E, Tag)|Tags], NTags) :-
	!, read_bytes(Stream, Pos, S, Read),
	add_comment_markup(Read, Out1),
	output_html(Out1),
	enclosing_tag(Tag, TagName),
	gen_html(Stream, S, E, Tags, NTags2),
	output_html([end(TagName)]),
	gen_html(Stream, E, NPos, NTags2, NTags).

enclosing_tag((Ann+Fix)/Arity, Tag) :-
	convert_tag(Ann, Tag),
	output_html([begin(Tag, [arity=Arity, fix=Fix])]).
enclosing_tag(Ann+Fix, Tag) :-
	convert_tag(Ann, Tag),
	output_html([begin(Tag, [fix=Fix])]).
enclosing_tag(Ann/Arity, Tag) :-
	convert_tag(Ann, Tag),
	output_html([begin(Tag, [arity=Arity])]).
enclosing_tag(Ann*Replacement, Tag) :-
	convert_tag(Ann, Tag),
	output_html([begin(Tag)]),
	write_replace_block(Replacement).
enclosing_tag(Ann, Tag) :-
	convert_tag(Ann, Tag),
	output_html([begin(Tag)]).

% This will create a
% <replacement>:- <directive>filter</directive>
% <filter>name</filter>(<dynamic>dynamic</dynamic>,...)</replacement>
% block from a parsed filter declaration. This will then go inside the
% badfilter block and the xslt will generate the code to replace the existing
% filter when fix is clicked.
write_replace_block(Replace) :-
	collect_filters_from_list(FiltStream, FilSyntax, [], 
			[ann_decl(filter, Replace)]),
	%the first element will be a wholefilter tag. this needs removing
    sort_highlights(FilSyntax, [_WholeFilter|SortedFil]),
	output_html([begin(replacement)]),
	gen_html(FiltStream, 0, _, SortedFil, _),
	output_html([end(replacement)]).

add_comment_markup([], []).
add_comment_markup([0'%|T], [comm([0'%|T1])|T2]) :-
	consume_till_line_end(T, T1, TR),
	add_comment_markup(TR, T2).
add_comment_markup([0'/,0'*|T], [comm([0'/,0'*|T1])|T2]) :-
	consume_till_comment_end(T, T1, TR),
	add_comment_markup(TR, T2).
add_comment_markup([H|T], NT) :-
	add_quoted_char(H, T2, NT),
	add_comment_markup(T, T2).

consume_till_line_end([10|T], [10], T).
consume_till_line_end([H|T], NT, TR) :-
	add_quoted_char(H, T2, NT),
	consume_till_line_end(T, T2, TR).

consume_till_comment_end([0'*,0'/|T], [0'*,0'/], T).
consume_till_comment_end([H|T], NT, TR) :-
	add_quoted_char(H, T2, NT),
	consume_till_comment_end(T, T2, TR).

add_quoted_char(0'&, L, [0'&, 0'a, 0'm, 0'p, 0';|L]).
add_quoted_char(0'<, L, [0'&, 0'l, 0't, 0';|L]).
add_quoted_char(0'>, L, [0'&, 0'g, 0't, 0';|L]).
add_quoted_char(0'", L, [0'&, 0'q, 0'u, 0'o, 0't, 0';|L]).
add_quoted_char(X, L, [X|L]).

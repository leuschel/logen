% This program is responsible for collects the information used to construct
% the menus using weblogen. It should be run every time new annotations,
% or simple filter types are added. The backend make file builds and runs the
% resulting executable automatically.

:- module(collect_anns, [main/1]).

:- use_module('../../annmeta', [get_simple_anns/1, higher_order_gui_info/3]).
:- use_module('../../gximports/generalise_and_filter.pl').
:- use_module(library(terms)).

test(X) :- get_simple_anns(X).

main([JSFile, CommandFile]) :-
	open(JSFile, write, Stream),
	get_simple_anns(Simple),
	add_to_menu_map(Stream, 'ann_menu_map', [unknown, unsafe], Simple, '', S0),
	add_higher_order_menus(Stream, 'ann_menu_map', S0, S3),

	findall(A, basic_binding_type(A), FilterTypeList),

	add_to_menu_map(Stream, 'filt_menu_map', [complex, unsafe], FilterTypeList, '', F),

	close(Stream),
	
	open(CommandFile, write, CStream),
	format(CStream, 'sed s/%ANNOTATIONS%/~w/ prologtohtml.xsl.template > prologtohtml.xsl~nsed s/%FILTERS%/~w/ filterstohtml.xsl.template > filterstohtml.xsl~n', [S3, F]),
	close(CStream).

add_higher_order_menus(Stream, MapVar, InString, OutString) :-
	findall(Id, (higher_order_gui_info(Id, A, _), nonvar(A)), Ids),
	add_higher_order_menus2(Stream, MapVar, Ids, InString, OutString).

add_higher_order_menus2(_, _, [], In, In).
add_higher_order_menus2(Stream, MapVar, [Id|Ids], In, Out) :-
	higher_order_gui_info(Id, Unknown, Anns),
	add_to_menu_map(Stream, MapVar, [Unknown], Anns, In, Out1),
	add_higher_order_menus2(Stream, MapVar, Ids, Out1, Out).

add_to_menu_map(Stream, Menu, Unknown, Anns, OldAnnString, NewAnnString) :-
	add_quotes(Anns, NAnns),
	add_quotes(Unknown, QUnknown),
	format(Stream, 'addToMenuMap(~w, ~w, ~w);~n', [Menu, QUnknown, NAnns]),
	append(Unknown, Anns, Anns2),
	add_to_string(Anns2, OldAnnString, NewAnnString).

add_to_string([], A, A).
add_to_string([A|As], '', N) :-
	add_to_string(As, A, N).
add_to_string([A|As], Str, NStr) :-
	atom_concat(Str, '\\|', Str2), atom_concat(Str2, A, Str3),
	add_to_string(As, Str3, NStr).

add_quotes([], []).
add_quotes([A|As], [B|Bs]) :-
	atom_concat(['\'', A, '\''], B),
	add_quotes(As, Bs).


%:- use_module('../../../old_logen/annotation/parser.pl').
:- use_module('../../../old_logen/annfile.pl').
%:- use_module('../../../old_logen/annotation/match_ann.pl').
:- use_module('../../../old_logen/run_bta.pl').
:- use_module('../../../old_logen/bta.pl').
:- use_module(library(pillow)).
:- use_module('sicstus_prolog_to_xml.pl').
:- use_module(library(charsio)). %:- use_module(library(codesio)). in SICS4
:- use_module(library(lists)).

ann_file(F, N) :- name(F, Chars), append(Chars, ".ann", NC), name(N, NC).

main.

bta :-
	read_line(FileString), name(Filename, FileString),
	read_line(NormString), name(Norm, NormString),
	on_exception(E, (
		ann_file(Filename, Annfile),
		bta:annotateFile(Filename, Annfile, 'auto_bta'),
		bta(Filename, [output(Annfile), run_auto_bta, silent, norm(Norm)])
	),
		(bta_error(E), !, halt(1))).

filt_prop :-
	read_line(FileString), name(Filename, FileString),
	on_exception(E, (
		ann_file(Filename, Annfile),
		annotateFile(Filename, Annfile, 'auto_bta'),
		bta(Filename, [output(Annfile), filter_prop, silent, input_ann(Annfile)])
	),
		(bta_error(E), !, fail)).

bta_error(E) :-
	open('error.xml', write, Stream),
	current_output(OldStream),
	set_output(Stream),
	output_xml_decl('error.xsl'),
	write('<!DOCTYPE holdspace [<!ENTITY nbsp "&#160;">]>'), nl,
	output_html([begin(article), nl, begin(error),
		title('BTA Error'), begin(details)]),
	describe_error(E),
	output_html([end(details), end(error), end(article)]),
	set_output(OldStream),
	close(Stream),
	format(user_error, '~q', E).

describe_error(syntax_error(_, _, _, _, _)) :-
	output_html(["An error occurred during parsing. This was likely caused ",
				 "by an improperly specified initial filter. Please check ",
				 "the syntax and retry."]).

describe_error(E) :-
	format_to_chars('~q', [E], EChars), % format_to_codes in SICS4
	output_html(["An unexpected error occurred. ",
				 "The following exception was thrown : ", \\, nl,
				 begin(code), verbatim(EChars), end(code) , \\, nl,
				 "Please report this error along with the above error to the ",
				 "System administrator."]).

:- save_program(bta, main), halt.

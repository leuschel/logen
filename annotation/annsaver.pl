:- module('annsaver', [save_annfile/1, save_annfile_from_list/4, write_filters/2]).

:- use_module('../annmeta.pl').
:- use_module('../annloader.pl').

% Saves the current annotations to a file.
save_annfile(Annfile) :-
	open(Annfile, write, Out),
	write_predicate_anns(Out),
	write_ann_decls(Out),
	close(Out).

write_predicate_anns(Stream) :-
	ann_clause(I, H, AB),
	portray_clause(Stream, (logen(I, H) :- AB)),
	fail.
write_predicate_anns(_).

write_ann_decls(Stream) :-
	ann_decl(Id, Decl),
	write_decl(Stream, Id, Decl),
	fail.
write_ann_decls(_).

write_decl(Stream, module, M) :-
	!, format(Stream, '(:-module(~w, [])).~n', [M]).
write_decl(Stream, Id, Decl) :-
    format(Stream, ':- ~w~n	~w.~n', [Id, Decl]).

% Plfile is the prolog file to read in and match the annotations to
% Annfile is the file to be created
% Anns is a list of annotations e.g. [unfold, rescall, memo, memo]
% Filters is a string containing the annotations
save_annfile_from_list(Plfile, Annfile, Anns, Filters) :-
	open(Plfile, read, In),
	open(Annfile, write, Out),
	read_clauses_and_annotate(In, Out, Anns),
	write_filters(Out, Filters),
	close(In),
	close(Out).

write_filters(_, []).
write_filters(Out, [F|Filters]) :-
	portray_clause(Out, F),
	write_filters(Out, Filters).

read_clauses_and_annotate(In, Out, Anns) :-
	read(In, Clause),
	(Clause == end_of_file ->
		true
	;
		(
		match_clause(Clause, AnnClause, Anns, NewAnns),
		portray_clause(Out, AnnClause),
		read_clauses_and_annotate(In, Out, NewAnns)
		)
	).

match_clause(:-(H, B), (logen(Head, H) :- Clauses), Anns, NewAnns) :-
	H =.. [Head|_],
	match_clauses(B, Clauses, Anns, NewAnns).
match_clause(H, logen(Head, H), Anns, Anns) :-
	H =.. [Head|_].

match_clauses(','(F,R), ','(logen(Ann, F), NR), [Ann|Anns], NewAnns) :-
	match_clauses(R, NR, Anns, NewAnns).
match_clauses(F, logen(Ann, F), [Ann|Anns], Anns).

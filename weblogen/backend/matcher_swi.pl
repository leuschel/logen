:- module(matcher_swi, [get_syntax_positions/2,
						get_ann_positions/4,
						get_pos_parse_tree/3,
						collect_filters/3,
						collect_filters_from_list/4,
						test/1]).

:- include(op_decl_swi).
:- use_module('../../annloader').
:- use_module('../../annmeta').
:- use_module('../../gximports/generalise_and_filter').
:- use_module(prolog_to_xml_swi).
:- use_module(library(memfile)).

:- dynamic correspond_ann_clause/2.
:- dynamic filter_fix/3.


test(Anns) :- get_ann_positions('match.pl', 'match.pl.ann', Anns).

get_ann_positions(Plfile, Annfile, Anns, Extras) :-
	load_annfile(Annfile),
	process_extras(Extras),
	assert_ann_clauses,
	open(Plfile, read, Stream),
	match_anns(Stream, Anns),
	close(Stream).

process_extras([]).
process_extras([E|Es]) :- process_extra(E), process_extras(Es).

get_unsafe_clause(Pred, Arity, ClauseNo, Clause) :-
	integer(Arity),
	integer(ClauseNo),
	findall(ann_clause(Id, H, A), (ann_clause(Id, H, A), functor(H, Pred, Arity)), Clauses),
	nth1(ClauseNo, Clauses, Clause).

% find the annotated clause Pred/Arity (number ClauseNo) and change the
% annotation pointed to by Path to Ann
process_extra(extra(Ann, Pred, Arity, ClauseNo, Path)) :-
	get_unsafe_clause(Pred, Arity, ClauseNo, ann_clause(P1, H1, Clause)),
	follow_path_and_change(Clause, Path, Ann, NClause, _),
	retract(ann_clause(P1, H1, Clause)),
	assert(ann_clause(P1, H1, NClause)).
process_extra(fixable(Ann, Pred, Arity, ClauseNo, Path, Fix)) :-
	get_unsafe_clause(Pred, Arity, ClauseNo, ann_clause(P1, H1, Clause)),
	follow_path_and_change(Clause, Path, Ann+Fix, NClause, _),
	retract(ann_clause(P1, H1, Clause)),
	assert(ann_clause(P1, H1, NClause)).
process_extra(fix_hide_nf(Pred, Arity, ClauseNo, Path)) :-
	get_unsafe_clause(Pred, Arity, ClauseNo, ann_clause(P1, H1, Clause)),
	follow_path_and_add(Clause, Path, fix_hide_nf, NClause),
	retract(ann_clause(P1, H1, Clause)),
	assert(ann_clause(P1, H1, NClause)).
process_extra(fixable_filter(Pred, Arity, Arg)) :-
	assert(filter_fix(Pred/Arity, Arg, 'dynamic')).

% should perhaps throw an exception
process_extra(extra(_, P, N, _)) :- format(user_error, 'Clause ~w number ~w does not exist~n.', [P, N]).
% other extras are not relevant here, so ignore
process_extra(_).

get_index([A1, A2, A3], 1, A1, [B1, A2, A3], B1).
get_index([A1, A2, A3], 2, A2, [A1, B2, A3], B2).
get_index([A1, A2, A3], 3, A3, [A1, A2, B3], B3).
get_index([A1, A2], 1, A1, [B1, A2], B1).
get_index([A1, A2], 2, A2, [A1, B2], B2).
get_index([A1], 1, A1, [B1], B1).

% This clause matches when Fix is _, which is useful when the fix is to revert
% the clause back to its old state, when the real change happens elsewhere

%final parameter passes back the part that got changed (actually a copy)
follow_path_and_change(logen(OldAnn, A), [], Ann+Fix, logen(Ann+OldAnn, A),
	logen(Ann+OldAnn, A)) :-
	var(Fix).
follow_path_and_change(logen(_, A), [], Ann, logen(Ann, A), logen(Ann, A)).
follow_path_and_change(HO, [(H,I)|Path], Ann, NHO, Changed) :-
	annmeta(load, HO, NHO, Args, NArgs),
	functor(HO, H, _),
	get_index(Args, I, Arg, NArgs, NArg),
	follow_path_and_change(Arg, Path, Ann, NArg, Changed).

% follow the path and then wrap the result in Fix. Used to put hide_nf around
% something.
follow_path_and_add(A, [], Fix, NA) :-
	NA =.. [Fix, A].
follow_path_and_add(HO, [(H,I)|Path], Fix, NHO) :-
	annmeta(load, HO, NHO, Args, NArgs),
	functor(HO, H, _),
	get_index(Args, I, Arg, NArgs, NArg),
	follow_path_and_add(Arg, Path, Fix, NArg).

assert_ann_clauses :-
	ann_clause(_, H, AB),
	convert_ann_to_source(H, AB, T),
	assert(correspond_ann_clause(T, (H:-AB))),
	fail.
	
assert_ann_clauses.

convert_ann_to_source(H, true, H) :- !.
convert_ann_to_source(H, B, (H:-T)) :- 
	strip_ann(B, T), !.

strip_ann(logen(_, A), _) :-
	var(A), !,
	throw('Free variable annotated directly. Use call(Var) instead.').

strip_ann(logen(_,(_,_)), _) :-
	!,
	throw('Conjunction annotated as if it was a simple predicate. To fix convert annotations of form logen(call, (_,_)) to logen(call, _), logen(call, _).').

strip_ann(logen(_, A), A).
strip_ann(fix_hide_nf(B), S) :-
	strip_ann(B, S).
strip_ann(A, S) :-
	annotation_matcher_data(A, S, MapFrom, MapTo),
	strip_ann_list(MapFrom, MapTo).

strip_ann_list([], []).
strip_ann_list([A|As], [S|Ss]) :-
	strip_ann(A, S),
	strip_ann_list(As, Ss).

get_pos_parse_tree(Filename, T, PT) :-
	open(Filename, read, Stream),
	positional_parse(Stream, T, PT),
	close(Stream).

positional_parse(Stream, Ts, PTs) :-
	read_term(Stream, T, [subterm_positions(P)]),
	(T == end_of_file ->
		(Ts = [], PTs = [])
	;
		(
		  positional_parse(Stream, T2, PT2), !,
		  Ts = [T|T2], PTs = [P|PT2]
		)
	).

get_filter_highlights(Stream, Highlights, Extras) :-
	read_term(Stream, T, [subterm_positions(P)]),
	(T == end_of_file ->
		Highlights = []
	;
		(
		  get_filter_highlights(Stream, H2, Extras), !,
		  get_highlights_from_tree(T, P, H, Extras),
		  append(H, H2, Highlights)
		)
	).

get_functor_pos_and_args(term_position(_,_,S,E,PArgs),S,E,PArgs).
get_functor_pos_and_args(S-E,S,E,[]).

% highlights bad filters with no fixes
get_highlights_from_tree(:-(filter(Filt)),
		term_position(S1,E1,_,_,[term_position(_,_,S2,E2,[T])]),
		[ann(S1,E1plus2,wholefilter), ann(S1, E1, badfilter),
		 ann(S2,E2,directive/Arity), ann(S,E,(filter)/Arity)|Highlights],
		Extras) :-
	functor(Filt, Name, Arity),
	member(filter(Name,Arity), Extras),
	!, E1plus2 is E1 + 2,get_functor_pos_and_args(T,S,E,PArgs),
	Filt =.. [_|Args],
	get_filter_args(Args, PArgs, Highlights, Name, Arity).

% highlights bad filters with fixes
get_highlights_from_tree(:-(filter(Filt)),
		term_position(S1,E1,_,_,[term_position(_,_,S2,E2,[T])]),
		[ann(S1,E1plus2,wholefilter), ann(S1, E1, badfilter*Replacement),
		 ann(S2,E2,directive/Arity), ann(S,E,(filter)/Arity)|Highlights],
		Extras) :-
	functor(Filt, Name, Arity),
	member(replace_filter(Name,Arity,Replacement), Extras),
	!, E1plus2 is E1 + 2,get_functor_pos_and_args(T,S,E,PArgs),
	Filt =.. [_|Args],
	get_filter_args(Args, PArgs, Highlights, Name, Arity).

get_highlights_from_tree(:-(filter(Filt)),
		term_position(S1,E1,_,_,[term_position(_,_,S2,E2,[T])]),
		[ann(S1,E1plus2,wholefilter), ann(S2,E2,directive/Arity),
		 ann(S,E,(filter)/Arity)|Highlights], _) :-
% S1 - E1 includes the whole term except for the '.\n', so since we know how it
% was printed we can just cheat to include it
	!, E1plus2 is E1 + 2,get_functor_pos_and_args(T,S,E,PArgs),
	Filt =.. [_|Args],
	length(PArgs, Arity),
	functor(Filt, Name, Arity),
	get_filter_args(Args, PArgs, Highlights, Name, Arity).
get_highlights_from_tree(_, _, [], _).

get_filter_args(Args, PArgs, Highlights, Name, Arity) :-
	%(filter_fix(Name/Arity, FixArg, FixType) -> true ; FixArg = -1),
	get_filter_args2(Args, PArgs, Highlights, 1, Name/Arity).

get_filter_args2([], [], [], _, _).
get_filter_args2([A|As], [S-E|Ps], [ann(S, E, A2)|Hs], Arg, Pred) :-
	get_filter_arg_highlight(A), !, Arg2 is Arg + 1,
	(filter_fix(Pred, Arg, FixType) -> A2 = unsafe+FixType ; A2 = A),
	get_filter_args2(As, Ps, Hs, Arg2, Pred).
get_filter_args2([_|As], [term_position(S,E,_,_,_)|Ps],
				[ann(S, E, A2)|Hs], Arg, Pred) :-
	(filter_fix(Pred, Arg, FixType) -> A2 = unsafe+FixType ; A2 = complex),
	Arg2 is Arg + 1, get_filter_args2(As, Ps, Hs, Arg2, Pred).

get_filter_arg_highlight(X) :- basic_binding_type(X).

get_syntax_positions(Filename, Syntax) :-
	open(Filename, read, Stream),
	get_syntax_read(Stream, Syntax),
	close(Stream).

get_syntax_read(Stream, Syntax) :-
	read_term(Stream, T, [subterm_positions(P)]),
	(T == end_of_file ->
		Syntax = []
	;
		(
		  get_syntax_read(Stream, Syn2), !,
		  get_syntax_from_tree(T, P, Syn),
		  append(Syn, Syn2, Syntax)
		)
	).

get_functor_pos(S-E, S, E).
get_functor_pos(term_position(_, _, S, E, _), S, E).

get_list_pos_head(S-E, [ann(S, E, head/0)]).
get_list_pos_head(term_position(_, _, S, E, Args), [ann(S, E, head/Arity)|ListPos]) :-
	!, length(Args, Arity), get_list_pos_from_list(Args, ListPos).
get_list_pos_head(T, L) :- get_list_pos(T, L).

get_list_pos(S-E, [ann(S, E, atom)]).
%get_list_pos(_-_, []).
get_list_pos(string_position(S,E), [ann(S, E, string)]).
% the following highlights like this: _{_ .... _}_
% perhaps it should do _{ ... }_
get_list_pos(brace_term_position(S,E,Arg), [ann(S, S1, brace)|List]) :-
	get_list_pos(Arg, L),
	S1 is S + 1, Em1 is E - 1,
	append(L, [ann(Em1, E, brace)], List).
get_list_pos(list_position(Begin, End, Args, _),
			 [ann(Begin, Begin1, list)|ListPos]) :-
	get_list_pos_from_list(Args, List),
	Begin1 is Begin + 1, Endm1 is End - 1,
	append(List, [ann(Endm1, End, list)], ListPos).
get_list_pos(term_position(_, _, S, E, Args), [ann(S, E, atom)|ListPos]) :-
	get_list_pos_from_list(Args, ListPos).

get_list_pos_from_list([], []).
get_list_pos_from_list([H|T], ListPos) :-
	get_list_pos(H, PH),
	get_list_pos_from_list(T, PT),
	append(PH, PT, ListPos).

get_syntax_from_tree(:-(_,_), term_position(_,_,_,_,[F, R]), ListPos) :-
	!,
	get_list_pos_head(F, List1),
	get_list_pos(R, List2),
	append(List1, List2, ListPos).

get_syntax_from_tree(_, term_position(S,_,S,E,Args), [ann(S, E, head/Arity)|ListPos]) :-
	!, get_list_pos_from_list(Args, ListPos), length(Args, Arity).
get_syntax_from_tree(_, S-E, [ann(S, E, head/0)]) :- !.

	
get_syntax_from_tree(_, L, ListPos) :- get_list_pos(L, ListPos).

canon(A, In, Out) :- nonvar(A), !, canon2(A, In, Out).
canon(A, In, Out) :- conjunct(A, In, Out).

canon2(A, In, Out) :-
    A = (B,C), !,
    canon(C, In, R),
    canon(B, R, Out).
canon2(A, In, Out) :-
    A =.. [F|Args],
    canon_list(Args, NArgs),
    B =.. [F|NArgs],
    conjunct(B, In, Out).

canon_list([], []).
	canon_list([A|As], [B|Bs]) :-
	canon(A, empty, nonempty(B)),
	canon_list(As, Bs).

conjunct(A, empty, nonempty(A)).
conjunct(A, nonempty(B), nonempty((A, B))).

match_anns(Stream, Anns) :-
	read_term(Stream, T, [subterm_positions(P)]),
	(T == end_of_file ->
		Anns = []
	;
		(
		  get_corresponding_anns(T, P, NAnns),
		  match_anns(Stream, Anns2), !,
		  get_syntax_from_tree(T, P, Syn),
		  append(Syn, Anns2, Anns3),
		  append(NAnns, Anns3, Anns)
		)
	).

% matches directly
get_corresponding_anns(T, P, Anns) :-
	correspond_ann_clause(T, AnnClause), !,
	get_anns_from_clause(P, AnnClause, Anns).
% try fuzzy match to get ones hidden by hide_nf conjunction reordering
get_corresponding_anns(T, P, Anns) :-
	fuzzy_match(T, P, NP, AnnClause),
	get_anns_from_clause(NP, AnnClause, Anns).
% match fails -> mark with unknowns
get_corresponding_anns(T, P, Anns) :-
	annotate_unknown_clause(T, P, Anns).

annotate_unknown_clause((:-_), _, []).
annotate_unknown_clause((_:-T), term_position(_, _, _, _, [_, PT]), Anns) :-
	annotate_unknown(T, PT, Anns).
annotate_unknown_clause(_, _, []).

annotate_unknown(T, P, Anns) :-
	term_to_position(T, P, Pos, TMap, PMap, Unknown),
	(Pos = [S,E] -> Anns = [ann(S,E,Unknown)|NAnns] ; Anns = NAnns),
	annotate_unknown_list(TMap, PMap, NAnns).

annotate_unknown_list([], [], []).
annotate_unknown_list([T|Ts], [P|Ps], Anns) :-
	annotate_unknown(T, P, A),
	annotate_unknown_list(Ts, Ps, As),
	append(A, As, Anns).

get_anns_from_clause(term_position(_, _, _, _, Args), (_:-B), NAnns) :-
	!, get_anns_from_body(Args, B, NAnns).

get_anns_from_clause(_P, _AnnClause, NAnns) :-
	%portray_clause(P),
	%portray_clause(AnnClause),
	NAnns = [].
	
get_anns_from_body(_, true, []) :- !.
get_anns_from_body([_,A], B, Anns) :- !, 
	%portray_clause(A),
	%portray_clause(B),
	get_anns_from_body2(A, B, Anns).

get_anns_from_body2(P, fix_hide_nf(A), [ann(S,E,fix_hide_nf)|Anns2]) :-
	ann_to_position(hide_nf(A), P, As, Ps, _, [S,E], _),
	get_anns_from_body2_list(Ps, As, Anns2).
get_anns_from_body2(P, AnnClause, Anns) :-
	ann_to_position(AnnClause, P, As, Ps, Ann, Pos, Arity),
	(Pos = [S,E] -> Anns = [ann(S,E,Ann/Arity)|Anns2] ; Anns = Anns2),
	get_anns_from_body2_list(Ps, As, Anns2).

get_anns_from_body2_list([], [], []).
get_anns_from_body2_list([P|Ps], [A|As], Anns) :-
	get_anns_from_body2(P, A, Anns1),
	get_anns_from_body2_list(Ps, As, Anns2),
	append(Anns1, Anns2, Anns).


% arg 1 is an open stream containing the filters
% (likely to be a memory file although it could be something else)
% arg 2 is the formatting for it
% arg 3 contains info for extra highlighting
collect_filters(ReturnStream, FilSyntax, Extras) :-
	new_memory_file(TmpFile),
    open_memory_file(TmpFile, write, TmpOut),
    write_filters(TmpOut),
    close(TmpOut),
	open_memory_file(TmpFile, read, TmpIn),
	get_filter_highlights(TmpIn, FilSyntax, Extras),
	close(TmpIn),
	open_memory_file(TmpFile, read, ReturnStream).

% this is the same as above but the filters are contained in a list (Filters)
% and aren't extracted from the ann_decl db.
collect_filters_from_list(ReturnStream, FilSyntax, Extras, Filters) :-
	new_memory_file(TmpFile),
    open_memory_file(TmpFile, write, TmpOut),
    write_filters_from_list(TmpOut, Filters),
    close(TmpOut),
	open_memory_file(TmpFile, read, TmpIn),
	get_filter_highlights(TmpIn, FilSyntax, Extras),
	close(TmpIn),
	open_memory_file(TmpFile, read, ReturnStream).


list([]).
list([_|As]) :- list(As).

prec(X, 0) :- var(X), !.
prec(type(_), 0) :- !.
prec(X, 0) :- atomic(X), !.
prec(X, 0) :- list(X), !.
prec(X, P) :- functor(X, Op, _), current_op(P, _, Op), !.
prec(_, 0).

prefix(fx).
prefix(fy).
args(fx, [x]).
args(fy, [y]).
args(xf, [x]).
args(yf, [y]).
args(xfx, [x,x]).
args(xfy, [x,y]).
args(yfx, [y,x]).
args(yfy, [y,y]).

% bracket if necessary
bpp(S, X, P1, P2, x) :- P2 < P1, !, pp(S, X).
bpp(S, X, P1, P2, y) :- P2 = P1, !, pp(S, X).
bpp(S, X, _, _, _) :- print(S, '('), pp(S, X), print(S, ')').

% have to do all the work here because SWI portrays dynamic as (dynamic)!
% this is incomplete! It will probably output the wrong thing if you use
% operators that require inner brackets to maintain precedence
% OTH it appears to work with the examples I have...
pp(S, X) :- var(X),!, print(S, X).
pp(S, X) :- atomic(X),!, writeq(S, X).
pp(S, X) :- list(X),!, print(S, '['), l_pp(S, X), print(S, ']').

%pp(S, '--->'(X, Def) ) :-
%	!, pp(S, X), print(S, ' --> '), pp(S, Def).
% don't print type as an operator here even though it is.
pp(S, type(X)) :- !, print(S,'type('), pp(S, X), print(S, ')').
pp(S, X) :- X=..[Op,Arg], current_op(P1, Type, Op), args(Type, [T]), !,  prec(Arg, P2),
			(prefix(Type) ->
				print(S, Op), bpp(S, Arg, P1, P2, T)
			;	bpp(S, Arg, P1, P2, T), print(S, Op)).
pp(S, X) :- X=..[Op,A1,A2], current_op(P, Type, Op), args(Type, [T1, T2]), !, prec(A1, P1), prec(A2, P2),
			bpp(S, A1, P, P1, T1), print(S, ' '), print(S, Op), print(S, ' '), bpp(S, A2, P, P2, T2).

pp(S, X) :- X=..[Op|Args], writeq(S, Op), print(S, '('), l_pp(S, Args), print(S, ')').

l_pp(_, []).
l_pp(S, [H]) :- !,pp(S, H).
l_pp(S, [H|T]) :- pp(S, H), print(S, ','), l_pp(S, T).

write_decl(_, module, _) :- !.
write_decl(Stream, Id, Decl) :-
    format(Stream, ':- ~w~n    ', [Id]), pp(Stream, Decl),
	write(Stream, '.'), nl(Stream).

write_filters(Stream) :-
    ann_decl(Id, Decl),
    display_in_filters(Id),
    write_decl(Stream, Id, Decl),
    fail.
write_filters(_).

write_filters_from_list(_, []).
write_filters_from_list(Stream, [ann_decl(Id, Decl)|Fs]) :-
    display_in_filters(Id),
    write_decl(Stream, Id, Decl),
    write_filters_from_list(Stream, Fs).

fuzzy_match((H:-B), term_position(S, E, FS, FE, [PF, PB]),
					term_position(S, E, FS, FE, [PF, NPB]), AnnClause) :-
	functor(B, Name, Arity),
	functor(FB, Name, Arity), !,
	correspond_ann_clause((H:-FB), (H:-AnnClause)),
	try_match(B, FB, PB, NPB, AnnClause), !.
	
%try_match(Body, FBody, PBody, NPBody, AnnClause).
try_match(B, B, PB, PB, logen(_,B)).
try_match((C1, C2), FB, PB, NPB, AnnClause) :-
	match_conj((C1, C2), FB, PB, NPB, AnnClause).
try_match((A->B;C), (FA->FB;FC), PB, NPB, AnnClause) :-
	match_if((A,B,C), (FA,FB,FC), PB, NPB, AnnClause).
try_match(A, FA, PB, NPB, hide_nf(AnnClause)) :-
	try_match(A, FA, PB, NPB, AnnClause).
try_match(A, FA, PB, NPB, fix_hide_nf(AnnClause)) :-
	try_match(A, FA, PB, NPB, AnnClause).

extract_if(if(A,B,C), A, B, C).
extract_if(resif(A,B,C), A, B, C).

match_if((A,B,C), (FA, FB, FC),
	 term_position(S, E, CS, CE, [term_position(S1,E1,CS1,CE1,[P1,P2]),P3]),
	 term_position(S, E, CS, CE, [term_position(S1,E1,CS1,CE1,[NP1,NP2]),NP3]),
	 AnnClause) :-
	!, extract_if(AnnClause, A1, A2, A3),
	try_match(A, FA, P1, NP1, A1),
	try_match(B, FB, P2, NP2, A2),
	try_match(C, FC, P3, NP3, A3).

match_conj(C1, FC, PB, NPB, (hide_nf((A,B)), C)) :-
	!, count_conjs((A,B), Count),
	reorder(C1, Count, RC1, RC2, PB, term_position(S, E, CS, CE, [P1, P2])),
	try_match(RC1, FC1, P1, NP1, (A,B)),
	try_match(RC2, FC2, P2, NP2, C),
	NPB = term_position(S, E, CS, CE, [NP1, NP2]),
	FC = (FC1, FC2).
% exact copy of above one but hide_nf -> fix_hide_nf
match_conj(C1, FC, PB, NPB, (fix_hide_nf((A,B)), C)) :-
	!, count_conjs((A,B), Count),
	reorder(C1, Count, RC1, RC2, PB, term_position(S, E, CS, CE, [P1, P2])),
	try_match(RC1, FC1, P1, NP1, (A,B)),
	try_match(RC2, FC2, P2, NP2, C),
	NPB = term_position(S, E, CS, CE, [NP1, NP2]),
	FC = (FC1, FC2).

match_conj((C1, C2), (FC1, FC2), term_position(S,E,CS,CE,[P1,P2]),
								 term_position(S,E,CS,CE,[NP1, NP2]),
							(A1, A2)) :-
	try_match(C1, FC1, P1, NP1, A1),
	try_match(C2, FC2, P2, NP2, A2).
	
	
% takes code of form (a1, a2, ...an, b) and turns it into ((a1, a2, ...an), b)
% also changes the term_position information
reorder(Conj, 0, A, B, PB, PB) :-
	nonvar(Conj), Conj = (A,B).

reorder(Conj, X, (A, C), D, PB, NPB) :-
	nonvar(Conj), Conj = (A,B), X1 is X - 1, 
	PB = term_position(S, E, CS, CE,
	                [P1, term_position(S1, E1, CS1, CE1, [P2, P3])]),
	reorder(B, X1, C, D, P3, NP3),
	NPB = term_position(S, E, CS1, CE1,
	                [term_position(S1, E1, CS, CE, [P1, P2]), NP3]).

% Assume first arg is always nonvar
count_conjs((A,B), Count) :-
	!, count_conjs(A, C1), count_conjs(B, C2),
	Count is 1 + C1 + C2.
count_conjs(hide_nf(A), Count) :-
	!, count_conjs(A, Count).
count_conjs(fix_hide_nf(A), Count) :-
	!, count_conjs(A, Count).
count_conjs(_, 0).


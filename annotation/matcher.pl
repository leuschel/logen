:- module('matcher', [get_annotation_positions/6, get_syntax_positions/2, fix_hidenfs/5, pos_ann_clause/2, assert_pos_ann_clauses/0]).

:- use_module('../annmeta.pl').
:- use_module('../annloader.pl').
:- use_module('parser.pl').
:- use_module('../bta/simple_bta.pl').
:- use_module('../tools/error_manager.pl').
:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module('../runtime_checks.pl').

:- dynamic pos_ann_clause/2.

get_annotation_positions(Filename, Annfile, NewAnns, Syntax, Filters, FilSyntax) :- 
	load_annfile(Annfile),
	assert_pos_ann_clauses,
	see(Filename),
	match_pos_clauses(AnnList, TempSyntax),
	make_comment_tags(TempSyntax, Syntax),
	seen,
	add_end_pos(AnnList, AnnList2),
	fix_hidenfs(AnnList2, NewAnns, _, [], _),
	collect_filters(Filters, FilSyntax).

fix_hidenfs([], [], _, [], _).

fix_hidenfs([0,0,hide_nf|List], [Start,End,hide_nf|NList], _, L, Start) :-
	!, fix_hidenfs(List, NList, _, [End|L], Start).

fix_hidenfs([0,0,hide_nf_end|List], NList, End, [End|L], _) :-
	!, fix_hidenfs(List, NList, End, L, _).

fix_hidenfs([Start,End,Ann|List], [Start,End,Ann|NList], _, L, Start) :-
	fix_hidenfs(List, NList, End, L, _).

get_syntax_positions(Filename, Syntax) :-
	see(Filename),
	get_syntax_read(TempSyntax),
	make_comment_tags(TempSyntax, Syntax),
	seen.

collect_filters(Filters, FilSyntax) :-
	mktemp('/tmp/filterXXXXXX', TmpFile),
	open(TmpFile, write, TmpOut),
	write_filters(TmpOut),
	close(TmpOut),
	file_to_string(TmpFile, Filters),
	see(TmpFile),
	get_syntax_read(TempFilSyntax),
	seen,
	%format(user_error, 'Deleting ~w~n', [TmpFile]),
	delete_file(TmpFile), !, % cut to stop it failing and trying to delete file
							 % twice.
	make_comment_tags(TempFilSyntax, FilSyntax).
	
write_decl(_, module, _) :- !.
write_decl(Stream, Id, Decl) :-
	format(Stream, ':- ~w~n	~w.~n', [Id, Decl]).

write_filters(Stream) :-
	ann_decl(Id, Decl),
	display_in_filters(Id),
	write_decl(Stream, Id, Decl),
	fail.
write_filters(_).

get_end_pos(Start, Atom, End) :-
    atom_length(Atom, L),
	End is Start + L.

make_comment_tags([], []).
make_comment_tags([list(X,_Y)|Xs], [X, NewY, list |Ys]) :-
	NewY is X + 1,
	make_comment_tags(Xs,Ys).
make_comment_tags([PosTag|Xs], [X, Y, Tag |Ys]) :-
	PosTag =.. [Tag, X, Y],
    make_comment_tags(Xs,Ys).

add_end_pos([], []).
add_end_pos([hide_nf_end, A, B|List], [A,B,hide_nf_end|NList]) :-
	add_end_pos(List, NList).
add_end_pos([hide_nf, A, B|List], [A,B,hide_nf|NList]) :-
	add_end_pos(List, NList).

add_end_pos([Ann, Start, Functor|List], [Start, End, Ann|NList]) :-
	get_end_pos(Start, Functor, End),
	add_end_pos(List, NList).


strip_ann_pos(logen(_,A),_ , _) :-
	var(A), !,
	throw('Free variable annotated directly. Use call(Var) instead.').

strip_ann_pos(logen(Ann,A),PA, [Ann,Pos,Func]) :-
	functor(A,Func,_),
	add_pos(A,PA,Pos),
	!.


strip_ann_pos(A, PA, Pos) :-
	annotation_matcher_data(A,_, MapFrom,MapTo),
	A = (_,_), !,
	 %% descend but dont add pos info (hack on conj)
	MapTo = [C1,C2],
	strip_ann_pos_l(MapFrom,MapTo, Pos),
	PA = (C1, C2).


strip_ann_pos(A, PA, Pos) :-
	annotation_matcher_data(A,B, MapFrom,MapTo),
	Pos = [Ann,Start,End|PosList],
	add_pos_keepargs(B, PA, Start),
	functor(A, Ann, _),
	%portray_clause(B),
	(is_if(B) ->
		(App = [], End = '->')
	;
		(nonvar(B) -> 
			(functor(B, End, _), App = [])
		; 
			(Start = 0, End = 0, App = [hide_nf_end, 0, 0]))
	),
	strip_ann_pos_l(MapFrom,MapTo, PosList2),
	append(PosList2, App, PosList), !.

/*canon(A, X, L) :- nonvar(A), A = (B,C), !, canon(C, X, R), canon(B, R, L).
canon(A, B, (A, B)) :- (nonvar(B) -> B \== null ; true), !.
canon(A, null, A).*/

canon(A, In, Out) :- nonvar(A), !, canon2(A, In, Out).
canon(A, In, Out) :- conjunct(A, In, Out).

canon2(A, In, Out) :-
    A = (B,C),
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


is_if(A) :- nonvar(A), A = ';'(B,_), nonvar(B), B='->'(_,_).

strip_ann_pos_l([],[],[]).
strip_ann_pos_l([A|As],[O|Os], PosList) :-
	strip_ann_pos(A,O, PL1),
	strip_ann_pos_l(As,Os, PL2),
	append(PL1,PL2, PosList).

add_pos(F, PF, Pos) :-
	F =.. [Func|Args],
	length(Args, X), length(NewArgs,X),
	PF =.. [Func,Pos|NewArgs].

add_pos_keepargs(A, (';'(_,'->'(Pos, If, Then), Else)), Pos) :- 
        nonvar(A), A = ';'(IT,Else), nonvar(IT), IT = (If->Then), !.

% special case to handle hide_nf
add_pos_keepargs(F, F, _) :- var(F), !.

add_pos_keepargs(F, PF, Pos) :-
	F =.. [Func|Args],
	PF =.. [Func,Pos|Args].

get_syntax_read(FilterSyntax) :-
    portable_read_position(Term, PosTerm, CommTokens),
    (Term == end_of_file ->
        FilterSyntax = []
    ;
        (
          get_syntax_read(Syntax),!,
          get_head_tag(PosTerm, HeadTags),
          %make_comment_tags(CommTokens, Comments),
          append(CommTokens, HeadTags, CHSyntax),
          append(CHSyntax, Syntax, FilterSyntax)
        )
    ).

match_pos_clauses(AnnList, Syntax) :-
	portable_read_position(T, PT2, CommTokens),
	(T == end_of_file ->
		(AnnList = [], Syntax = [])
	;
		(
		canon(PT2, empty, nonempty(PT)), !,
		match_pos_clauses(RestAnnList, RestSyntax),
		find_matching_clause(PT, T, PosList),
		get_head_tag(PT, HeadTags),
		append(PosList, RestAnnList, AnnList),
		append(CommTokens, HeadTags, PrelimSyntax),
		append(PrelimSyntax, RestSyntax, Syntax)
		)
	).

% if the clause is fully annotated
find_matching_clause(PT, _, PosList) :-
	pos_ann_clause(PT, PosList), !.
	
find_matching_clause(PT, _, PosList) :-
	pos_ann_clause((hide_nf, _, PT), PosList), !.

% otherwise make all annotations unknown
find_matching_clause(PT, T, NewList) :-
	%portray_clause(T),
	annotate_memo_clause(T, AT),
	%portray_clause(AT),
	AT = ':-'(logen(_,H), AB), !,
	convert_ann_to_pos(H, AB, PC, PosList),
	PC = PT,
	convert_to_unknown(PosList, NewList).

find_matching_clause(_, _, []).

convert_to_unknown([], []).
convert_to_unknown([Ann, Start, End|List], [NAnn, Start, End|NList]) :-
	(is_simple_ann(Ann) -> NAnn = unknown ; NAnn = Ann),
	convert_to_unknown(List, NList).
	
assert_pos_ann_clauses :- 
	ann_clause(_,H,AB),
	%portray_clause((H:-AB)),
	convert_ann_to_pos(H, AB, PC, PosList),
	%portray_clause(PC-PosList),
	assert(pos_ann_clause(PC, PosList)),
	fail.

assert_pos_ann_clauses.

convert_ann_to_pos(H, AB, PC, PosList) :-
	strip_ann_pos(AB,PB, PosList),
	canon(PB, empty, nonempty(CPB)), !,
	H =.. [Func|Args],
	length(Args,X),
	length(NArgs,X),
	PH =.. [Func,_|NArgs],
	PC = (':-'(_, PH,CPB)).

convert_body((A,B), (PA,PB), Pos) :-
	convert_body(A,PA,Pos1),
	convert_body(B,PB,Pos2),
	append(Pos1,Pos2,Pos).


convert_body(C,PC, [Pos]) :-
	C =..[Func|Args],
	PC =.. [Func,Pos|Args].

%%
%% Following code is straight from match_ann.pl
%%

get_filter_tags([],[]).
get_filter_tags([A|As],[NA|Ts]) :-
	A =.. [Name, Pos],
    !,
	atom_length(Name, Len),
    End is Pos + Len,
	NA =.. [Name, Pos, End],
    get_filter_tags(As,Ts).

get_filter_tags([_|As],Ts) :-
    !,
    get_filter_tags(As,Ts).

%%% This one should only match filters...
get_head_tag((:-(_Pos,filter(_Pos1,Head))), [filter(HeadPos,HeadEnd)|HeadTags]) :-
	!,
	Head =.. [HeadFunc, HeadPos|Args],
	%arg(1,Head, HeadPos ),
	%functor(Head, HeadFunc, _),
	get_end_pos(HeadPos, HeadFunc, HeadEnd),
	get_filter_tags(Args, HeadTags).

get_head_tag((:-(_Pos, Head)), HeadTags) :-
    !,
	get_head_tag(Head, HeadTags).

get_head_tag((:-(_Pos, Head, Body)), Tags) :-
    !,
    get_head_tag(Head, HeadTags),
    get_body_tag(Body, BodyTags),
    append(HeadTags, BodyTags, Tags).

get_head_tag(Head, [head(HeadPos, HeadEnd)]) :-
    arg(1,Head, HeadPos),
    functor(Head, HeadFunc, _Arity),
    get_end_pos(HeadPos, HeadFunc, HeadEnd),
    %%% what are these meant to do?? results never used??!?
    %A1 is Arity -1,
    %functor(CopyHead,HeadFunc, A1),
    !.

%% Make sure this doesnt cause failure, if all else fails just dont
%% return any tags...
get_head_tag(_,[]).

get_call_tag(Call,[]) :-
    nonvar(Call).
    %%% what are these meant to do?? results never used??!?
    %functor(Call,HeadFunc,Arity),
    %A1 is Arity -1,
    %functor(CopyHead,HeadFunc,A1).

get_body_tag((A,B),Tags) :-
    nonvar(A),
    !,
    get_call_tag(A,CTag),
    get_body_tag(B,BTag),
    append(CTag,BTag,Tags).

get_body_tag(C,Tag) :-
    get_call_tag(C,Tag).

get_body_tag(B,[]) :-
    portray_clause(unable_to_parse_body(B)).

tagHeadArguments([],[],[]).
tagHeadArguments([X|As],[_Type| Ts],Tags) :-
    var(X),
    !,
    tagHeadArguments(As,Ts,Tags).

tagHeadArguments([X|As],[Type| Ts],Tags) :-
    getHeadArgumentTag(X,Type,Tag),
    %portray_clause(    getHeadArgumentTag(X,Type,Tag)),
    tagHeadArguments(As,Ts,TagR),
    append(Tag,TagR,Tags).

getHeadArgumentTag(X,_Type,[]) :-
    var(X),
    !.
%%% LISTS are a special case as they are
%%% not position marked the same as others
getHeadArgumentTag([](_),_Type,[]) :-
    !.
getHeadArgumentTag([],_Type,[]) :-
    !.
getHeadArgumentTag([H|T], Type,Tags) :-
    !,
    getHeadArgumentTag(H,Type,Tag1),
    getHeadArgumentTag(T,Type,Tag2),
    %Tag2 = [],
    append(Tag1,Tag2,Tags).

getHeadArgumentTag(X,_,[]) :-
    atomic(X).

getHeadArgumentTag(X,Type,Tag) :-
    atomic(Type),
    !,
    (X = '$VAR'(Pos,Name,_) ->
        get_end_pos(Pos,Name,End),
        Tag = [Pos,End,Type]
    ;
        arg(1,X,Pos),
        (number(Pos) ->
        functor(X,Name,_),
        get_end_pos(Pos,Name,End),
        X =.. [Name,Pos|Args],

        %portray_clause(call__getHeadArgumentTag(Args,Type,Tag1)),
        getHeadArgumentTag(Args,Type,Tag1),
        %portray_clause(exit__getHeadArgumentTag(Args,Type,Tag1)),

        %Tag1 = [],

        Tag = [Pos,End,Type|Tag1]

        ;
        portray_clause(no_match_for_filter(X)),
        Tag = []
        )
    ).

getHeadArgumentTag(_X,_Type,[]) :-
    %portray_clause(ignoring(X,Type,[])),
    true.

getHeadArgumentTagL([],_Type,[]).
getHeadArgumentTagL([Arg|As],Type,Tags) :-
    %portray_clause(aaaa_getHeadArgumentTag(Arg,Type,Tag)),
    getHeadArgumentTag(Arg,Type,Tag),
    getHeadArgumentTagL(As,Type,TagR),
    append(Tag,TagR,Tags).


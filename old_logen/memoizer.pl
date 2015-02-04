%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code heavily borrowed from memo.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(memoizer,
	    [
	     memo_table/3,
             find_pattern/3, find_pattern/4,
             import_memo_file/2,
             export_memo_file/2,
             keep_and_mark_crossmodule/1,
             patid/4,
             insert_pattern_with_filter_types/5
             ]).

:- use_module(library(lists)).
:- use_module(library(terms)).   % ciao: see variant/2 below

:- use_module(logen_messages).
:- use_module('cogen-tools',[string_concatenate/3]).
:- use_module(logen_filter).
:- use_module(logen_attributes).



% ____________________________________________________________
%
% Memo tables
%
:- dynamic memo_table/3.   % memo_table(Module, CallPattern, ResCall)

patid(Module, CallPattern, ResCall, Module:Functor) :-
        % convert (in any direction) between a memo_table entry
        % and a term that can act as unique id for this entry.
        (var(CallPattern) ->
            memo_table(Module, CallPattern, ResCall)
        ;
            true
        ),
        ResCall =.. [Functor|_],
        !.

import_memo_file(Module, MemoFile) :-
        string_concatenate('Loading table ', MemoFile, Msg1),
        print_message(Msg1),
	seeing(OldFile),
	see(MemoFile),
	repeat,
	   read(T),
	   process_memo_entry(T, Module),
	   T == end_of_file,
	   !,
	seen,
	see(OldFile).

process_memo_entry(table(CallPattern, ResCall, Attributes), Module) :-
        assert(memo_table(Module, CallPattern, ResCall)),
        patid(Module, CallPattern, ResCall, Id),
        process_memo_table_entry(Attributes, Id).
process_memo_entry(gensym(N), Module) :-
        setattr(Module, gensym, N).
process_memo_entry(end_of_file, _).

process_memo_table_entry([], _).
process_memo_table_entry([Attribute|Tail], Id) :-
        setattr(Id, Attribute, true),
        process_memo_table_entry(Tail, Id).


export_memo_file(Module, MemoFile) :-
        tell(MemoFile),
        (getattr(Module, gensym, N) ->
            portray_clause(gensym(N))
        ;
            true
        ),
        (
           memo_table(Module, CallPattern, ResCall),
           patid(Module, CallPattern, ResCall, Id),
           findall(Attribute, getattr(Id, Attribute, true), Attributes),
           portray_clause(table(CallPattern, ResCall, Attributes)),
           fail
        );
        told.

keep_and_mark_crossmodule(Module) :-
        memo_table(Module, CallPattern, ResCall),
        patid(Module, CallPattern, ResCall, Id),
        (getattr(Id, crossmodule, true) ->
            setattr(Id, pending, true)
        ;
            retract(memo_table(Module, CallPattern, ResCall)),
            del_all_attr(Id)
        ),
        fail.
keep_and_mark_crossmodule(_).


% ____________________________________________________________
%
% Predicates that are called by the gx files
%

% -- filter_atom should not be used --
%new_pattern(Module, CallPattern, ResCall) :-
%        (find_pattern(Module, CallPattern, ResCall) -> true
%          ; filter_atom(Module, CallPattern, ResCall),
%            assert_pattern(Module, CallPattern, ResCall)
%        ),
%        patid(Module, CallPattern, ResCall, Id), setattr(Id, pending, true).

find_pattern(Module, CallPattern, ResCall) :-
        % memo_table lookup: to use with CallPattern instantiated.
        % this does not succeed if CallPattern would become
        % more instantiated.
        copy_term(CallPattern, P2),
        memo_table(Module, CallPattern, ResCall),
        variant(CallPattern, P2),
        !.
% here is a non-sicstus-specific version of 'variant':
%variant(A, B) :- variant_ex(A, B, _).
%variant_ex(A, B, _) :- ground(A), !, ground(B), A = B.
%variant_ex(_, B, _) :- ground(B), !, fail.
%variant_ex(A, B, Lst) :- var(A), !, var(B), variant_cv(A, B, Lst).
%variant_ex(_, B, _) :- var(B), !, fail.
%variant_ex([A|As], [B|Bs], Lst) :- !, variant_ex(A, B, Lst), variant_ex(As, Bs, Lst).
%variant_ex(A, B, Lst) :- A=..[F|As], B=..[F|Bs], variant_ex(As, Bs, Lst).
%variant_cv(V, W, Lst) :- var(Lst), !, Lst = [(V,W)|_].
%variant_cv(V, W, [(X,Y)|_]) :- X == V, !, Y == W.
%variant_cv(_, W, [(_,Y)|_]) :- Y == W, !, fail.
%variant_cv(V, W, [_|Tail]) :- variant_cv(V, W, Tail).


find_pattern(Module, CallPattern, ResCall, internal) :- !,
        find_pattern(Module, CallPattern, ResCall).

find_pattern(Module, CallPattern, ResCall, _NotInternal) :-
        find_pattern(Module, CallPattern, ResCall),
        patid(Module, CallPattern, ResCall, Id),
        setattr(Id, crossmodule, true).

insert_pattern_with_filter_types(Module, Call, FilterTypes, Requestor, ResidualCall) :-
        % this call should really be specialized away into the gx file
        filter_atom_with_filter_types(Module,Call,FilterTypes,ResidualCall),
        assert(memo_table(Module, Call, ResidualCall)),
        patid(Module, Call, ResidualCall, Id),
        setattr(Id, pending, true),
        (Requestor = internal -> true
          ; setattr(Id, crossmodule, true)
        ).





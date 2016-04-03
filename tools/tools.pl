% (c) 1996-2016 Michael Leuschel
% see https://github.com/leuschel/logen for more details

:- module(tools, [string_concatenate/3,
    print_error/1,
    print_message/1,
    same_len/2, is_list_skel/1,
	convert_cli_into_atom/2,
	get_modulename_filename/2,
	get_tail_filename/2]).




is_list_skel(X) :- nonvar(X), (X=[] -> true ; (X=[_|T], is_list_skel(T))).

same_len([],[]).
same_len([_|T],[_|T2]) :- same_len(T,T2).

print_error(Msg) :-
	format(user_error, "!~w~n", [Msg]).

print_message(Msg) :-
	format(user, "%~w~n", [Msg]).


:- if(current_prolog_flag(dialect, ciao)).
:- use_module(ciao_tools,[read_from_chars/2]).
:- else.
:- use_module(sics_tools,[read_from_chars/2]).
:- endif.
convert_cli_into_atom(CLIGOAL,Atom) :- read_from_chars(CLIGOAL,Atom).


string_concatenate(S1, S2, S3) :-
	name(S1, S1S),
	name(S2, S2S),
	append(S1S,S2S,S3S),
	name(S3,S3S).

:- use_module(library(lists)).

% :- assert_must_succeed(tools:get_modulename_filename('/aaaa/bbb/cc/d.app','d')).
get_modulename_filename(Path,Module) :- 
    get_tail_filename(Path,Tail),
    (split_last(Tail, '.',  M, _) -> Module=M ; Module='_').

% :- assert_must_succeed(tools:get_tail_filename('/aaaa/bbb/cc/d.app','d.app')).
get_tail_filename(Path,Tail) :- (split_last(Path, '/',  _, T) -> Tail=T ; Tail=Path).

split_last(Atom, Sep,  Head, Tail) :-
   atom_chars(Atom,ListAscii), atom_chars(Sep,[SepACode]),
   split_last2(ListAscii,SepACode,[],[],HeadA, TailA),
   atom_chars(Head,HeadA), atom_chars(Tail,TailA).

split_last2([],Sep,CurSplit,[Sep|Head],ResH,ResT) :-
   reverse(CurSplit,ResT),
   reverse(Head,ResH).
split_last2([Sep|Tail],Sep,CurSplit,Head,ResH,ResT) :- !,
   append([Sep|CurSplit],Head,NewHead),
   split_last2(Tail,Sep,[],NewHead,ResH,ResT).
split_last2([H|Tail],Sep,CurSplit,Head,ResH,ResT) :-
   split_last2(Tail,Sep,[H|CurSplit],Head,ResH,ResT).

:- module(flags,
	  [set_logen_flag/1,
	   unset_logen_flag/1,
	   query_logen_flag/1,
	   reset_all_flags/0,reset_to_defaults/0,
	   set_logen_flag/2,
	   unset_logen_flag/2,
	   query_logen_flag/2
	  ]).

ciao((:- set_prolog_flag(multi_arity_warnings, off))).

:- dynamic logen_flag/1.
logen_flag(pppp(flatten)).

% XXX use a module-safe mechanism like ann_directive/1 instead of this:
% known_flags(gx_module(_Module), 'a module to include in the gx file').

known_flags(moduledriver(rebuild),
            'rebuild everything').
known_flags(pppp(flatten),
	    'flattens the pp buffer before sending for processing').
known_flags(pppp(gX),
	    'printing in gx mode').
known_flags(pppp(resnot),
	    'resolve residual nots in program').
known_flags(option(clear_memo), 'clear memo before specialisation').


reset_all_flags :-
  unset_logen_flag(_),
  unset_logen_flag(_,_).
  
  
reset_to_defaults :-
  set_logen_flag(gx_debug_mode,0).


query_logen_flag(Flag) :-
	logen_flag(Flag).

set_logen_flag(Flag) :-
	unset_logen_flag(Flag),
	assert(logen_flag(Flag)).

unset_logen_flag(Flag) :-
	retractall(logen_flag(Flag)).
	
	

:- dynamic logen_flag/2.
logen_flag(option(clear_memo),yes).

set_logen_flag(Key,Flag) :-
    unset_logen_flag(Key,_),
    assert(logen_flag(Key,Flag)).
unset_logen_flag(Key,_Flag) :-
	retractall(logen_flag(Key,_Flag)).
	
query_logen_flag(Key,Flag) :- logen_flag(Key,Flag).

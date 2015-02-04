:- module(gensym,[gensym/2,
                  old_gensym_value/2,set_gensym_counter/2,
                  gennum/1,
                  reset_gennum/0,
                  set_gennum_counter/1,
                  old_gennum_value/1]).

:- use_module(library(lists),[append/3]).

gennum(Nr) :-
	oldvalue(num__num,Nr),
	N1 is Nr + 1,
	set_flag(gensym(num__num),N1).

oldvalue(Prefix,N) :- flag(gensym(Prefix),N),!.
oldvalue(_Prefix,0).

set_flag(Name,X) :-
	nonvar(Name),
	retract(flag(Name,_Val)),!,
	asserta(flag(Name,X)).
set_flag(Name,X) :-
	nonvar(Name),
	asserta(flag(Name,X)).

:- dynamic flag/2.
flag(foo,foo) :- fail.

reset_gensym :- retract(flag(_X,_Y)),fail.
reset_gensym.


reset_gennum :- retractall(flag(gensym(num__num),_Y)).

set_gennum_counter(ID) :-  set_flag(gensym(num__num),ID).
set_gensym_counter(Sym,ID) :-  set_flag(gensym(Sym),ID).

old_gensym_value(Sym,Val) :- oldvalue(Sym,Val).
old_gennum_value(Val) :- oldvalue(num__num,Val).


gensym(Prefix,V) :-
	var(V),
	atom(Prefix),
	oldvalue(Prefix,N),
	N1 is N + 1,
	set_flag(gensym(Prefix),N1),
	name(PE_Sep,"__"),
	string_concatenate(Prefix,PE_Sep,PreSep),
	string_concatenate(PreSep,N1,V).
	
%:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
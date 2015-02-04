/* ---------------------- */
/* GENERATING NEW SYMBOLS */
/* ---------------------- */
/* File: gensym.pl */


:- module(gensym,
    [gensym/2, gennum/1, reset_gennum/1]).

:- use_module('ciao_tools.pl', [string_concatenate/3]).

/* ===================================================== */

/* ------ */
/* GENSYM */
/* ------ */

/* generate a new symbol with the given prefix */
/* code from the "Art of Prolog" */
gensym(Prefix,V) :-
	var(V),
	atom(Prefix),
	oldvalue(Prefix,N),
	N1 is N + 1,
	set_flag(gensym(Prefix),N1),
	name(PE_Sep,"__"),
	string_concatenate(Prefix,PE_Sep,PreSep),
	string_concatenate(PreSep,N1,V).

gennum(Nr) :-
	oldvalue(num__num,Nr),
	N1 is Nr + 1,
	set_flag(gensym(num__num),N1).

reset_gennum(Nr) :-
	set_flag(gensym(num__num),Nr).

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

/* already defined in ecce_sicstus
:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
*/
:- module(filter, [can_be_var/1, 
			 dynamicType/1,
                   is_definitely_static/1, 
                   definitely_nonvar/1, 
                   get_sub_types/4,
                   translate_single_type/1,
		   translate_list_of_filters/2
                  % ,test/1
		  ]).

% JPG - December 2003



:- use_module(library(lists)).
ciao((:- use_module(library(sort)))).
% :- use_module(detlogentypes). 

:- use_module(logenbta). /* added by mal */


/*
Steve::: I DO NOT UNDERSTAND THIS, IF I IMPORT THIS MODULE
SICSTUS VERSION STOPS WORKING?????
*/
/*%%%CIAO
:- use_module('logen_create_types', []).
*/%%%CIAO


test(new) :-
	Type = [[dynamic,nonvar,static,list(dynamic)],[dynamic,nonvar,list(dynamic)]],
	translate_type(Type,Name),
	portray_clause(translate_type(Type,Name)).

test(0) :-
	write(can_be_var([[list,static],[list,nonvar]])),nl,
	can_be_var([[list,static],[list,nonvar]]).
test(1) :-
	write(can_be_var([[list,static],[list,nonvar],[var]])),nl,
	can_be_var([[list,static],[list,nonvar],[var]]).
test(2) :-
	write(is_definitely_static([[static],[list,static]])),nl,
	is_definitely_static([[static],[list,static]]).
test(3) :-
	write(is_definitely_static([[static],[list,static],[list,nonvar]])),nl,
	is_definitely_static([[static],[list,static],[list,nonvar]]).
test(test4(X,Y)) :-
	write(get_sub_types([[list,static],[list,nonvar]],[a,b],X,Y)), nl,
	get_sub_types([[list,static],[list,nonvar]],[a,b],X,Y).
	
	
can_be_var(Ts) :-
	member(X,Ts),
	(var(X) ; (X = '$VAR'(_))),
	!.
can_be_var(Ts) :-
	dynamicType(T),
	member(T,Ts).


dynamicType(T) :-
	'denotes_$VAR'(T).
	
is_definitely_static(Ts) :-
	\+ can_be_var(Ts),
	memberAll(Ts,static).

is_type(T, Ts) :-
	\+can_be_var(Ts),
	subsetAll(Ts, T).

is_nonvar_type(Ts) :-  /* is a type which is equivalent to logen's nonvar type */
	\+ can_be_var(Ts),
	memberAll(Ts,nonvar).	% changed JPG
	%member([dynamic],Ts).

is_a_static_list(Ts) :-
	\+ can_be_var(Ts),
	subsetAll(Ts,[list,static]).	% changed JPG
	%\+ member([dynamic],Ts),
	%[[dynamic,list,static]] = Ts.
	
subsetAll([],_).
subsetAll([T|Ts],X) :-
	subset(X,T),
	subsetAll(Ts,X).

subset(X,Y) :-
        \+ (member(Z,X), \+ member(Z,Y)).

is_a_list_skeleton(Ts) :-
	\+ can_be_var(Ts),
	%\+ member([dynamic],Ts),
	memberAll(Ts,list). % changed JPG
   %([[dynamic,list,static],[dynamic,list]] = Ts
   % ;
    %[[dynamic,list],[dynamic,list,static]] = Ts
   %).
	
memberAll([],_).
memberAll([T|Ts],X) :-
	member(X,T),
	memberAll(Ts,X).

definitely_nonvar(Ts) :-
	\+ can_be_var(Ts).
	
get_sub_types(Ts,U,Us,UTypes) :- /* added by mal */
   var(U), print(error_arg_var_in_get_sub_types(Ts,U,Us,UTypes)),
   Us=[],UTypes=[].
get_sub_types(Ts,U,Us,UTypes) :-
	% \+ can_be_var(Ts),
	U =.. [F|Us],
	functor(U,F,N),
	findLHSs(Ts,F/N,LHSs),
	reduceArgs(LHSs,[],[_|UTypes]).
	
findLHSs(Ts,F/N,Ls) :-
	findall(L,(member(R,Ts),transition(L,R,F/N)),Ls).
	
	
%Form sets of unique occurences of arguments 	
reduceArgs([],X,X).	
reduceArgs([A|As],T,R) :-	
	A =.. B,
	reduceArg(B,T,C),
	reduceArgs(As,C,R).

reduceArg([],[],[]).
reduceArg([A|As],[],[[A]|Rs]) :-
	reduceArg(As,[],Rs).

reduceArg([A|As],[T|Ts],[T|Rs]) :-
	member(A,T),
	!,
	reduceArg(As,Ts,Rs).
reduceArg([A|As],[T|Ts],[[A|T]|Rs]) :-
	reduceArg(As,Ts,Rs).

	
transition(L,R,F/N) :-
	functor(L,F,N),
	flatten_denotes(denotes(L,_),D),
	call(D),
	N1 is N+1,
	arg(N1,D,R).
	
flatten_denotes(denotes(T,V),Den) :-
	T =.. [F|Xs],
	name(denotes,Dn),
	name(F,Fn),
	append(Dn,[95|Fn],DFn),
	name(DF,DFn),
 	append(Xs,[V],Ys),
	Den =.. [DF|Ys].



:- dynamic type_seen_before/2.

translate_single_type(DIType) :-  retractall(type_seen_before(_,_)),
                      translate_and_print_type(DIType,_).


:- dynamic cur_stream/1.
print_cur(X) :- cur_stream(S), write(S,X).
nl_cur :- cur_stream(S), write(S,'\n').

translate_list_of_filters(Stream,[]) :- !,
    write(Stream,'/*  EMPTY FILTER DECLARATIONS */\n').
translate_list_of_filters(Stream,List) :- retractall(type_seen_before(_,_)),
    retractall(cur_stream(_)),
    assert(cur_stream(Stream)),
	old_gensym_value(t,SavedGS),
	set_gensym_counter(t,0),
	write(Stream, '/* FILTERS */'), nl(Stream),
	(translate_filters(Stream,List) -> true
	  ; print(translate_filters_failed(List))),
	set_gensym_counter(t,SavedGS),
	nl.

translate_filters(_,[]).
translate_filters(Stream,[Atom|T]) :-
	translate_filter(Atom,NewAtom),
	write(Stream,' :- filter '),write_term(Stream,NewAtom,[quoted(true),ignore_ops(true)]), write(Stream,'.\n'),
	translate_filters(Stream,T).
	     

translate_filter(FilterAtom,NewAtom) :- nonvar(FilterAtom), FilterAtom =.. [Pred|Args],
   l_translate_type(Args,TArgs),
   NewAtom =.. [Pred|TArgs].


l_translate_type([],[]).
l_translate_type([Type|T],[TType|TT]) :- translate_type(Type,TType),
	l_translate_type(T,TT).


translate_and_print_type(DIType,type(TypeNr)) :- sort(DIType,SDIType),
                         gensym(t,TypeNr),
                         assert(type_seen_before(SDIType,type(TypeNr))),
                         findall(SubT, 
                                 get_a_sub_type_term(SDIType,SubT),Res),
                         print_type(DIType,TypeNr,Res).
                         
print_type(DIType,TypeNr,RHS) :- print_cur(':- type '),print_cur(TypeNr),
                         print_cur(' ---> '), print_rhs(RHS),
                         print_cur('. %'),print_cur(DIType),nl_cur.

print_rhs([]) :- print_cur(fail).
print_rhs([X]) :- print_cur(X),!.
print_rhs([H|T]) :-
	print_cur('('), %% added by steve
	print_cur(H), print_cur(' ; '), print_rhs(T),
	print_cur(')').


%%% to identify a type use is_type/2
%%% this specifies it can not be var + each disjoint type contains the type

translate_type(T,Res) :- can_be_var(T),!,Res=dynamic.

%%%SICS/*
translate_type(T,Res) :- 'logen_create_types':userdefined_type(Type), is_type([Type],T), !, Res = Type.
%%%SICS*/

%translate_type(T,Res) :- is_type([list(struct(/,[static,dynamic]))],T), !,Res=list(struct(/,[static,dynamic])).
translate_type(T,Res) :- is_type([list(static)],T), !,Res=list(static).
translate_type(T,Res) :- is_type([list,static],T), !, Res=list(static).
translate_type(T,Res) :- is_definitely_static(T),!,Res=static.
translate_type(T,Res) :- is_type([list],T), !, Res=list(dynamic).
translate_type(T,Res) :- is_type([list(dynamic)],T), !,Res=list(dynamic).
translate_type(T,Res) :- is_nonvar_type(T),!,Res=nonvar.


translate_type(T,Res) :- sort(T,ST),type_seen_before(ST,Res),!.
translate_type(T,Res) :- translate_and_print_type(T,Res).

get_a_sub_type_term(DIType,SubTermType) :-
    denotes_symbol(S),get_sub_types(DIType,S,_SubTerms,SubTypes),
    l_translate_type(SubTypes,TranslSubTypes),
    functor(S,Op,_Ar),
    SubTermType =.. [Op|TranslSubTypes].


/* ----------------------------------------- */

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



/* Test output:
translate_list_of_filters([match([[dynamic,list,static],[dynamic,list]])]).
:- type t__5 ---> [dynamic|type(t__5)] ; []. %[[dynamic,list,static],[dynamic,list]]
filter(match(type(t__5))).

| ?- translate_list_of_filters([match([[dynamic,list,static],[dynamic,list]],[A]),
				match1([[dynamic,list,static],[dynamic,list]],[A],
					      [[dynamic,list,static],[dynamic,list]],[A])]).

:- type t__9 ---> [dynamic|type(t__9)] ; []. %[[dynamic,list,static],[dynamic,list]]
filter(match(type(t__9),dynamic)).
filter(match1(type(t__9),dynamic,type(t__9),dynamic)).

?- translate_single_type([[dynamic,list,static],[dynamic,list]]).
:- type t(3) ---> [dynamic|t(3)] ; []. %[[dynamic,list,static],[dynamic,list]]

yes
?- translate_single_type([[dynamic,static],[dynamic,list]]).
:- type t(5) ---> a ; b ; [dynamic|t(5)] ; []. %[[dynamic,static],[dynamic,list,static],[dynamic,list]]
:- type t(4) ---> a ; b ; [dynamic|t(5)]. %[[dynamic,static],[dynamic,list]]



?-translate_list_of_filters([p([A])]).

?- translate_list_of_filters([match1([[dynamic,static],[dynamic,list,static]],[A],[[dynamic,static],[dynamic,list,static]],[A]),match([[dynamic,static],[dynamic,list,static]],[A])]).
filter(match1(static,dynamic,static,dynamic)).
filter(match(static,dynamic)).

?- translate_list_of_filters([[[dynamic,static],[dynamic,list,static]]\==[A],
                             match1([[dynamic,static],[dynamic,list,static]],[A],
                             [[dynamic,static],[dynamic,list,static]],[A]),
                             match([[dynamic,static],[dynamic,list,static]],[A])]).


*/
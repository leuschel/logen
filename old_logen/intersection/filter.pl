:- module(filter, [can_be_var/1, 
			 dynamicType/1,
                   is_definitely_static/1, 
                   definitely_nonvar/1, 
                   get_sub_types/4,
                   translate_single_type/1,
		   translate_list_of_filters/1
                  % ,test/1
		  ]).

% JPG - December 2003

:- use_module(library(lists)).
ciao((:- use_module(library(sort)))).
% :- use_module(detlogentypes). 

:- use_module(logenbta). /* added by mal */

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
	var(X),
	!.
can_be_var(Ts) :-
	dynamicType(T),
	member(T,Ts).


dynamicType(T) :-
	'denotes_$VAR'(T).
	
is_definitely_static(Ts) :-
	\+ can_be_var(Ts),
	memberAll(Ts,static).
	
memberAll([],_).
memberAll([T|Ts],X) :-
	member(X,T),
	memberAll(Ts,X).

definitely_nonvar(Ts) :-
	\+ can_be_var(Ts).
	
get_sub_types(Ts,U,Us,UTypes) :- /* added by mal */
   var(U), print_message(error,arg_var_in_get_sub_types(Ts,U,Us,UTypes)),
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

translate_list_of_filters(List) :- retractall(type_seen_before(_,_)),
	old_gensym_value(t,SavedGS),
	set_gensym_counter(t,0),
	(translate_filters(List) -> true
	  ; print(translate_filters_failed(List))),
	set_gensym_counter(t,SavedGS).

translate_filters([]).
translate_filters([filter(Atom)|T]) :-
	translate_filter(Atom,NewAtom),
	print(filter(NewAtom)), print('.'),nl,
	translate_filters(T).
	     

translate_filter(FilterAtom,NewAtom) :- nonvar(FilterAtom), FilterAtom =.. [Pred|Args],
   l_translate_type(Args,TArgs),
   NewAtom =.. [Pred|TArgs].


l_translate_type([],[]).
l_translate_type([Type|T],[TType|TT]) :- translate_type(Type,TType),
	l_translate_type(T,TT).

:- use_module('../gensym.pl').

translate_and_print_type(DIType,type(TypeNr)) :- sort(DIType,SDIType),
                         gensym(t,TypeNr),
                         assert(type_seen_before(SDIType,type(TypeNr))),
                         findall(SubT, 
                                 get_a_sub_type_term(SDIType,SubT),Res),
                         print_type(DIType,TypeNr,Res).
                         
print_type(DIType,TypeNr,RHS) :- print(':- type '),print(TypeNr),
                         print(' ---> '), print_rhs(RHS),
                         print('. %'),print(DIType),nl.

print_rhs([]) :- print(fail).
print_rhs([X]) :- print(X),!.
print_rhs([H|T]) :- print(H), print(' ; '), print_rhs(T).



translate_type(T,Res) :- can_be_var(T),!,Res=dynamic.
translate_type(T,Res) :- is_definitely_static(T),!,Res=static.
translate_type(T,Res) :- sort(T,ST),type_seen_before(ST,Res),!.
translate_type(T,Res) :- translate_and_print_type(T,Res).

get_a_sub_type_term(DIType,SubTermType) :-
    denotes_symbol(S),get_sub_types(DIType,S,_SubTerms,SubTypes),
    l_translate_type(SubTypes,TranslSubTypes),
    functor(S,Op,_Ar),
    SubTermType =.. [Op|TranslSubTypes].


/* Test output:
translate_list_of_filters([filter(match([[dynamic,list,static],[dynamic,list]]))]).
:- type t__5 ---> [dynamic|type(t__5)] ; []. %[[dynamic,list,static],[dynamic,list]]
filter(match(type(t__5))).

| ?- translate_list_of_filters([filter(match([[dynamic,list,static],[dynamic,list]],[A])),
				filter(match1([[dynamic,list,static],[dynamic,list]],[A],
					      [[dynamic,list,static],[dynamic,list]],[A]))]).

:- type t__9 ---> [dynamic|type(t__9)] ; []. %[[dynamic,list,static],[dynamic,list]]
filter(match(type(t__9),dynamic)).
filter(match1(type(t__9),dynamic,type(t__9),dynamic)).

?- translate_single_type([[dynamic,list,static],[dynamic,list]]).
:- type t(3) ---> [dynamic|t(3)] ; []. %[[dynamic,list,static],[dynamic,list]]

yes
?- translate_single_type([[dynamic,static],[dynamic,list]]).
:- type t(5) ---> a ; b ; [dynamic|t(5)] ; []. %[[dynamic,static],[dynamic,list,static],[dynamic,list]]
:- type t(4) ---> a ; b ; [dynamic|t(5)]. %[[dynamic,static],[dynamic,list]]

*/
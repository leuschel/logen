% (c) 1996-2016 Michael Leuschel
% see https://github.com/leuschel/logen for more details

:- module(typechecker,[type_check/2, term_is_of_type/2, term_is_of_type/3,
   type/1, check_types/0]).


/* ------------------------ */
/* TYPE CHECKER FOR PROLOG  */
/* ------------------------ */

:- op(1150, fx, type).
:- op(500,yfx,--->).

:- use_module('ciao_tools.pl').

/*
=============================================================================
Using the type checker:
=============================================================================

1) Built-in types:

The following are built-in types:
	any
	ground
	nonground
	var
	nonvar
	integer
	real
	number
	atom
	atomic
	ask(Type) [ask user whether of type]

The following are built-in type constructors:
	list/1  -> list(TypeOfListElements)
	vlist/1 -> same as list/1 except that variables also match this type
	term/2  -> term(Functor,ListOfTypesForArguments)
	vterm/2 -> same as term/2 except that variables also match this type

=============================================================================

2) Defining your own types:

just enter type/2 definitions for each type you want to use, like:

	type(program,list(clause)).
	type(clause,term(cl,[atom,list(atom)])).
	type(atom,any).

You can also define your own type constructors:

	type(binary_tree(Type),term(null,[])).
	type(binary_tree(Type),
		term(tree,[binary_tree(Type),Type,binary_tree(Type)])).

=============================================================================

3) Type checking:

Just call:
	term_is_of_type([1,2,3],list(integer)).
	term_is_of_type(tree(null,1,null),binary_tree(ground)).

You can also call
	check_for_illegal_types.
to test whether you have introduced some illegal type definitions
 (it might currently loop for badly defined types like "type(rec,rec)." ).
=============================================================================
*/

:- dynamic user_defined_type/2.

type(Type ---> Def) :- assert_user_type(Type,Def),!.
type(Type = Def) :- assert_user_type(Type,Def),!.
type(TD) :- print_error('### Illegal Type Declaration: '), print_error(TD),nl.
  
 
assert_user_type(Type,Def) :- 
  %print_message(add_type(Type,Def)),
  translate_type_def(Def,TranslatedDef,Type),!,
  (user_defined_type(Type,OldDef)
   -> ((OldDef = TranslatedDef) -> true
         ; (print_error('### Type Clash for: '), print_error(Type),
            print_error(OldDef), print_error(TranslatedDef))
       )
    ;  (predefined_type(Type)
         -> (print_error('### Trying to redefine built-in type: '), print_error(Type))
         ;  assert(user_defined_type(Type,TranslatedDef))
       )
   ).
assert_user_type(Type,Def) :- print_error('### Could not translate type description for type:'),
 print_error(Type), print_error(Def).

translate_type_def(X,any,_) :- var(X),!,print_error('### Variable Type !').
translate_type_def(type(X),X,_) :- !.
translate_type_def(list(X),list(TX),Cur) :- !, translate_type_def(X,TX,Cur).
translate_type_def(vlist(X),vlist(TX),Cur) :- !, translate_type_def(X,TX,Cur).
translate_type_def((X;Y),(TX;TY),Cur) :- !,
       translate_type_def(X,TX,Cur), translate_type_def(Y,TY,Cur).
translate_type_def(term(F,A),term(F,TA),Cur) :- !, l_translate_type_def(A,TA,Cur).
translate_type_def(vterm(F,A),vterm(F,TA),Cur) :- !, l_translate_type_def(A,TA,Cur).
translate_type_def(X,X,CurType) :- (predefined_type(X) ; user_defined_type(X,_) ; X=CurType),!.
translate_type_def(X,term(Fun,TArgs),Cur) :- nonvar(X), X =.. [Fun|Args], !,
     l_translate_type_def(Args,TArgs,Cur).
translate_type_def(X,X,_) :- print_message('*** Forward type reference'), print_message(X).

l_translate_type_def([],[],_Cur).
l_translate_type_def([A|T],[AT|TT],Cur) :-
     translate_type_def(A,AT,Cur), l_translate_type_def(T,TT,Cur).

/* ----------------- */
/* predefined_type/1 */
/* ----------------- */

/* checks whether something is a predefined type */

predefined_type(any).
predefined_type(ground).
predefined_type(nonground).
predefined_type(var).
predefined_type(nonvar).
predefined_type(integer).
predefined_type(real).
predefined_type(number).
predefined_type(atom).
predefined_type(atomic).
predefined_type(list(_X)).
predefined_type(term(_F,_TL)).
predefined_type(vlist(_X)).
predefined_type(vterm(_F,_TL)).
predefined_type(ask(_T)).

/* ------------------------ */

print_types :- print_message('---------'),user_defined_type(Type,_Descr),
   print_message(type(Type,_Descr)),fail.
print_types :- print_message('---------').

/* ------------------------ */
/* legal_type_description/1 */
/* ------------------------ */

/* checks whether something is a legal type description */

check_types :- print('% Checking user defined types: '),
   user_defined_type(Type,Descr),
   (legal_type_description(Descr) -> (print(Type),print(' '))
     ; (print_error('### Error in type definition:'), print_error(Type),
        print_error(Descr))),
    fail.
check_types :- nl.     

legal_type_description(X) :- var(X),!,fail.
legal_type_description(any) :- !.
legal_type_description(ground) :- !.
legal_type_description(nonground) :- !.
legal_type_description(var) :- !.
legal_type_description(nonvar) :- !.
legal_type_description(integer) :- !.
legal_type_description(real) :- !.
legal_type_description(number) :- !.
legal_type_description(atom) :- !.
legal_type_description(atomic) :- !.
legal_type_description(ask(_Type)) :- !.
legal_type_description(list(Type)) :- !,
	legal_type_description(Type).
legal_type_description(vlist(Type)) :- !,
	legal_type_description(Type).
legal_type_description(term(_Functor,TypeList)) :- !,
    ((length(TypeList,Arity),functor(T,_Functor,Arity),
     user_defined_type(T,_))
      -> (print_message('*** Warning: term also exists as type:'), print_message(T))
      ;  true
    ),
	l_legal_type_description(TypeList).
legal_type_description(vterm(_Functor,TypeList)) :- !,
	l_legal_type_description(TypeList).
legal_type_description((Type1 ; Type2)) :- !,
    legal_type_description(Type1),!,legal_type_description(Type2).
legal_type_description(Type) :-
	\+(predefined_type(Type)),
	user_defined_type(Type,_Descr),!.
legal_type_description(Type) :- print_error('### Illegal type descriptor: '),
    print_error(Type),fail.

l_legal_type_description([]).
l_legal_type_description([Type1|Rest]) :-
	legal_type_description(Type1),
	l_legal_type_description(Rest).


/* ----------------- */
/* term_is_of_type/2 */
/* ----------------- */

type_check(Term,Type) :- %print_message(type_check(Term,Type)),
   reset_type_error,
   term_is_of_type(Term,Type,yes),
   \+(type_error_occurred).

term_is_of_type(Term,Type) :-
	term_is_of_type(Term,Type,yes). /* yes for print error messages */


/* ----------------- */
/* term_is_of_type/3 */
/* ----------------- */

/* if the third argument is yes then an error message is printed if the
	type check does not succeed and the call to term_is_of_type
	will NEVER FAIL !! */
/* if the third argument is different from yes than no message will be printed
	but the call will fail if the type check fails */

/* term_is_of_type(Term,Descr,PE) :-
	print(tiot(Term,Descr)),nl,fail. */

term_is_of_type(Term,Descr,PrintErrMsg) :-
	is_inf(Term),!,
	print_inf_error(Term,Descr,PrintErrMsg),
	read(_Cont).

term_is_of_type(_Term,any,_PrintErrMsg) :-
	!. /* anything is of type any */
term_is_of_type(Term,ground,_PrintErrMsg) :-
	ground(Term),!.
term_is_of_type(Term,nonground,_PrintErrMsg) :-
	\+(ground(Term)),!.
term_is_of_type(Term,var,_PrintErrMsg) :-
	var(Term),!.
term_is_of_type(Term,nonvar,_PrintErrMsg) :-
	nonvar(Term),!.
term_is_of_type(Term,integer,_PrintErrMsg) :-
	integer(Term),!.
term_is_of_type(Term,real,_PrintErrMsg) :-
	real(Term),!.
term_is_of_type(Term,number,_PrintErrMsg) :-
	number(Term),!.
term_is_of_type(Term,atom,_PrintErrMsg) :-
	atom(Term),!.
term_is_of_type(Term,atomic,_PrintErrMsg) :-
	atomic(Term),!.
term_is_of_type(Term,ask(Type),_PrintErrMsg) :-
	!,
	print('Type => '),print(Type),nl,
	print('Term => '),print(Term),nl,
	print('(y or n) =>'),read(UserChoice),
	UserChoice=y.
term_is_of_type(Term,list(Type),PrintErrMsg) :-
	var(Term),!,
	print_type_error(Term,list(Type),PrintErrMsg).
term_is_of_type([],list(_Type),_PrintErrMsg) :- !.
term_is_of_type([Head|Tail],list(Type),_PrintErrMsg) :-
	term_is_of_type(Head,Type,_PrintErrMsg),!,
	term_is_of_type(Tail,list(Type),yes).
term_is_of_type(Term,vlist(_Type),_PrintErrMsg) :-
	var(Term),!. /* also to avoid modifying Term in the next 2 clauses */
term_is_of_type([],vlist(_Type),_PrintErrMsg) :- !.
term_is_of_type([Head|Tail],vlist(Type),_PrintErrMsg) :-
	term_is_of_type(Head,Type,_PrintErrMsg),!,
	term_is_of_type(Tail,vlist(Type),yes).
term_is_of_type(Term,term(Functor,ArgTypeList),PrintErrMsg) :-
	var(Term),!,
	print_type_error(Term,term(Functor,ArgTypeList),PrintErrMsg).
term_is_of_type(Term,term(Functor,ArgTypeList),_PrintErrMsg) :-
	Term =.. [Functor|Args],!,
	l_term_is_of_type(Args,ArgTypeList,yes), !.
term_is_of_type(Term,vterm(_Functor,_ArgTypeList),_PrintErrMsg) :-
	var(Term),!. /* also to avoid modifying Term in the next clause */
term_is_of_type(Term,vterm(Functor,ArgTypeList),_PrintErrMsg) :-
	Term =.. [Functor|Args],!,
	l_term_is_of_type(Args,ArgTypeList,yes), !.
term_is_of_type(Term,Type,_PrintErrMsg) :-
	\+(predefined_type(Type)),
	user_defined_type(Type,Descr), /* user defined type */
	term_is_of_type(Term,Descr,no),!.
	/* printing=no because several alternative options might exist */
term_is_of_type(Term,(Type1 ; Type2),PrintErrMsg) :-
    (term_is_of_type(Term,Type1,no) -> true
      ; term_is_of_type(Term,Type2,PrintErrMsg)).

term_is_of_type(Term,Type,PrintErrMsg) :-
	print_type_error(Term,Type,PrintErrMsg).


l_term_is_of_type([],[],_PrintErrMsg).
l_term_is_of_type([Term1|Rest],[Type1|TypeList],PrintErrMsg) :-
	term_is_of_type(Term1,Type1,PrintErrMsg),!,
	l_term_is_of_type(Rest,TypeList,PrintErrMsg).
	
print_type_error(_T,Type,_) :- 
  (legal_type_description(Type) -> true
     ; (print_error(illegal_type(Type)),print_error(term(_T)))),fail.
print_type_error(Term,Type,PrintErrMsg) :- \+(PrintErrMsg=no), nl,
	print_error('### Type Error Occured! Type:'),print_error(Type),
	print_error('### Term:'),safe_print_term(Term),nl,
	assert_type_error.


print_inf_error(Term,Type,PrintErrMsg) :- \+(PrintErrMsg = no), nl,
	print_error('### Cyclic Term Error: Requested Type '),print_error(Type),
	print_error('### Term: '),safe_print_term(Term),nl,
	assert_type_error.

safe_print_term(X) :- var(X),!,print(X).
safe_print_term(X) :- is_inf(X),!,
	X =.. [Func|Args],
	print(Func),print('('),
	l_safe_print(Args),
	print(')').
safe_print_term(X) :- print(X).

l_safe_print([]).
l_safe_print([Term|T]) :-
	(is_inf(Term)
	 -> (print('!CYCLIC!'))
	 ;  (print(Term))
	), 
	((T=[]) -> true ; print(',') ),
	l_safe_print(T).

:- dynamic type_error/1.
type_error(no).

assert_type_error :- 
	retract(type_error(_N)),fail.
assert_type_error :-
	assert(type_error(yes)).


reset_type_error :-
	retract(type_error(_N)),fail.
reset_type_error :-
	assert(type_error(no)).

type_error_occurred :-
	type_error(yes).



:- use_module(library(terms)).
is_inf(X) :- cyclic_term(X).


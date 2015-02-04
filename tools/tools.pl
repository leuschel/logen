:- module(tools, [rev/3, member_nr/3, exact_member/2, exact_member_lookup/4,
                  remove/3, list_intersection/3, list_difference/3,
                  string_concatenate/3, is_upper_case_name/1,
	              print_error/1, print_message/1, print_short_msg/1,
	              print_dynamic_pred/3,
	              clever_and/3,
	              host_platform/1,
	              application_path/1,
	              translate_term_into_atom/2,
	              get_parent_directory/2,
	              is_list_skel/1]).

:- use_module(library(lists)).

:- use_module(self_check,'self_check.pl',[assert_pre/2,assert_post/2,
                              assert_must_succeed/1,assert_must_fail/1]).
:- use_module(library(system)).


is_list_skel(X) :- nonvar(X), (X=[] -> true ; (X=[_|T], is_list_skel(T))).


application_path(Dir) :-
   \+(prolog_flag(system_type,development)),
   environ('SP_APP_DIR',Dir).  /* /usr/local/bin/sicstus on development systems */

host_platform(Res) :-
  prolog_flag(host_type,HostType),
  name(HostType,AsciiList),
  map_host_platform(AsciiList,Res),!.

map_host_platform(HT,darwin) :- append("powerpc-darwin",_,HT).
map_host_platform(HT,windows) :- append("x86-win",_,HT).
map_host_platform(HT,linux) :- append("x86-linux",_,HT).
map_host_platform(_,unknown).


clever_and(true,X,X) :- !.
clever_and(X,true,X) :- !.
clever_and(X,Y,'And'([],[X,Y])).


print_error(Error) :-
	current_output(X),
	set_output(user_error),
	(var(Error) -> print_message(error,'_') ; print_message(error,Error)),
	set_output(X).
print_message(Msg) :-
	current_output(X),
	set_output(user),
	(var(Msg) -> print_message(informational,'_') ; print_message(informational,Msg)),
	set_output(X).
print_short_msg(Msg) :-
	current_output(X),
	set_output(user),
	write(Msg),
	set_output(X).
	
	

rev([],A,A).
rev([H|X],A,R) :- rev(X,[H|A],R).

member_nr(X,[X|_],1).
member_nr(X,[_|T],N) :-  member_nr(X,T,TN), N is TN + 1.


:- assert_pre(tools:exact_member(_Var,Vs),
                       (list_skeleton(Vs))).
:- assert_post(tools:exact_member(_Var,_Vs), true).
:- assert_must_succeed(tools:exact_member(V,[V])).
:- assert_must_succeed(tools:exact_member(V,[X,Z,V])).
:- assert_must_fail(tools:exact_member(W,[X,Z,V])).
:- assert_must_fail(tools:exact_member(W,[])).

exact_member(X,[Y|T]) :-
   ((X==Y) -> true ; exact_member(X,T)).
   

:- assert_pre(tools:exact_member_lookup(_Var,_ValRes,Vs,Vals),
                       (list_skeleton(Vs),list_skeleton(Vals))).
:- assert_post(tools:exact_member_lookup(_Var,_ValRes,_Vs,_Vals), true).
:- assert_must_succeed(tools:exact_member_lookup(V,2,[V],[2])).
:- assert_must_succeed(tools:exact_member_lookup(V,2,[X,Z,V],[1,3,2])).
:- assert_must_fail(tools:exact_member_lookup(V,3,[X,Z,V],[1,3,2])).
:- assert_must_fail(tools:exact_member_lookup(W,3,[X,Z,V],[1,3,2])).
:- assert_must_fail(tools:exact_member_lookup(W,3,[],[1,3,2])).
                       
exact_member_lookup(Var,ValRes,[V|TV],[Val|TVal]) :-
   ((Var==V) -> (ValRes=Val) ; exact_member_lookup(Var,ValRes,TV,TVal)).
   

/*
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
*/

remove([X|T],X,T).
remove([Y|T],X,[Y|DT]) :-  \+(X=Y),   remove(T,X,DT).


:- assert_must_succeed(tools:list_intersection([a,b,c,d],[d,f,b],[b,d])).

list_intersection([],_L,[]).
list_intersection([H|T],L,Res) :-
   (remove(L,H,NL) -> (Res=[H|RR]) ; (Res=RR,NL=L)),
   list_intersection(T,NL,RR).
   
   
:- assert_must_succeed(tools:list_difference([a,b,c,d],[b,f,d],[a,c])).

list_difference([],_L,[]).
list_difference([H|T],L,Res) :-
   (remove(L,H,NL) -> (Res=RR) ; (Res=[H|RR],NL=L)),
   list_difference(T,NL,RR).

:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   safe_name(X,Xs),safe_name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
   
   
safe_name(lambda_res(X),[114,101,115,95|N]) :- !, name(X,N).
safe_name(X,N) :- atomic(X),!, name(X,N).
safe_name(X,N) :- var(X),!,name(var,N).
safe_name(X,N) :- functor(X,F,_),name(F,N), print_error(non_atomic_in_safe_name(X)).
   
   
   
safe_numbervars(Term,Start,End) :-
  on_exception(_E,numbervars(Term,Start,End),true).
   
:- use_module(library(charsio)).
translate_term_into_atom(Term,Atom) :- safe_numbervars(Term,0,_),
                       write_term_to_chars(Term,Temp,[quoted(true),numbervars(true)]),
                       name(Atom,Temp).
   
:- assert_must_succeed(tools:is_upper_case_name('GOODS')).
:- assert_must_succeed(tools:is_upper_case_name('ZZAA')).
:- assert_must_fail(tools:is_upper_case_name('capacity')).
:- assert_must_fail(tools:is_upper_case_name('PARAs')).

is_upper_case_name(Name) :-
  safe_name(Name,AsciiList),
  upper_case_list(AsciiList).
  
upper_case_list([]).
upper_case_list([H|T]) :- H >="A", H=<"Z", upper_case_list(T).   
   
print_dynamic_pred(InModule,Pred,N) :- nl,
   print(':- dynamic '), print(Pred), print('/'),print(N), print('.'),nl,
   functor(Call,Pred,N),
   call(InModule:Call), write_term(Call,[quoted(true)]),print('.'),nl,fail.
print_dynamic_pred(InModule,Pred,N) :- 
   functor(Call,Pred,N),
   (call(InModule:Call) -> true ; (write_term(Call,[quoted(true)]), print(' :- fail.'),nl)),
   nl.



:- assert_must_succeed(get_parent_directory('/aaaa/bbb/cc/d.app','/aaaa/bbb/cc/')).
:- assert_must_succeed(get_parent_directory('/aaaa/bbb/cc/','/aaaa/bbb/cc/')).
    
get_parent_directory(Path,NewPath) :-
   name(Path,PathAscii),
   strip_last(PathAscii,[],[],New),
   name(NewPath,New).
   
:- use_module(library(lists)).

strip_last([],ResSoFar,_,Res) :- reverse(ResSoFar,Res).
strip_last([47|Tail],ResSoFar,StripSoFar,Res) :- !,
  append([47|StripSoFar],ResSoFar,NewRes),
  strip_last(Tail,NewRes,[],Res).
strip_last([A|Tail],ResSoFar,StripSoFar,Res) :-
  strip_last(Tail,ResSoFar,[A|StripSoFar],Res).
  
  
/* for Ciao 
:- use_module(library(dec10_io)).
:- use_module(library(write)).

prefix([],_).
prefix([H|T],[H|Y]) :- prefix(T,Y).
*/



/*
fd_copy_term(X,Y,true) :- copy_term(X,Y).  
*/
/* comment out for Sicstus 3.8.5(?) or higher */



/*
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

/* defined in library(lists):
same_length([],[]).
same_length([_|T],[_|FT]) :- same_length(T,FT).
*/


/*
:- dynamic debugging/1.

debugging(off).

debug(Val) :- retractall(debugging(_)),assert(debugging(Val)).

debug_print(_X) :- (debugging(on) -> print(_X) ; true).
debug_nl :- (debugging(on) -> nl ; true).
*/
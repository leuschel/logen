:- module(tools, [rev/3, member_nr/3, exact_member/2, exact_member_lookup/4,
                  remove/3, list_intersection/3,
                  string_concatenate/3, is_upper_case_name/1,
	              print_error/1, print_message/1, print_short_msg/1,
	              print_dynamic_pred/3,
	              clever_and/3]).

:- use_module(library(lists)).
:- use_module(library(messages)).

clever_and(true,X,X) :- !.
clever_and(X,true,X) :- !.
clever_and(X,Y,'And'([],[X,Y])).


print_error(Error) :-
	current_output(X),
	set_output(user_error),
	(var(Error) -> show_message(error,'_') ; show_message(error,Error)),
	set_output(X).
print_message(Msg) :-
	current_output(X),
	set_output(user),
	(var(Msg) -> show_message(informational,'_') ; show_message(informational,Msg)),
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


exact_member(X,[Y|T]) :-
   ((X==Y) -> true ; exact_member(X,T)).
   

                       
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


list_intersection([],_L,[]).
list_intersection([H|T],L,Res) :-
   (remove(H,L,NL) -> (Res=[H|RR]) ; (Res=RR,NL=L)),
   list_intersection(T,NL,RR).

% :- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   safe_name(X,Xs),safe_name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
   
   
safe_name(lambda_res(X),[114,101,115,95|N]) :- !, name(X,N).
safe_name(X,N) :- atomic(X),!, name(X,N).
safe_name(X,N) :- name(non_atomic,N), print_error(non_atomic_in_safe_name(X)).
   
   
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

same_length([],[]).
same_length([_|T],[_|FT]) :- same_length(T,FT).
*/


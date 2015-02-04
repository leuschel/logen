:- module(error_manager,[reset_errors/0, add_error/2, add_error/3,
           add_message/2, add_message/3,
           get_error/2, get_error/1, get_all_errors/1,
           test_error_occured/1, count_errors_occured/2 ]).

:- use_module(tools).

:- dynamic logged_error/2.

reset_errors :- retractall(logged_error(_,_)).

add_error(Source,ErrMsg, ErrTerm) :-
    translate_expression(ErrTerm,RString),
    string_concatenate(ErrMsg,RString,S),
    add_error(Source,S).
    
add_error(Source,ErrMsg) :- print_error('An error occurred !'),
   print_error(source(Source)),
   print_error(ErrMsg),
   assertz(logged_error(Source,ErrMsg)).
   
add_message(Source,ErrMsg, ErrTerm) :- /* same as add_error but does not print err msg */
    translate_expression(ErrTerm,RString),
    string_concatenate(ErrMsg,RString,S),
    add_message(Source,S).
add_message(Source,ErrMsg) :- 
   assertz(logged_error(Source,ErrMsg)).
   
get_error(Source,ErrMsg) :-
   retract(logged_error(Source,ErrMsg)).
   
get_error(Msg) :-
    get_error(Source,ErrMsg),
    string_concatenate(ErrMsg,';',S0),
    string_concatenate(Source,': ',S1),
    string_concatenate(S1,S0,Msg).
      
get_all_errors(All) :-
    findall(Err,get_error(Err),All),
    All \= [].
    
    
    
test_error_occured(Source) :- logged_error(Source,_),!.

%:- use_module(library(lists),[length/2]).

count_errors_occured(Source,NrOfErrors) :- 
  findall(1,logged_error(Source,_),Ls),
  length(Ls,NrOfErrors).
  
  
  /* needs to be defined: */

:- use_module(library(charsio)).

  
translate_expression(Exp,S) :-
	write_to_chars(Exp,Chars),
	name(S,Chars).

:- module('logen_messages', [
	    print_error/1, print_message/1, print_short_msg/1,
	    print_error_line_number/1, print_error_line_number/0,
	    print_debug_message/1,
	    print_indentation/0,
	    increase_indentation/0, decrease_indentation/0]).

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(messages)).
:- endif.

:- dynamic indent_level/1.

indent_level(0).

print_error_line_number(Error) :-
    print_error(Error),print_error_line_number.
print_error_line_number :-
    current_stream(_FN,_mode,Stream),
    line_count(Stream,N),
	current_output(X),
	set_output(user_error),
	write('! ###  Error occured at line number: '), write(N), nl,
	set_output(X). 
print_error(Error) :-
	current_output(X),
	set_output(user_error),
	write('! ### '),write(Error), nl,
	set_output(X).
print_message(Msg) :-
	current_output(X),
	set_output(user),
	show_message(informational,Msg),
	set_output(X).

print_short_msg(Msg) :-
	current_output(X),
	set_output(user),
	write(Msg),
	set_output(X).
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:


%:- use_module('prob/logen_preferences.pl').
get_preference(gx_debug_mode,1).

print_debug_message(Msg) :-
  ((get_preference(gx_debug_mode,C), C>0)
     -> print_message(Msg) ; true).
     
     

print_indentation  :-
	current_output(X),
	set_output(user),
	indent,
	set_output(X).
	
indent :- indent_level(X),indent2(X).
indent2(X) :- (X>0 -> (write('+'),X1 is X-1, indent2(X1)) ; write('>')).

increase_indentation :-
    retract(indent_level(X)),
    X1 is X+1,
    assert(indent_level(X1)).
    
decrease_indentation :-
    retract(indent_level(X)),
    (X>0
     -> (X1 is X-1,assert(indent_level(X1)))
     ;  (assert(indent_level(0)))
    ).

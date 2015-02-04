/* not being used for the moment; maybe useful for fully standalone gx files;
  but currently this module interferes with run_gx.pl */
  
:- module(run_gx_tools,
	  [ add_two_arguments/5,
	    print_message/1,
	    print_short_msg/1
	  ]).
	  
	  
:- use_module(library(lists)).	  

/* copied from cogen-tools: */	  
add_two_arguments(Suffix, Call, Arg1, Arg2, RCall) :-
	Call =.. [Pred|Args],	
	res_name(Suffix, Pred, ResPred),
	append(Args, [Arg1, Arg2], NewArgs),
	RCall =.. [ResPred|NewArgs].
	
	
res_name(T,Pred,ResPred) :-
  name(PE_Sep,T),string_concatenate(Pred,PE_Sep,ResPred).
  
  
	  
print_message(Msg) :-
	current_output(X),
	set_output(user),
	print_message(informational,Msg),
	set_output(X).

print_short_msg(Msg) :-
	current_output(X),
	set_output(user),
	write(Msg),
	set_output(X).

:- dynamic memo_table/3.

%:- use_module(library(terms),[variant/2]).

 
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
% (c) 1996-2016 Michael Leuschel
% see https://github.com/leuschel/logen for more details

 :- module(error_manager,[reset_errors/0, reset_errors_from_source/1,
             add_error/3,
             add_message/4,
	     add_exception/4,
           get_error/2, get_all_errors/1,
	   set_verbosity_level/2,
           test_error_occured/1, count_errors_occured/1, count_errors_occured_with_source/2,verbosity_level/1 ]).


:- dynamic logged_error/2.
:- dynamic verbosity_level/1.


set_verbosity_level(NewLevel, OldLevel) :-
	verbosity_level(OldLevel),
	retract(verbosity_level(OldLevel)),
	assert(verbosity_level(NewLevel)).

verbosity_level(1).

message_stream(user_error).
error_stream(user_error).


reset_errors :- retractall(logged_error(_,_)).
reset_errors_from_source(Source) :- retractall(logged_error(Source,_)).

add_exception(Source,ErrFormat, ErrArgs, Exception) :- 
	parse_exception(Exception, FmtS,FmtArgs),
	append(ErrFormat, FmtS, FormatString),
	append(ErrArgs, FmtArgs, FormatArgs),
	add_error(Source, FormatString, FormatArgs).
	
add_error(Source,ErrFormat, ErrArgs) :- 
	error_stream(ErrS),
	format(ErrS, "~N ! An error occurred~n! ",[]),
	format(ErrS, ErrFormat, ErrArgs),
	format(ErrS, "~N", []),
	flush_output(ErrS),
	assertz(logged_error(Source,(ErrFormat-ErrArgs))).

    
   
%% use source for debug level also
%%% info(source,Level)
%%% level 1 = Normal print
%%% level 2 = -v  (verbose)
%%% level 3 = -vv (extra verbose (full debug))
% Michael : doesn't seem to work at gx time !
add_message(_Source, Level, Msg, Args) :-
	verbosity_level(C),
	Level =< C,
	!,
	print_log_message(Msg, Args).
add_message(_,_, _,_) :-
	!.

	



print_log_message(Msg, Args) :-
	message_stream(Str),
	format(Str, "~N % ",[]),
	format(Str, Msg, Args),
	format(Str, "~N", []),
	flush_output(Str).
	
   
get_error(Source,ErrMsg) :-
   retract(logged_error(Source,ErrMsg)).
   
%get_error(error(Source,Msg-Args)) :-
%    get_error(Source,Msg-Args).
    
   
      

get_all_errors(All) :-
    findall(Err,get_error(_Source,Err),All),
    \+(All=[]).
    
        
test_error_occured(Source) :- logged_error(Source,_),!.

%:- use_module(library(lists),[length/2]). length is builtin

count_errors_occured(NrOfErrors) :-
  count_errors_occured_with_source(_,NrOfErrors).
  
count_errors_occured_with_source(Source,NrOfErrors) :- 
  findall(1,logged_error(Source,_),Ls),
  length(Ls,NrOfErrors).



%%% Human readable error messages:
%% should return a FmtString and Arg - e.g. compatible with format(FmtString,FmtArgs), describing the error
parse_exception(error(syntax_error(P1,P2, Msg, Tokens),_Pred), FmtString, FmtArgs) :-
	token_format_string(Tokens,ErrString),
	append("SYNTAX ERROR: (lines ~w-~w) ~w~n",ErrString, FmtString),
	append([P1,P2,Msg],Tokens, FmtArgs),
	!.

parse_exception(error(existence_error(source_sink,Filename),_Pred), "Unable to open file: ~w", [Filename]) :- !.

parse_exception(E, "~w", [E]).  % default just print


token_format_string([],[]).
token_format_string([_T|Ts],[126,119,32|ErrStr]) :- token_format_string(Ts,ErrStr).


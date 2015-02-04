
%%:- module(socket, [prolog_socket/0]).

:- use_module(library(sockets)).
:- use_module(library(lists)).

:- op(1150, fx, type).
%runtime_entry(start) :-
%	start.

prolog_socket :- start(_).

start(Port) :-
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(localhost, Port)),
	socket_listen(Socket, 5),

	%open(prologport, write,Stream),
	%format(Stream,'~w~n',[Port]),
	%close(Stream),
	prolog_flag(user_output, Stdout, Stdout),
	format(Stdout,'Port: ~w~n', [Port]),
	flush_output(Stdout),
	wait(Socket),
	socket_close(Socket).

write_list(_Stream,[]).
write_list(Stream,[A|T]) :-
	write_val(Stream,A),
	format(Stream, " ",[]),
	write_list(Stream,T).
write_val(Stream,L) :-
	is_list(L),
	!,
	write_list(Stream,L).
write_val(Stream,A) :-
	format(Stream,'~w',[A]).

%% If a goal is succesful then we must return the variable mappings
%% as well as success.  X=val\000Y=val etc, no ordering is imposed
write_var_map(_,[]).
%write_var_map(_,[Var], Map) :-
%	!,
%	format_to_chars('~w', [Var], Map).
write_var_map(S,[Var=Val|Vars]) :-
	%write_val(Val,SVal),	
	%format_to_chars('~@~w~@~s', [ put_byte(1), Var,put_byte(2), SVal], SMap),
	format(S,'~@~w~@',[put_byte(1),Var,put_byte(2)]),
	write_val(S,Val),
	write_var_map(S,Vars).


%% Wait for a connection on the given socket
wait(Socket) :-
	socket_accept(Socket, Stream),
	read_socket(Socket, Stream).

%% We have a connection so just wait for valid terms, and return either
%% OK X=val\000Y=val1
%% FAIL\000
%% ERROR SHORT long message here\000
read_socket(Socket,Stream) :-	
	on_exception(E,
		     read_term(Stream, Term, [variable_names(Vars)]),
		     (portray_clause(user_error,goal_parse_error),Parse=fail,exception(E,Stream))),
	
	%portray_clause(user_error, term(Term)),
	(Parse=ok ->
	    true,
	    %DEBUG portray_clause(goal(Term)),
	    (
	      (Term == end_of_file ->
		  (
		    socket_close(Socket),
		    halt
		  )
	      ;
   		  %portray_clause(user_error,calling(Term)),
		  make_call(Stream, Term, Vars)
		  %portray_clause(user_error,exit(Term))
		  %DEBUG% ,portray_clause(vars(Vars))
	      )
	    )	    
	;
	    (
	      %% Parse failed, but we have already written error message
	      true,
	      portray_clause(user_error, an_exception_occurred(E))
	    )
	),
	flush_output,
	put_code(Stream, 1),
	put_code(Stream, 1),	
	flush_output(Stream),

	read_socket(Socket, Stream).
	

	

%% We have a valid term so make the call
make_call(Stream,Term, Vars) :-
	( on_exception(CallEx,
		       call(Term),
		       (Exception = true, exception(CallEx,Stream))) ->
	    (Exception == true ->
		%exception occurred but we have already written to stream in handler
		portray_clause(user_error, exception(make_call/3, CallEx)),
		true
	    ;
		(		  
		  %% write return values directly to stream
		  %format_to_chars('OK--', [], STRING),
		  %portray_clause(user_error, return('OK', Vars)),
		  format(Stream,'OK--', []),		  
		  write_var_map(Stream, Vars)
		  %append(STRING, MAP, SReturn),
		  %name(Return, SReturn),
		  %portray_clause(return(Return)),
		  %write_term(Stream, Return, [])
		 
		
		
		)
	    )		  	      
	;		  
	    write_term(Stream, 'FAIL', []),
	    portray_clause(user_error, return('FAIL'))
	).


get_stream_info(S,Info) :-
	on_exception(E,get_stream_info1(S,Info),(portray_clause(user_error, exception(E)))).

get_stream_info1(S,blank):-
	portray_clause(user_error, stream_is(S)),
	%true.
	fail.
get_stream_info1(S, Info) :-
	stream_property(S,file_name(Info)),!.

get_stream_info1(S, Info) :-
	
	stream_property(S,alias(Info)),
	!.
get_stream_info1(S, unknown).


tokens_to_string([],[]).
tokens_to_string([A|As], [B|Bs]) :-
	token_to_string1(A,B),
	tokens_to_string(As,Bs).


token_to_string1(atom(A)-_, A) :- !.
token_to_string1(var(_,S,_)-_, A) :-
	!,
	name(A,S).


token_to_string1(A, A).


%% Exception handlers
%% First Argument is Sicstus exception, second is verbose message for user
exception(E,_Stream) :- portray_clause(user_error,E),fail.


exception(syntax_error(Call, Pos, Msg, Tokens,Lineend),Stream) :-
	!,
	%portray_clause(user_error,syntax_error(_Call, _A, Msg, _Tokens,_B)),
	portray_clause(user_error,syntax_stream(Call)),
	(Call = read_term(Err,_,_) ->
	    %portray_clause(user_error,syntax_stream(Err)),	    
	    get_stream_info(Err,Info)
	    %portray_clause(user_error,stream_id(Info))	
	;
	    Info = unknown_stream
	),
	tokens_to_string(Tokens,Where),
	%(Call = read_term(ErrStream,_,_) ->
	%    stream_property(ErrStream,file_name(F)),
	%    portray_clause(user_error, err_filename(F))
	%;
	%    true
	%),
	
	format(Stream,'ERR-~@PARSE~@~w in ~w\nlines::~w-~w\nCheck ~w\n', [put_byte(1),put_byte(1),Msg,Info,Pos,Lineend,Where]).
	


%exception(existence_error(get(_,_), 0, _, 0, past_end_of_stream),Stream) :-
exception(existence_error(_, _, _, _, past_end_of_stream),Stream) :-
	!,
	format(Stream,'ERR-~@EXISTENCE~@Attempted to read past end of stream', [put_byte(1),put_byte(1)]).
	
	

exception(existence_error(_, _,procedure,PredSpec,_),Stream) :-
	!,	
	%format_to_chars('ERR-~@EXISTENCE~@Unable to call predicate ~w', [put_byte(1),put_byte(1),PredSpec], Return),
	%name(Term, Return).
	format(Stream,'ERR-~@EXISTENCE~@Unable to call predicate ~w', [put_byte(1),put_byte(1),PredSpec]).
	

exception(existence_error(_, _,file,PredSpec,_),Stream) :-
	!,	
	%format_to_chars('ERR-~@EXISTENCE~@Unable to open file ~w', [put_byte(1),put_byte(1),PredSpec], Return),
	%name(Term, Return).
	format(Stream,'ERR-~@EXISTENCE~@Unable to open file ~w', [put_byte(1),put_byte(1),PredSpec]).

exception(E,Stream) :-
	%format_to_chars('ERR-~@UNKNOWN~@~w', [put_byte(1),put_byte(1),E], Return),
	%name(Term, Return),
	format(Stream,'ERR-~@UNKNOWN~@~w', [put_byte(1),put_byte(1),E]),
	portray_clause(user_error,unknown_exception(E)).


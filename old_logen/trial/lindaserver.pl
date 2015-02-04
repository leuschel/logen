:- module('lindaserver', [linda_server/0,			  
			  connect_to_linda/1,
			  quit_linda_server/0
			 ]).

:- use_module(library('linda/server')).
:- use_module(library('linda/client')).
:- use_module(library(sockets)).
:- use_module(library(system),[exec/3]).
:- use_module(library(charsio), [format_to_chars/3]).

%%% The port the socket will broadcast the linda port on!
port(9001).

%% for debug of port server comment out first line
port_server_streams([null,null,null]) :- !.
port_server_streams([std,null,std]).


linda_server :-
	linda((_Host:Port)-start_port_server(Port)),
	quit_port_server.

%% Build the command and then start the port server
start_port_server(Port) :-
	format_to_chars("sicstus -l lindaserver.pl --goal \"lindaserver:port_server(~w),halt.\"", [Port],CharCommand), name(Cmd, CharCommand),
	format(user_error, "Starting port server (transmitting ~w)...",[Port]),
	port_server_streams(Streams),
	exec(Cmd, Streams, _pid),
	format(user_error, "OK~n",[]).
	
port_server :-
	%% Get the port number from file
	linda_port_file(File),
	open(File, read, P),
	read_term(P, Term, []),
	close(P),	
	Term = linda_port(LindaPort),
	port_server(LindaPort).

port_server(LindaPort) :-
	%% open up socket and tell everyone who asks the port!
	current_host(Host),
	port(Port),
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host,Port)),
	send_port_number(Socket,LindaPort),
	socket_close(Socket).
	    
send_port_number(Socket,LindaPort) :-
	socket_listen(Socket, 5),
	portray_clause(user,waiting),
	socket_accept(Socket, _Client, Stream),
	read_term(Stream, Term,[]),
	(Term == get_port ->
	    (	portray_clause(Stream, linda_port(LindaPort)),		
		flush_output(Stream),	
		send_port_number(Socket,LindaPort)
	    )
	;
	    true
	).

	
%%% Send quit message to port server
quit_port_server :-
	port(Port),
	current_host(Host),
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host,Port), Stream),
	portray_clause(Stream,quit),
	flush_output(Stream),
	socket_close(Socket).

%% Fetch the linda port number from the host and connect
connect_to_linda(Host) :-
	get_linda_port(Host,LindaPort),
	format(user_error,"Connecting to Linda on ~w~n", [(Host:LindaPort)]),
	linda_client((Host:LindaPort)).

%%% query the portserver on Host for the LindaPort
get_linda_port(Host,LindaPort) :-
	port(Port),	
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host,Port), Stream),
	portray_clause(Stream,get_port),
	flush_output(Stream),
	read_term(Stream,Term, []),
	Term = linda_port(LindaPort),
	socket_close(Socket).

%% Shutdown the server
quit_linda_server :-
	shutdown_server,
	close_client.


	





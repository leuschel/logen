:- module(timer_ciao, [start_time/0, end_time/1, end_time/2]).

:- use_module(library(prolog_sys)).

start_time :-
	statistics(runtime,_).

end_time(Message, S) :-	
	statistics(runtime,[_,T1]),
	Time is T1/1000,
	write(S, Message),
	write(S, Time),
	write(S, ' secs.'),
	nl(S).

end_time(S) :-
	end_time('Analysis Time: ',S).
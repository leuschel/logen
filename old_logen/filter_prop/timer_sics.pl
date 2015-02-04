:- module(timer_sics, [start_time/0, end_time/1, end_time/2]).

:- use_module(library(system)).

start_time :-
%	true.
	statistics(runtime,_).

end_time(Message, S) :-
%	write(S,'ignoring time--Steve\n').
	statistics(runtime,[_,T1]),	
	Time is T1/1000,
	write(S, Message),
	write(S, Time),
	write(S, ' secs.'),
	nl(S).

end_time(S) :-
	end_time('Analysis Time: ',S).
%	statistics(runtime,[_,T1]),
%	Time is T1/1000,
%	write(S, 'Analysis Time: '),
%	write(S, Time),
%	write(S, ' secs.'),
%	nl(S).

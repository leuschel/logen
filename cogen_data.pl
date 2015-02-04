:- module(cogen_data, [cogen_data/2, clear_cogen_data/0]).

%% Any State used by cogen, cogen_data(id, Data).
%% Will be clear each run
:- dynamic cogen_data/2.

%% Reset all cogen data, is called by cogen/0
clear_cogen_data :-
	retractall(cogen_data(_,_)).

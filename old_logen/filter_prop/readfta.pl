:- module(readfta, [readfta/2]).

:- use_module(readprog).

readfta(File, FTA) :-
	readprog(File,Cls),
	clause2fta(Cls, FTA).

clause2fta([predicates(_)|Cls],FTA) :-
	clause2fta(Cls,FTA).
clause2fta([clause(((L -> R) :- true),[])|Cls],[[G,Arglist,R]|FTA]) :-
	L =.. [G|Arglist],
	clause2fta(Cls,FTA).
% replace the previous clause by the one below if arguments should be
% singleton lists (e.g. [[nat],[list]] instead of [nat,list]
%clause2fta([clause(((L -> R) :- true),[])|Cls],[[G,Args,R]|FTA]) :-
%	L =.. [G|Arglist],
%	makeArgList(Arglist,Args),
%	clause2fta(Cls,FTA).
clause2fta([],[]).
	 
makeArgList([],[]).
makeArgList([A|Arglist],[[A]|Args]) :-
	makeArgList(Arglist,Args).

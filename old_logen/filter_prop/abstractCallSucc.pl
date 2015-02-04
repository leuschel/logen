:- module(abstractCallSucc,[
		abstractSuccess/1,
		abstractCall/1]).
		


% defines success and call patterns over the types
% dynamic, static, nonvar, var

abstractSuccess(static is static).
abstractSuccess(dynamic \== dynamic).
abstractSuccess(dynamic \= dynamic).
abstractSuccess(dynamic == dynamic).

abstractSuccess(functor(nonvar,static,static)).
abstractSuccess(atomic(static)).

abstractSuccess(static < static).
abstractSuccess(static =< static).
abstractSuccess(static > static).
abstractSuccess(static >= static).


% conditions under which call can be evaluated
abstractCall(dynamic is static).
abstractCall(static \= static).

%abstractCall(dynamic \== dynamic).
abstractCall(static \== static).
abstractCall(static == static).
abstractCall(static \= static).
abstractCall(functor(nonvar,dynamic,dynamic)).
abstractCall(functor(dynamic,static,static)).
abstractCall(atomic(static)).

abstractCall(static < static).
abstractCall(static =< static).
abstractCall(static > static).
abstractCall(static >= static).


%% There is a problem with this file with respect
%% to bta:
%% solve_atom(A), and solve(B) are both unsafe
%% but only one need be marked memo.
%% How to choose which one?
solve([]).
solve([A|T]) :- solve_atom(A), solve(T).

solve_atom(A) :- my_clause(A,B), solve(B).


my_clause(app([],L,L),[]).
my_clause(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
my_clause(rev(A,B), [rev(A,[],B)]).
my_clause(rev([],A,A),[]).
my_clause(rev([A|B], C, D), [rev(B,[A|C],D)]).
my_clause(rev_app(A,B,C), [rev(A,D), app(D,B,C)]).



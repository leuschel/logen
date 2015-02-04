
solve([]).
solve([A|T]) :- solve_atom(A), solve(T).

solve_atom(A) :- my_clause(A,B), solve(B).


my_clause(app([],L,L),[]).
my_clause(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
my_clause(p,[p]).



solve([]).
solve([A|T]) :- solve_atom(A), solve(T).

solve_atom(A) :- my_clause(A,B), solve(B).


my_clause(app([],L,L),[]).
my_clause(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
my_clause(p,[p]).
my_clause(solve2([]),[]).
my_clause(solve2([A|T]), [solve_atom2(A), solve2(T)]).
my_clause(solve_atom2(A), [my_clause2(A,B), solve2(B)]).
my_clause(my_clause2(app([],L,L),[]),[]).
my_clause(my_clause2(app([H|X],Y,[H|Z]),[app(X,Y,Z)]),[]).


test(R) :-
   solve_atom(solve_atom2(app([a,b,c],[d,e,f,g],R))).
   
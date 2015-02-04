/* an extension of vanilla_list.pl which adds support for negation and
   built-ins */
   
l_solve([]).
l_solve([H|T]) :- solve_literal(H), l_solve(T).
solve_literal(not(H)) :- \+ solve_literal(H).
solve_literal(H) :- (user_predicate(H) -> solve_atom(H) ; fail).
solve_literal(C) :- built_in(C).

built_in(=(X,Y)) :- X=Y.
built_in(is(X,Y)) :- X is Y.

solve_atom(H) :- my_clause(H,Bdy), l_solve(Bdy).

user_predicate(Call) :-
  my_clause(H,_),functor(H,F,N),functor(Call,F,N).
my_clause(dapp(X,Y,Z,R), [app(Y,Z,YZ), app(X,YZ,R)]).
my_clause(app([],R,R), []).
my_clause(app([H|X],Y,[H|Z]), [app(X,Y,Z)]).
my_clause(not_abc(X),[not(app(_,[X|_],[a,b,c]))]).
my_clause(square(X,R),[R is X*X]).

test(X) :- solve_literal(not_abc(X)).


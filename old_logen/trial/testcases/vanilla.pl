/* Created by Pylogen */
solve([]).
solve([A|T]) :- solve_atom(A),solve(T).
    

solve_atom(A) :-
    claus(A,B),
    solve(B).

claus(p(_X,0),[]).
claus(p(X,s(N)),[p(f(f(f(f(X,X)))),N),q(f(f(X)))]).
claus(q(_),[]).
claus(runa(0),[]).
claus(runa(s(X)),[runa(X)]).
claus(a,[b, c(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(
               0
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               ))))))))))))))))
               )))))))))))))))))]).
claus(b,[]).
claus(b,[]).
claus(b,[]).
claus(b,[]).
claus(b,[]).
claus(c(_),[fail]).

test(_) :- solve_atom(a).
test(_).


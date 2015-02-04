
t(X) :- p(f(f(X))),p(g(g(X))).

p(a).
p(f(X)) :- X \== b.
p(g(X)) :- X \== b.


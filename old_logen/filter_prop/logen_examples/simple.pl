

p(X,Y) :- q(X,Y).

q(X,X).
q(X,Z) :- e(X,Y), q(Y,Z).

e(X,X).



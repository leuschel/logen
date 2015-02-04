

p(X,Y) :- q(X,Y), r(Y,X).


q(a,b).
q(b,c).
q(d,e).

r(b,a).
r(e,d).
r(f,e).
r(c,b).

test(X) :- p(b,X).
test(X) :- p(X,b).
    
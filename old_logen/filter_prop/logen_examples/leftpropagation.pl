/* leftpropagation.pl */

q(X,Y,Z) :- r(X,Y),p(X,Z).

r(a,b).
r(b,c).
r(c,a).

p(a,a).
p(a,b).
p(a,c).
p(b,b).
p(b,c).
p(c,c).

q2(X,Y,Z) :- r(X,Y),p(X,Z).
g(X1,X2,X3) :- p(X1,X2), q(X2,X3), r(X1,X3).

p(a,b).        q(b,c).       r(a,d).
p(b,c).        q(c,d).       r(b,c).
                             r(a,W).

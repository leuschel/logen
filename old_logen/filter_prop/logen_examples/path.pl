path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z),path(Z,Y).

edge(a,b).
edge(a,c).
edge(c,b).
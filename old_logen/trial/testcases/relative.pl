
go(X) :- relative(john,X).


relative(X,Y) :- ancestor(Z,X), ancestor(Z,Y).
 
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).
 
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
 
father(jap,carol).
father(jap,jonas).
father(jonas,maria).
mother(carol,paulina).
mother(carol,albertina).
mother(albertina,peter).
mother(maria,mary).
mother(maria,jose).
mother(mary,anna).
mother(mary,john).

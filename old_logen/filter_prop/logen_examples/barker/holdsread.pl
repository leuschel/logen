

holds_read(_User,Object) :- built_in(Object).
holds_read(User,Object) :- permitted(User,read,Object), fact(Object), call(Object).
holds_read(User,Object) :- permitted(User,read,Object), rule(Object,Body), l_holds_read(User,Body).

l_holds_read(_U,[]).
l_holds_read(U,[H|T]) :- holds_read(U,H), l_holds_read(U,T).

built_in('='(X,X)).
built_in('is'(X,Y)) :- X is Y.


permitted(User,Op,Obj) :- ura(User,Role), active(User,Role), senior_to(Role,R2),
                          pra(R2,Op,Obj).

sd(r1,r2).

senior_to(X,X).
senior_to(X,Y) :- sd(X,Z), senior_to(Z,Y).

active(u1,r1).
active(u2,r2).

ura(u1,r1).
ura(u1,r2).
ura(u2,r2).

pra(r1,read,s(_)).
pra(r2,read,p(_)).
pra(r2,read,q(_,_)).
pra(r1,read,r(_,_)).

fact(p(X)).
fact(s(X)).
rule(q(X,Y),[p(X),p(Y)]).
rule(r(X,Y),[q(X,Y),s(X)]).



holds(U,O) :- holds_read(U,O).

:- holds_read(u1,q(X,Y)), print(X,Y),nl.
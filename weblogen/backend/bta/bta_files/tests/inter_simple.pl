

int(cst(X),X).
int(+(X,Y),Res) :- int(X,RX), int(Y,RY),Res is RX+RY.
int(-(X,Y),Res) :- int(X,RX), int(Y,RY), Res is RX-RY.
int(*(X,Y),Res) :- int(X,RX), int(Y,RY), Res is RX*RY.
int(/(X,Y),Res) :- int(X,RX), int(Y,RY), Res is RX/RY.
int(fun(X),Res) :- def(X,Def), int(Def,Res).

%
%def(inc(X),cst(R)) :- R is X+1.
%
def(square(X),*(X,X)).
def(cube(X), *(X,fun(square(X)))).
def(power4(X),fun(square((fun(square(X)))))).
def(rec,fun(rec)).
def(rec,fun(rec)).

%def(one,cst(1)).

























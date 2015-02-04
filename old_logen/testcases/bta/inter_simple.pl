

int(cst(X),X).
int(+(X,Y),Res) :- int(X,RX), int(Y,RY). %, Res is RX+RY.
int(-(X,Y),Res) :- int(X,RX), int(Y,RY). %, Res is RX-RY.
int(fun(X),Res) :- def(X,Def), int(Def,Res).

%
%def(inc(X),cst(R)) :- R is X+1.
%
def(rec,fun(rec)).
def(rec,fun(rec)).

%def(rec, fun(_)).
%def(one,cst(1)).




























int(cst(X),_,X).
int(var(X),Env,R) :- lookup(X,Env,R).
int(plus(X,Y),Env,Res) :- int(X,Env,RX), int(Y,Env,RY), Res is RX+RY.
int(minus(X,Y),Env,Res) :- int(X,Env,RX), int(Y,Env,RY), Res is RX-RY.
int(fun(X),Env,Res) :- def0(X,Def), int(Def,Env,Res).


def0(one,cst(1)).
def0(rec,fun(rec)).
def0(big,fun(big(fun(big)))).


lookup(X,[X/Y|_], Y).
lookup(X,[Var/_|T], Res) :-
    X \= Var,
    lookup(X,T,Res).

test(Env, Res) :-
    int(minus(plus(var(xx),cst(4)),var(zz)),Env,Res).

















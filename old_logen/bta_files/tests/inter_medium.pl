

int(cst(X),_,_,X).
int(var(X),Vars,Vals,R) :- lookup(X,Vars,Vals,R).
int(plus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX+RY.
int(minus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX-RY.
int(fun(X),Vars,Vals,Res) :- def0(X,Def), int(Def,Vars,Vals,Res).


def0(one,cst(1)).
def0(rec,fun(rec)).
def0(big,fun(big(fun(big)))).




lookup(X,[X|_],[Val|_],Val).
lookup(X,[Y|T],[_|ValT],Res) :-
   X \= Y, lookup(X,T,ValT,Res).

test(R,X,Z) :- int(minus(plus(var(xx),cst(4)),var(zz)),[aa,bb,cc,dd,ee,ff,gg,zz,yy,xx],[A,B,C,D,E,F,G,Z,Y,X],R).















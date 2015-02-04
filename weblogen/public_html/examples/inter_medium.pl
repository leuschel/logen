

int(cst(X),_,_,X).
int(var(X),Vars,Vals,R) :- lookup(X,Vars,Vals,R).
int(plus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX+RY.
int(minus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX-RY.
int(fun(X),Vars,Vals,Res) :- def0(X,Def), int(Def,Vars,Vals,Res).
%int(fun(X,Arg),Vars,Vals,Res) :- 
%   def1(X,Var,Def), int(Arg,Vars,Vals,ResArg),
%   int(Def,[Var|Vars],[ResArg|Vals],Res).

def0(one,cst(1)).
def0(rec,fun(rec)).

%def1(inc,xx,plus(var(xx),cst(1))).
%def1(rec,xx,fun(rec,var(xx))).

lookup(X,[X|_],[Val|_],Val).
lookup(X,[Y|T],[_|ValT],Res) :-
   X \= Y, lookup(X,T,ValT,Res).

test(R) :- int(minus(plus(var(xx),cst(4)),var(zz)),[aa,bb,cc,dd,ee,ff,gg,zz,yy,xx],[0,0,0,0,0,0,0,1,2,3],R).
%test2(R) :- int(minus(plus(var(xx),cst(4)),fun(inc,var(zz))),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).
%test_rec(R) :- int(fun(rec),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).










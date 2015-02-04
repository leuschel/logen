
big(0,_).
big(s(Y),X) :-
    int(+(cst(3),cst(5)),R1),
    int(plus(var(aa),plus(var(aa),var(aa))), [aa],[R1],R),
    match(*(+(a,b)),X),
    big(Y,X).




%% int medium
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

test(R) :- int(minus(plus(var(xx),cst(4)),var(zz)),[aa,bb,cc,dd,ee,ff,gg,zz,yy,xx],[0,0,0,0,0,0,0,1,2,3],R).

%% int simple

int(cst(X),X).
int(+(X,Y),Res) :- int(X,RX), int(Y,RY),Res is RX+RY.
int(-(X,Y),Res) :- int(X,RX), int(Y,RY), Res is RX-RY.
int(fun(X),Res) :- def(X,Def), int(Def,Res).

def(rec,fun(rec)).


%% Regexp
match(Regexp,String) :- regexp(Regexp,String,[]).

regexp(eps,T,T).
regexp(X,[X|T],T) :- atomic(X).
regexp(+(A,_B),Str,DStr) :- regexp(A,Str,DStr).
regexp(+(_A,B),Str,DStr) :- regexp(B,Str,DStr).
regexp(.(A,B),Str,DStr) :- regexp(A,Str,I), regexp(B,I,DStr).

regexp(*(A),S,DS) :- regexp(.(A,*(A)),S,DS).

regexp(*(A),S,S).

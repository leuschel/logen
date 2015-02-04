/* Created by Pylogen */



%int(cst(X), _,_,X).
int(var(X), Vars,Vals, R) :- lookup(X, Vars,Vals,R).
%int(+(X,Y), Vars,Vals,R) :- int(X,Vars,Vals,X1), int(Y,Vars,Vals, Y1), R is X1 + Y1.



lookup(X,[X|_],[Val|_],Val).
lookup(X,[Y|T],[_|ValT],Res) :-
   X \= Y, lookup(X,T,ValT,Res).

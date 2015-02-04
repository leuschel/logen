ast(X) :- 
X=[class(c,
   [
   meth(foo,
     seq(
       mcall(tok1,this,bar),
       mcall(tok2,this,foo))),
   meth(bar, mcall(tok3,this,bar))
   ])
  ].

subExpr(E,E).
subExpr(E,X) :- 
  X =..  [_|List],
  mem(E1,List),
  subExpr(E,E1).

within(TOK,Meth,Cl, Ast) :-
  mem(class(Cl,Meths),Ast),
  mem(meth(Meth,Body),Meths),
  subExpr(mcall(TOK,_,_),Body).

:- use_module(library(lists)).
    
pc1(Trace,Ast) :-
    Trace=[calls(TOK,_,bar)|_], within(TOK,foo,c,Ast).

pc2(Trace,Ast) :-
    Trace=[calls(TOK,_,bar)|_], within(TOK,foo,c,Ast),
member(calls(_,_,foo),Trace).

mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).
    
test1(Trace) :- ast(Ast), Trace=[calls(tok1,_,bar)|_], pc1(Trace,Ast).
test2(Trace) :- ast(Ast), Trace=[calls(tok1,_,bar)|_], pc2(Trace,Ast).

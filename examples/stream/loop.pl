/* a simple program to show the optimisation we want to obtain:
  recompute func only when necessary */

main(_) :- loop(25,R), print(res(R)),nl.

:- dynamic loop/2.

loop(0,0).
loop(X,Res) :- X>0,
   X10 is X//10,
   func(X10,Y),  /* stays fixed for 10 iterations */
   X1 is X-1,
   loop(X1,R1),
   Res is Y+X+R1.
   
:- dynamic func/2.
func(X,R) :- R is X*X.

:- dynamic table_function/1.
table_function(func/2).


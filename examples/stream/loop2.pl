/* a simple program to show the optimisation we want to obtain:
  recompute func only when necessary */

:- dynamic loop/2.

loop(0,0).
loop(X,Res) :- X>0,
   X10 is X//10,
   Y5 is X//5,
   func2(X10,Y5,R0),  /* stays fixed for 10 iterations */
   print(func2(X10,Y5,R0)),nl,
   X1 is X-1,
   loop(X1,R1),
   Res is R0+R1.
   
:- dynamic func/3.
func2(X,Y,R) :- func(Y,RY),R is X+RY.


:- dynamic func/2.
func(X,R) :- R is X*X.

:- dynamic table_function/1.
table_function(func/2).
table_function(func2/3).

/*
?- [loop2].
?- loop(25,R).
func2(2,5,27)
func2(2,4,18)
func2(2,4,18)
func2(2,4,18)
func2(2,4,18)
func2(2,4,18)
func2(1,3,10)
func2(1,3,10)
func2(1,3,10)
func2(1,3,10)
func2(1,3,10)
func2(1,2,5)
func2(1,2,5)
func2(1,2,5)
func2(1,2,5)
func2(1,2,5)
func2(0,1,1)
func2(0,1,1)
func2(0,1,1)
func2(0,1,1)
func2(0,1,1)
func2(0,0,0)
func2(0,0,0)
func2(0,0,0)
func2(0,0,0)

R = 197 ? 

*/
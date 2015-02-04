/* A Model Checker for CTL fomulas */
/* written for XSB-Prolog */
/* by Michael Leuschel, Thierry Massart */

:- use_module(library(clpr)).

sat(_E,true).
sat(_E,false) :- fail.

sat(E,p(P)) :- prop(E,P). /* elementary proposition */

sat(E,and(F,G)) :- sat(E,F), sat(E,G).

sat(E,or(F,_G)) :- sat(E,F).
sat(E,or(_F,G)) :- sat(E,G).

sat(E,not(F)) :- not(sat(E,F)).

sat(E,en(F)) :- /* exists next */
    trans(_Act,E,E2),sat(E2,F).

sat(E,an(F)) :- /* always next */
    not(sat(E,en(not(F)))).

sat(E,eu(F,G)) :- /* exists until */
    sat_eu(E,F,G).

sat(E,au(F,G)) :- /* always until */
  sat(E,not(eu(not(G),and(not(F),not(G))))),
  sat_noteg(E,not(G)).

sat(E,ef(F)) :- /* exists future */
 sat(E,eu(true,F)).

sat(E,af(F)) :- /* always future */
  sat_noteg(E,not(F)).

sat(E,eg(F)) :- /* exists global */
  not(sat_noteg(E,F)). /* we want gfp -> negate lfp of negation */

sat(E,ag(F)) :- /* always global */
  sat(E,not(ef(not(F)))).
  

/* :- table sat_eu/3.*/
/* tabulation to compute least-fixed point using XSB */
  
sat_eu(E,_F,G) :- /* exists until */
    sat(E,G).
sat_eu(E,F,G) :- /* exists until */
   sat(E,F), trans(_Act,E,E2), sat_eu(E2,F,G).


/* :- table sat_noteg/2.*/
/* tabulation to compute least-fixed point using XSB */

sat_noteg(E,F) :-
   sat(E,not(F)).
sat_noteg(E,F) :-
   not(  (trans(_Act,E,E2), not(sat_noteg(E2, F) ))  ).

/* classical Prolog version:

trans(enter_cs,[X,Sema,CritSec,Y,C],
	       [X1,Sema1,Cs1,Y,C]) :-
	     X>0, X1 is X-1,
	     Sema>0, Sema1 is Sema-1,
	     CritSec>=0,
	     Cs1 is CritSec+1,
	     Y>=0, C>=0.
trans(exit_cs, [X,Sema,CritSec,Y,C],
	           [X,Sema1,CritSec1,Y1,C]) :-
	     CritSec>0, CritSec1 is CritSec-1,
	     Sema>=0,Sema1 is Sema + 1,
	     Y>=0,Y1 is Y+1,
	     C>=0.
trans(restart, [X,Sema,CritSec,Y,ResetCtr],
	           [X1,Sema,CritSec,Y1,ResetCtr1]) :-
	       Y>0, Y1 is Y-1,
	       X>= 0, X1 is X+1,
	       Sema >= 0, CritSec >= 0,
	       ResetCtr>=0, ResetCtr<10, ResetCtr1 is ResetCtr + 1.
	       
prop([_X,_Sema,CritSec,_Y,_C],unsafe) :- CritSec>1.
prop([0,_Sema,0,0,_C],deadlock).
prop([_X,0,0,0,_C],deadlock).

*/
	       
	       
trans(enter_cs,[X,Sema,CritSec,Y,C],
	       [X1,Sema1,Cs1,Y,C]) :-
	     {X>0}, {X1 = X-1},
	     {Sema>0}, {Sema1 = Sema-1},
	     {CritSec>=0},
	     {Cs1 = CritSec+1},
	     {Y>=0}, {C>=0}.
trans(exit_cs, [X,Sema,CritSec,Y,C],
	           [X,Sema1,CritSec1,Y1,C]) :-
	     {CritSec>0, CritSec1 = CritSec-1,
	     Sema>=0,Sema1 = Sema + 1,
	     Y>=0,Y1 = Y+1,
	     C>=0}.
trans(restart, [X,Sema,CritSec,Y,ResetCtr],
	           [X1,Sema,CritSec,Y1,ResetCtr1]) :-
	       {Y>0, Y1 = Y-1,
	       X>= 0, X1 = X+1,
	       Sema >= 0, CritSec >= 0,
	       ResetCtr>=0, ResetCtr<10, ResetCtr1 = ResetCtr + 1}.
	       /* print(restart([X,Sema,CritSec,Y,ResetCtr])),nl. */

prop([_X,_Sema,CritSec,_Y,_C],unsafe) :- {CritSec>1}.
prop([0,_Sema,0,0,_C],deadlock).
prop([_X,0,0,0,_C],deadlock).


mc(X) :- sat([X,1,0,0,0],ef(p(unsafe))).
mc(_).

mc2(X) :- sat([X,1,0,0,0],ef(p(deadlock))).

time(Goal,Time) :- 
	statistics(runtime,[Global1,_]),
	call(Goal),
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1.

time(Goal) :-
	time(Goal,Time),
	print('Time for goal: '),print(Goal),
	print(' is: '),print(Time), print(' ms'),nl.
	
:- time(mc(2)).
	
	

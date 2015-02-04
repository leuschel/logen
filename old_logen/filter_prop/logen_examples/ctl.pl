/* A Model Checker for CTL fomulas */
/* written for XSB-Prolog */
/* by Michael Leuschel, Thierry Massart */


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
   /* loop via double negation --> monotonic --> ok */
   
   /*
   sat(E,an(noteg(F)))  -->
   
   not(sat(E,en(not(noteg(F)))))  -->
   
   not(  trans(_Act,E,E2),sat(E2, not(noteg(F)) )  )  -->
   
   not(  trans(_Act,E,E2), not(sat(E2, noteg(F)) )  ) -->
   
   not(  trans(_Act,E,E2), not(sat_noteg(E2, F) )  )
   */


trans(enter_cs,[s(X),s(Sema),CritSec,Y,C],
	       [X,Sema,s(CritSec),Y,C]).
trans(exit_cs, [X,Sema,s(CritSec),Y,C],
	       [X,s(Sema),CritSec,s(Y),C]).
trans(restart, [X,Sema,CritSec,s(Y),ResetCtr],
	       [s(X),Sema,CritSec,Y,s(ResetCtr)]).

prop([X,Sema,s(s(CritSec)),Y,C],unsafe).
prop([0,Sema,0,0,C],deadlock).
prop([X,0,0,0,C],deadlock).

/*

trans(action,S,NewState) :- trans(S,NewState).
trans(0,1). trans(0,2).
trans(1,3). trans(1,4).
trans(2,5). trans(2,6).
trans(3,0). trans(3,7).
trans(4,7). trans(5,8).
trans(6,0). trans(6,8).
trans(7,2). trans(8,1).


prop(0,n1). prop(0,n2).
prop(1,t1). prop(1,n2).
prop(2,n1). prop(2,t2).
prop(3,c1). prop(3,n2).
prop(4,t1). prop(4,t2).
prop(5,t1). prop(5,t2).
prop(6,n1). prop(6,c2).
prop(7,c1). prop(7,t2).
prop(8,t1). prop(8,c2).

*/

unsafe :- model_check(Tr).

model_check(Tr) :-
 initial_marking(M),
 search_unsafe(Tr,M).

initial_marking([s(NumberOfProcesses),s(0),0,0,0]).
/*
initial_marking([NumberOfProcesses,s(0),0,0,0]).
*/

unsafe_marking([X,0,0,0,_]).
unsafe_marking([0,X,0,0,_]).

/*
unsafe_marking([_,_,s(s(_)),_,_]).
*/
search_unsafe([],State) :- unsafe_marking(State).
search_unsafe([Action|As],InState) :-
	trans(Action,InState,NewState),
	search_unsafe(As,NewState).
	

trans(enter_cs,[s(X),s(Sema),CritSec,Y,C],
	       [X,Sema,s(CritSec),Y,C]).
trans(exit_cs, [X,Sema,s(CritSec),Y,C],
	       [X,s(Sema),CritSec,s(Y),C]).
trans(restart, [X,Sema,CritSec,s(Y),ResetCtr],
	       [s(X),Sema,CritSec,Y,s(ResetCtr)]).


	       
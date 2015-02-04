
unsafe :- model_check(Tr).

model_check(Tr) :-
 initial_marking(M),
 search_unsafe(Tr,M).

initial_marking([s(NumberOfProcesses),0,0,0,0]).


unsafe_marking([_,s(X),_,s(Y),_]).


search_unsafe([],State) :- unsafe_marking(State).
search_unsafe([Action|As],InState) :-
	trans(Action,InState,NewState),
	search_unsafe(As,NewState).
	

trans(t1,[s(P1),P2,P3,P4,P5],
	       [P1,s(P2),P3,P4,P5]).
trans(t2,[s(P1),P2,P3,P4,P5],
	       [P1,P2,P3,s(P4),P5]).
trans(t3,[P1,s(P2),P3,P4,P5],
	       [P1,P2,s(s(P3)),P4,P5]).
trans(t4,[P1,P2,s(P3),P4,P5],
	       [P1,s(P2),P3,P4,P5]).
trans(t5,[P1,P2,P3,s(P4),P5],
	       [P1,P2,P3,P4,s(s(P5))]).
trans(t6,[P1,P2,P3,P4,s(P5)],
	       [P1,P2,P3,s(P4),P5]).


	       
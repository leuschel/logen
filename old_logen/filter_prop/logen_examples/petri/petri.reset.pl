
unsafe :- model_check(Tr).

model_check(Tr) :-
 initial_marking(M),
 search_unsafe(Tr,M).

initial_marking([s(0),0]).
initial_marking([0,0]).


unsafe_marking([s(s(s(_X1))),_X2]).


search_unsafe([],State) :- unsafe_marking(State).
search_unsafe([Action|As],InState) :-
	trans(Action,InState,NewState),
	search_unsafe(As,NewState).
	

trans(t1,[s(P1),P2],
	       [0,s(P2)]).
trans(t2,[P1,s(P2)],
	       [s(s(P1)),P2]).


	       
back_check(Tr) :-
 unsafe_marking(M),
 search_initial(Tr,M).
 
search_initial([],State) :- initial_marking(State).
search_initial([Action|As],InState) :-
	trans(Action,PredState,InState),
	search_initial(As,PredState).
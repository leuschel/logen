

        
go(Res) :- search(s(s(s(0))),s(s(s(0))),west,
        [state(s(s(s(0))),s(s(s(0))),west)],Res).

search(0,0,Boat,Result,Result).
search(WM,WC,east,History,Result) :-
        move_boat_west(WM,WC,NWM,NWC),
        safe(NWM,NWC),
        not_loop(NWM,NWC,west,History),
        search(NWM,NWC,west,[state(NWM,NWC,west)|History],Result).
search(WM,WC,west,History,Result) :-
        move_boat_east(WM,WC,NWM,NWC),
        safe(NWM,NWC),
        not_loop(NWM,NWC,east,History),
        search(NWM,NWC,east,[state(NWM,NWC,east)|History],Result).

safe(s(s(s(0))),WestCann) :- le(WestCann,s(s(s(0))) ).
safe(0,WestCann) :- le(WestCann,s(s(s(0))) ).
safe(W,W) :-  lt(0,W), lt(W,s(s(s(0))) ).

not_loop(WestMiss,WestCann,Boat,History) :-  /* added wrt DPPD to ensure BTA works */
    \+(loop(WestMiss,WestCann,Boat,History)).
not_loop(WestMiss,WestCann,Boat,History) :-
    fail,loop(WestMiss,WestCann,Boat,History).
    
loop(WestMiss,WestCann,Boat,History) :-
        mem(state(WestMiss,WestCann,Boat),History).

move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus2(NewWestMiss,WestMiss), NewWestCann = WestCann.
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(NewWestMiss,WestMiss), NewWestCann = WestCann.
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(NewWestMiss,WestMiss),
        plus1(NewWestCann,WestCann).
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus1(NewWestCann,WestCann).
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus2(NewWestCann,WestCann).

move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus2(WestMiss,NewWestMiss), NewWestCann = WestCann.
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(WestMiss,NewWestMiss), NewWestCann = WestCann.
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(WestMiss,NewWestMiss), plus1(WestCann,NewWestCann).
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus1(WestCann,NewWestCann).
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus2(WestCann,NewWestCann).

mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).


le(0,_). /* added wrt DPPD to avoid calling \+ gt */
le(s(X),s(Y)) :- le(X,Y).

lt(0,s(_)). /* added wrt DPPD to avoid calling \+ ge */
lt(s(X),s(Y)) :- lt(X,Y).

gt(s(X),0).
gt(s(X),s(Y)) :- gt(X,Y).

ge(0,0).
ge(s(X),0).
ge(s(X),s(Y)) :- ge(X,Y).

plus1(X,s(X)).
plus2(X,s(s(X))).

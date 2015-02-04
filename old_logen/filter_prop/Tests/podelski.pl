
% Example from "regular directional types" paper

gen([0,1]).
gen([0|X]) :- gen(X).

trans(X,Y) :-
	trans1(X,Y).
trans([1|X],[0|Y]) :- 
	trans2(X,Y).

trans1([0,1|T],[1,0|T]).
trans1([H|T],[H|T1]) :- 
	trans1(T,T1).

trans2([0],[1]).
trans2([H|T],[H|T1]) :- 
	trans2(T,T1).

reachable(X) :- 
	gen(X).
reachable(X) :- 
	reachable(Y),
	trans(Y,X).


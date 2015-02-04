/* file: ex_depth.pro */

solve([],Depth,Depth).
solve([Head|Tail],DepthSoFar,Res) :-
	claus(Head,Body),
	solve(Body,s(DepthSoFar),IntDepth),
	solve(Tail,IntDepth,Res).


claus(member(X,[X|T]),[]).
claus(member(X,[Y|T]),[member(X,T)]).

claus(inboth(X,L1,L2),[member(X,L1),member(X,L2)]).

claus(app([],L,L),[]).
claus(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).

claus(delete(X,[X|T],T),[]).
claus(delete(X,[Y|T],[Y|D]),[delete(X,T,D)]).

claus(test(A,L1,L2,Res),
	[inboth(A,L1,L2),delete(A,L1,D1),app(D1,L2,Res)]).

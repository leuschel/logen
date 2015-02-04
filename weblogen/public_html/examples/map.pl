/* file: map.pro */


map(P,[],[]).
map(P,[H|T],[PH|PT]) :-
	Call =.. [P,H,PH],
	print(Call), %% debug line
	call(Call),
	map(P,T,PT).



reduce(Func,Base,[],Base).
reduce(Func,Base,[H|T],Res) :-
	reduce(Func,Base,T,TRes),
	Call =.. [Func,H,TRes,Res],
	call(Call).


q(a,b).
q(b,c).
q(c,d).
q(d,e).

reduce_add(List,Res) :-
	reduce(add,0,List,Res).
add(X,Y,Z) :-
	Z is X + Y.


rev(L,R) :-
	rev(L,[],R).

rev([],L,L).
rev([H|T],A,R) :-
	rev(T,[H|A],R).

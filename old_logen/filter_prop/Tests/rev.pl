rev([],[]).
rev([X|Xs],Ys) :-
	rev(Xs,Ws),
	app(Ws,[X],Ys).

app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

%p(rev(_,_)).
%p(app(_,_,_)).

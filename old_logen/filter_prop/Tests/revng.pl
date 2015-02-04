rev([],[]).
rev([X|Xs],Ys) :-
	rev(Xs,Ws),
	app(Ws,[X],Ys).

app([],ng1,ng1).
app([ng2|Xs],Ys,[ng2|Zs]) :-
	app(Xs,Ys,Zs).

%p(rev(_,_)).
%p(app(_,_,_)).

/* file: transpose.pro */

/* PD query:
	transpose([[X1,X2,X3,X4,X5,X6,X7,X8,X9],
		Xr2,Xr3],Xtrm) */

transpose(Xs,[]) :-
	nullrows(Xs).
transpose(Xs,[Y|Ys]) :-
	makerow(Xs,Y,Zs),
	transpose(Zs,Ys).

makerow([],[],[]).
makerow([[X|Xs]|Ys],[X|Xs1],[Xs|Zs]) :-
	makerow(Ys,Xs1,Zs).

nullrows([]).
nullrows([[]|Ns]) :-
	nullrows(Ns).




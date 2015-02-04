/* Created by Pylogen */
/* file: transpose.pl */

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




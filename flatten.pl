flatten(X,Res) :- var(X),!,Res=X.

flatten((X,Y),Res) :- !,
	flatten(X,FX),
	((FX==fail) -> (Res=fail) 
	 ;
	 (flatten(Y,FY),
	  ((FX==true) -> (Res=FY); ((FY==true) -> (Res = FX) ; (Res = (FX,FY))))
	 )).
flatten((LHS;E), Res) :- nonvar(LHS), LHS=(I->T),!,
  flatten(I,FI),
  ((FI==fail) -> flatten(E,Res) ;
            ((FI==true) -> flatten(T,Res) ;
                (Res = (FI->FT;FE), flatten(T,FT), flatten(E,FE))
  )).
 flatten(call(X),X) :- nonvar(X), !.
%flatten((Lhs;true), LHS) :- !.
%flatten((true;RHS), RHS) :- !.
flatten((L;R), Disj) :-
	!,
	flatten(L, FL),
	flatten(R, FR), 
	(FL = true -> Disj = FR ; 
	    (
		FR = true ->
		Disj = FL
	    ;
		Disj = (FL;FR)
	    )
	).

flatten(\+(X),\+(Y)) :- flatten(X,Y),!.
flatten(when(C,Call), when(C,FCall)) :- flatten(Call,FCall), !.
flatten(X,X).

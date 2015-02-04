/* watchdog_test.pl */
/* This file is purposefully incorrectly annotated */
/* Only use in watchdog mode (which will identify the various errors)  !! */

rev(L,R) :-
	revacc(L,[],R).

revacc([],A,A).
revacc([H|X],A,R) :-
	revacc(X, [H|A], R).

rev2(L,R) :-
	revacc2(L,[],R).
revacc2([],A,A).
revacc2([H|X],A,R) :-
	revacc2(X, [H|A], R).
	
rev3(L,R) :-
	revacc3(L,[],R).
revacc3([],A,A).
revacc3(XX,A,R) :-
	(XX=[H|X] -> revacc3(X, [H|A], R) ;  fail).

/* a simple loop */
loop:- loop,loop.

/* a simple loop via findall */
loop2(X) :- findall(X, loop2(X), R).

/* incorrectly call a built-in at specialization time: */
builtinerror(X,Y) :- nl, '=..'(X,Y).
builtinerror2(X,Y) :- format(a,b).  /* for the moment this is called by logen and then an exception occurs */

/* generate infinite number of constants by memoization */
inf_nr(X) :- X1 is X+1, inf_nr(X1).

/* generate infinite number of constants by unfolding */
inf_nr2(X) :- X1 is X+1, inf_nr2(X1).

/* memo arg is not static */
filter_err(X) :- X=Y, filter_err(Y).
/* memo: middle arg is not static */
filter_err2(2,X,Z) :- X=Y, filter_err2(2,Y,Z).


if_err1(X,Y) :- (X=a -> Y=b ; Y=c). /* for a residual if it is not allowed for the then
 branch to instantiate the else branch or vice-versa */
 
 backprop_err(X) :- var(X),X=a.  /* the X=a will incorrectly instantiate var/1 */
  /* caught by the watchdog when the -wp switch is set */

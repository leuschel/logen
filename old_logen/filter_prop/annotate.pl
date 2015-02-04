:- module(annotate, [clause_solve/3, annotateClauses/3]).

% JPG - November 2003

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(canonical).
:- use_module(myterms).


annotateClauses([query(Q)|Cls],[(_ :- S)|M],[(entry :- Q, callpattern(S))|AProg]) :-
	annotateClauses(Cls,M,AProg).
annotateClauses([query(Q)|Cls],[fail|M],[(entry :- Q, callpattern(fail))|AProg]) :-
	annotateClauses(Cls,M,AProg).
annotateClauses([predicates(_)|Cls],M,AProg) :-
	annotateClauses(Cls,M,AProg).
annotateClauses([clause((H :- B),_)|Cls],M,[(H :- AnnB)|AProg]) :-
	annotateBody(B,M,M1,AnnB),
	annotateClauses(Cls,M1,AProg).
annotateClauses([],_,[]).


annotateBody((B,Bs),[(_ :- S)|M0],M1,(B,callpattern(S),AnnB)) :-
	!,
	annotateBody(Bs,M0,M1,AnnB).
annotateBody((B,Bs),[fail|M0],M1,(B,callpattern(fail),AnnB)) :-
	!,
	annotateBody(Bs,M0,M1,AnnB).
annotateBody(true,M,M,true) :-
	!.
annotateBody(B,[(_ :- S)|M],M,(B,callpattern(S))).
annotateBody(B,[fail|M],M,(B,callpattern(fail))).

clause_solve([],_,[]).
clause_solve([predicates(_)|Cls],D,Sols) :-
	!,
	clause_solve(Cls,D,Sols).
clause_solve([Cl|Cls],D,[S|Sols]) :-
	melt(Cl,clause((H :- B),_)),
	tsolve(B,D,T),
	headtype(H,T,S),
	!,
	clause_solve(Cls,D,Sols).
clause_solve([_|Cls],D,[fail|Sols]) :-
	clause_solve(Cls,D,Sols).



% tsolve(A,B,C):
%	A: clause body (only predicates in current SCC)
%	B: current approximation for all predicates
%	C: "solved" clause body.
%	
% example:  let A ; (p(X),q(X))
%	let B = [[p(a),p(b)], [q(d),q(b),q(e)]]
%	output C = (member(p(X),[p(a),p(b)]), member(q(X), [q(d),q(b),q(e)]))

tsolve(true,_,true) :-
	!.
tsolve((B,Bs),R,(T1,T2)) :-
	!,
	tsolve(B,R,T1),
	tsolve(Bs,R,T2).
tsolve(B,R,member(B,T1)) :-
	functor(B,S,N),
	headclause(S/N,R,(_ :- T)),
	!,
	melteach(T,T1).
tsolve(B,_,true) :-	% builtins assumed to succeed with no binding if no defn. exists
       sp_builtin(B).

% headtype(A,B,C):  projects solution from body to head of clause
%	A: clause head
%	B: solution for body (output from tsolve and temp goals
%	C: representation of set of instances of H

headtype(H, B1, (X :- S)) :- 
	functor(H,F,N),
	functor(X,F,N),
	findall(H,call(B1),Sols),
	canonical(X),
	canonical_each(Sols),
	pruneatoms(Sols,S,Sols).

canonical_each([]).
canonical_each([X|Xs]) :-
	canonical(X),
	canonical_each(Xs).

headclause(P/N,R,(H :- B)) :-
	member([(H :- B)],R),
	functor(H,P,N).

pruneatoms([X|Xs],Xs1,U) :-
	member(X,Xs),
	!,
	pruneatoms(Xs,Xs1,U).
pruneatoms([X|Xs],Xs1,U) :-
	subsumed(X,U),
	!,
	pruneatoms(Xs,Xs1,U).
pruneatoms([X|Xs],[X|Xs1],U) :-
	pruneatoms(Xs,Xs1,U).
pruneatoms([],[],_).

subsumed(X,[Y|_]) :-
	X \== Y,
	instanceOf(X,Y),
	!.
subsumed(X,[_|L]) :-
	subsumed(X,L).
	

	
melteach([],[]).
melteach([X|Xs],[Y|Ys]) :-
	melt(X,Y),
	melteach(Xs,Ys).
	

	
	
	
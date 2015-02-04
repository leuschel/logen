:- module(queryGenIndexed, [queryGen/2, flat_name/2]).

% Convert a program to simple query-answer form:

% Limitations.  Input program consists of definite clauses.
%               Doesn't handle metagoals like if-then-else, disjunction, bagof etc.
%               Maybe the ciaopp program parser will enable this later.

% Usage 1
% queryGen(+ProgramClauses,-QAProgram).

% +ProgramClauses - a list of the clauses to be transformed (read by readprog/2).
% -QAProgram - a list containing the transformed program clauses.
%            - first element of list a term predicates(Ps) where Ps
%              is a list of the predicates in the transformed program.
%            - remaining elements, terms clause((H :- B), Vs) where H:- B
%              is a transformed clause, Vs is a binding list with the
%              original variable names.
%            Note, clauses for a predicate might not be consecutive.
%


:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(canonical).
:- use_module(myterms).


queryGen([predicates(_)|Cls],[predicates(Qs)|QProg]) :-
	qa_read_clauses(Cls,QProg,[],1),
	getPreds(QProg,Qs).

qa_read_clauses([],QProg,QProg,_).
qa_read_clauses([clause(C,Vs)|Cs],Out,Out2,N) :- 
	melt(clause(C,Vs),clause(C1,Vs1)),
	make_qa_clauses(clause(C1,Vs1),Out,Out1,N),
	N1 is N+1,
	qa_read_clauses(Cs,Out1,Out2,N1).

addQpred(P,Ps,Ps) :-
	member(P,Ps),
	!.
addQpred(P,Ps,[P|Ps]).

make_qa_clauses(C,Out0,Out1,N) :-
	query_clauses(C,Out0,Out1,N).

query_clauses(clause(Cl,Vs),Out,Out1,I) :- 
	!,
	headBody(Cl,A,B),	% allows for Logen form
	bodylitqueries(A,B,true,Vs,Out,Out1,I,1).
query_clauses(_,Out,Out,_).

headBody((logen(_,H) :- B),H,B) :-
	!.
headBody((H :- B),H,B).

bodylitqueries(_,true,_,_,Out,Out,_,_) :-
	!.
bodylitqueries(A,B,Pre,Vs,[BjClause,QClause|Out0],Out1,I,J) :-
	removefirst(B,Bj,Bs),
	!,
	bodyAtom(Bj,Bj1),
	checkMemoed(Bj,Bj2),
	joingoals(Pre,Bj2,Pre1),
	flat_name('QUERY'(Bj1),QBj),
	QBj =.. [Q1|Xs],
	QI =.. [Q1,I],
	flat_name(QI, QBjI),
	QIJ =.. [QBjI,J],
	flat_name(QIJ,QBIJ),
	QueryBIJ =.. [QBIJ|Xs],
	flat_name(query(A),QA),
	flat_name(query(Bj1),QBNoIndex),
	functor(QBNoIndex,Q,M),
	functor(Query,Q,M),
	Query =.. [Q|Ys],
	QueryB =.. [QBIJ|Ys],
	QClause = clause((Query:- QueryB),[]),
	canonical(QClause),
	copyterm(clause((QueryBIJ :- QA,Pre),Vs),BjClause),
	canonical(BjClause),
	J1 is J+1,
	bodylitqueries(A,Bs,Pre1,Vs,Out0,Out1,I,J1).

removefirst((B,Bs),B,Bs) :-
	!.
removefirst(B,B,true).

bodyAtom(logen(_,B),B) :-
	!.
bodyAtom(B,B).

checkMemoed(logen(memo,_),true) :- 	% omit answers for memoed calls 
	!.
checkMemoed(logen(rescall,_),true) :-
	!.
checkMemoed(logen(_,B),B) :-
	!.
checkMemoed(B,B).


flat_name(A,A) :-
	sp_builtin(A),
	!.
flat_name(A,A1) :-
	A =.. [F,As],
	As =.. [G|Args],
	name(F,Fn),
	name(G,Gn),
	append(Gn,[95|Fn],An),
	name(H,An),
	A1 =.. [H|Args].


joingoals(true,Xs,Xs) :-
        !.
joingoals(Xs,true,Xs) :-
        !.
joingoals((true,Xs),Ys,Zs) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals((Xs,true),Ys,Zs) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals((X,Xs),Ys,(X,Zs)) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals(X,Xs,(X,Xs)) :-
        X =.. [F|_],
        F \== ','.

	
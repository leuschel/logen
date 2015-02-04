:- module(queryGen, [queryGen/2, flat_name/2]).

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
	qa_read_clauses(Cls,QProg,[]),
	getPreds(QProg,Qs).

qa_read_clauses([],QProg,QProg).
qa_read_clauses([clause(C,Vs)|Cs],Out,Out2) :- 
	melt(clause(C,Vs),clause(C1,Vs1)),
	make_qa_clauses(clause(C1,Vs1),Out,Out1),
	qa_read_clauses(Cs,Out1,Out2).

addQpred(P,Ps,Ps) :-
	member(P,Ps),
	!.
addQpred(P,Ps,[P|Ps]).

make_qa_clauses(C,Out0,Out1) :-
	query_clauses(C,Out0,Out1).

query_clauses(clause(Cl,Vs),Out,Out1) :- 
	!,
	headBody(Cl,A,B),	% allows for Logen form
	bodylitqueries(A,B,true,Vs,Out,Out1).
query_clauses(_,Out,Out).

headBody((logen(_,H) :- B),H,B) :-
	!.
headBody((H :- B),H,B).

bodylitqueries(_,true,_,_,Out,Out) :-
	!.
bodylitqueries(A,B,Pre,Vs,[BjClause|Out0],Out1) :-
	removefirst(B,Bj,Bs),
	!,
	bodyAtom(Bj,Bj1),
	checkMemoed(Bj,Bj2),
	joingoals(Pre,Bj2,Pre1),
	flat_name(query(Bj1),QBj),
	flat_name(query(A),QA),
	copyterm(clause((QBj :- QA,Pre),Vs),BjClause),
	canonical(BjClause),
	bodylitqueries(A,Bs,Pre1,Vs,Out0,Out1).

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

	
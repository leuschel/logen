:- module(qa_trans, [qa_trans/3, qa2file/3, qaLoad/2, myClause/3]).

% Convert a program and atomic goal to query-answer form:
% sometimes known as magic-set transformation.

% Limitations.  Input program consists of definite clauses.
%               Doesn't handle metagoals like if-then-else, disjunction, bagof etc.
%               Maybe the ciaopp program parser will enable this later.

% Usage 1
% qa_trans(+File,+Query,-QAProgram).

% +File - a filename containing the program to be transformed
% +Query - an atomic goal
% -QAProgram - a list containing the transformed program clauses.
%            - first element of list a term predicates(Ps) where Ps
%              is a list of the predicates in the transformed program.
%            - remaining elements, terms clause((H :- B), Vs) where H:- B
%              is a transformed clause, Vs is a binding list with the
%              original variable names.
%            Note, clauses for a predicate may not be consecutive.
%
% Example query (using naive reserve program)
%
%       ?- qa_trans('rev.pl', rev(_,_), Cls).
% Answer:
%       Cls = [predicates([rev_query/2,rev_ans/2,app_query/3,app_ans/3]),
%              clause((rev_query(_,_):-true),[]),
%              clause((rev_ans([],[]):-rev_query([],[])),[]),
%              clause((rev_ans([_B|_C],_D):-
%                    rev_ans(_C,_A),app_ans(_A,[_B],_D),rev_query([_B|_C],_D)),
%                    ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D]),
%              clause((app_query(_A,[_B],_D):-rev_query([_B|_C],_D),rev_ans(_C,_A)),
%                    ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D]),
%              clause((rev_query(_C,_A):-rev_query([_B|_C],_D),true),
%                    ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D]),
%              clause((app_ans([],_E,_E):-app_query([],_E,_E)),['Ys'=_E]),
%              clause((app_ans([_F|_G],_H,[_F|_I]):-
%                    app_ans(_G,_H,_I),app_query([_F|_G],_H,[_F|_I])),
%                    ['X'=_F,'Xs'=_G,'Ys'=_H,'Zs'=_I]),
%              clause((app_query(_G,_H,_I):-app_query([_F|_G],_H,[_F|_I]),true),
%                    ['X'=_F,'Xs'=_G,'Ys'=_H,'Zs'=_I])
%              ]

% Usage 2
% qa2file(+File,+Query,+Outfile).
% +File - a filename containing the program to be transformed
% +Query - an atomic goal
% +Outfile - the clauses are written to the output file using the original
%            variable names.
%            Note, clauses for a predicate may not be consecutive.


% Usage 3
% qaLoad(+File,+Query).
% Similar to above, but clauses are asserted as facts of the form
%       myClause(H,N,Vs)
% where (H :- B) is the clause and Vs is the binding list of variable names.

%?- qaLoad('../Exs/rev.pl',rev(_,_)).
%
%Number of clauses: 7
%
%Finished reading ../Exs/rev.pl
%
%yes
%?- myClause(X,Y,Z).
%
%X = rev_query(_,_),
%Y = true,
%Z = [] ? ;
%
%X = rev_ans([],[]),
%Y = rev_query([],[]),
%Z = [] ? ;
%
%X = rev_ans([_B|_C],_D),
%Y = (rev_ans(_C,_A),app_ans(_A,[_B],_D),rev_query([_B|_C],_D)),
%Z = ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D] ? ;
%
%X = app_query(_A,[_B],_D),
%Y = (rev_query([_B|_C],_D),rev_ans(_C,_A)),
%Z = ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D] ? ;
%
%X = rev_query(_C,_A),
%Y = (rev_query([_B|_C],_D),true),
%Z = ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D] ? ;
%
%X = app_ans([],_A,_A),
%Y = app_query([],_A,_A),
%Z = ['Ys'=_A] ? ;
%
%X = app_ans([_A|_B],_C,[_A|_D]),
%Y = (app_ans(_B,_C,_D),app_query([_A|_B],_C,[_A|_D])),
%Z = ['X'=_A,'Xs'=_B,'Ys'=_C,'Zs'=_D] ? ;
%
%X = app_query(_B,_C,_D),
%Y = (app_query([_A|_B],_C,[_A|_D]),true),
%Z = ['X'=_A,'Xs'=_B,'Ys'=_C,'Zs'=_D] ? ;
%
%no



:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).

:- dynamic(myClause/3).



qa_trans(F,Q,QAProg) :-
	qa_consult(F, Q, QAProg).

qa2file(F,Q,F1) :-
	qa_trans(F,Q,OutProg),
	open(F1,write,S),
	dumpProg(OutProg,S),
	close(S).
	
qaLoad(F,Q) :-
	qa_trans(F,Q,OutProg),
	retractall_fact(myClause(_,_,_)),
	assertProg(OutProg).
	
	

qa_consult(F,Q,[predicates(Ps1),clause((QQ :- true),[])|QAProg]) :-
	nl(user_error),
	readprog(F,Cls),
	qa_read_clauses(Cls,Ps,QAProg,[]),
	flat_name(query(Q),QQ),
	functor(QQ,PQ,M),
	addQpred(PQ/M,Ps,Ps1),
	nl(user_error).


qa_read_clauses([],Ps,QA,QA) :- 
	!,
	close_list(Ps).
qa_read_clauses([clause(C,Vs)|Cs],Ps,Out,Out2) :- 
	get_pred_name(C,Pred,Bodypreds),
	qa_names([Pred|Bodypreds],QApreds),
	each_memb1(QApreds,Ps),
	make_qa_clauses(clause(C,Vs),Out,Out1),
	qa_read_clauses(Cs,Ps,Out1,Out2).
qa_read_clauses([predicates(_)|Cs],Ps,Out,Out1) :- 
	qa_read_clauses(Cs,Ps,Out,Out1).


get_pred_name((H :- B),P/N,BPs) :-
	!,
	functor(H,P,N),
	body_preds(B,BPs).
get_pred_name(H ,P/N,[]) :-
	functor(H,P,N).

body_preds(true,[]) :-
	!.
body_preds((\+ B,Bs),Ps) :-
	!,
	body_preds((B,Bs),Ps).
body_preds((B,Bs),Ps) :-
	sp_builtin(B),
	!,
	body_preds(Bs,Ps).
body_preds((B,Bs),[P/N|Ps]) :-
	!,
	functor(B,P,N),
	body_preds(Bs,Ps).
body_preds(\+ B,Ps) :-
	!,
	body_preds(B,Ps).
body_preds(B,[]) :-
	sp_builtin(B),
	!.
body_preds(B,[P/N]) :-
	functor(B,P,N).

each_memb1([],_).
each_memb1([P|Ps],S) :-
	memb1(P,S),
	each_memb1(Ps,S).
	
memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).

addQpred(P,Ps,Ps) :-
	member(P,Ps),
	!.
addQpred(P,Ps,[P|Ps]).


file_suffix([],X,X).
file_suffix([X|Xs],Ys,[X|Zs]) :-
	file_suffix(Xs,Ys,Zs).

close_list([]) :-
	!.
close_list([_|X]) :-
	close_list(X).

make_qa_clauses(C,Out0,Out2) :-
	copy_term(C,D),
	ans_clause(D,Out0,Out1),
	query_clauses(C,Out1,Out2).



ans_clause(clause((A :- B),Vs),[clause((AA :- FBsQA),Vs)|Out0],Out0) :-
	!,
	answerlits(B,Bs),
	flat_name(ans(A),AA),
	flat_name(query(A),QA),
	flat_names(Bs,FBs),
	joingoals1(FBs,QA,FBsQA).
ans_clause(clause(A,Vs),[clause((AA :- QA),Vs)|Out0],Out0) :- 
	flat_name(ans(A),AA),
	flat_name(query(A),QA).

query_clauses(clause((A :- B),Vs),Out,Out1) :- 
	!,
	bodylitqueries(A,B,Vs,Out,Out1).
query_clauses(_,Out,Out).

bodylitqueries(_,true,_,Out,Out) :-
	!.
bodylitqueries(A,B,Vs,Out0,Out1) :-
	removelast(B,Bj,Bs1),
	sp_builtin(Bj),
	!,
	bodylitqueries(A,Bs1,Vs,Out0,Out1).
bodylitqueries(A,B,Vs,[BjClause|Out0],Out1) :-
	removelast(B,Bj,Bs1),
	!,
	answerlits(Bs1,Bs2),
	flat_names(Bs2,QBs2),
	flat_name(query(Bj),QBj),
	flat_name(query(A),QA),
	copy_term(clause((QBj :- QA,QBs2),Vs),BjClause),
	bodylitqueries(A,Bs1,Vs,Out0,Out1).

removelast((B,C,Bs),Bn,(B,Bs1)) :-
	!,
	removelast((C,Bs),Bn,Bs1).
removelast((B,C),C,B) :-
	!.
removelast(B,B,true).


answerlits((B,Bs),(B,Bs1)) :-
	sp_builtin(B),
	!,
	answerlits(Bs,Bs1).
answerlits((B,Bs),(ans(B),Bs1)) :-
	!,
	answerlits(Bs,Bs1).
answerlits(B,B) :-
	sp_builtin(B),
	!.
answerlits(B,ans(B)).

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


flat_names((B,Bs),(B1,Bs1)) :-
	!,
	flat_name(B,B1),
	flat_names(Bs,Bs1).
flat_names(B,B1) :-
	flat_name(B,B1).

qa_names([],[]).
qa_names([P/N|Ps],[QP/N,AP/N|Qs]) :-
	functor(A,P,N),
	flat_name(ans(A),AA),
	flat_name(query(A),QA),
	functor(QA,QP,N),
	functor(AA,AP,N),
	qa_names(Ps,Qs).



joingoals1(true,Xs,Xs) :-
	!.
joingoals1(Xs,true,Xs) :-
	!.
joingoals1((true,Xs),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((Xs,true),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((X,Xs),Ys,(X,Zs)) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1(X,Xs,(X,Xs)) :-
	X =.. [F|_],
	F \== ','.

dumpProg([predicates(_)|Cls],S) :-
	dumpClauses(Cls,S).
	
dumpClauses([clause((H :- B),Vs)|Cls],S) :-
	applyVarNames(Vs),
	write(S,(H :- B)),
	write(S,'.'),
	nl(S),
	dumpClauses(Cls,S).
dumpClauses([],_).

applyVarNames([]).
applyVarNames([N = N|Ns]) :-
	applyVarNames(Ns).

assertProg([_|Cls]) :-
	assertClauses(Cls).
	
assertClauses([]).
assertClauses([clause((H :- B),Vs)|OutProg]) :-
	assertz_fact(myClause(H,B,Vs)),
	assertClauses(OutProg).

	
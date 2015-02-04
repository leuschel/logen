:- module(qa_logen_new, [qaLogenTrans/3]).

% Convert a program and atomic goal to query-answer form:
% Takes into account Logen annotations memo, unfold, rescall, call
% Uses NEW form of annotations: logen({call,unfold, memo, rescall}, Q)


:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(readprog).
:- use_module(flatnames).


qaLogenTrans(F,Q,QAProg) :-
	qa_consult(F, Q, QAProg).


qa_consult(F,Q,[predicates(Ps),clause((Q00 :- true),[]),Q0Clause|QAProg]) :-
	nl(user_error),
	readprog(F,Cls),
	initial_flatquery(Q,Q00),
	flat_name(query(Q),QQ),
	copy_term(clause((QQ :- Q00),[]),Q0Clause),
	addPreds((QQ :- Q00),Ps),
	qa_read_clauses(Cls,Ps,QAProg,[],0),
	nl(user_error).
	
qa_read_clauses([],Ps,QA,QA,_) :- 
	!,
	close_list(Ps).
qa_read_clauses([clause((logen(_,H) :- B),Vs)|Cs],Ps,Out,Out2,I) :- 
	!,
	I1 is I+1,
	make_qa_clauses(clause((H :- B),Vs),Ps,Out,Out1,I1),
	qa_read_clauses(Cs,Ps,Out1,Out2,I1).
qa_read_clauses([_|Cs],Ps,Out,Out1,I) :- 
	qa_read_clauses(Cs,Ps,Out,Out1,I).

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

close_list([]) :-
	!.
close_list([_|X]) :-
	close_list(X).

make_qa_clauses(C,Ps,Out0,Out2,I) :-
	copy_term(C,D),
	ans_clause(D,Ps,Out0,Out1),
	query_clauses(C,Ps,Out1,Out2,I).

ans_clause(clause((A :- B),Vs),Ps,[C1|Out0],Out0) :-
	answerlits(B,Bs),
	flat_name(query(A),QA),
	joingoals1(Bs,QA,BsQA),
	C1 = clause((A :- BsQA),Vs),
	addPreds((A :- BsQA),Ps).
	
addPreds((H :- B),Ps) :-
	get_pred_name((H :- B),R1,Rs1),
	each_memb1([R1|Rs1],Ps).

query_clauses(clause((A :- B),Vs),Ps,Out,Out1,I) :- 
	!,
	bodylitqueries(A,Ps,B,Vs,Out,Out1,I).
query_clauses(_,_,Out,Out,_).

bodylitqueries(_,_,true,_,Out,Out,_) :-
	!.
bodylitqueries(A,Ps,B,Vs,[BijClause,QBjClause|Out0],Out1,I) :-
	removelast(B,logen(_,Bj),Bs1,J),
	!,
	answerlits(Bs1,Bs2),
	indexed_flatname(query,I,J,Qij),
	flat_name(query(Bj),QBj),
	QijBj =.. [Qij,Bj],
	flat_name(QijBj,QBij),
	flat_name(query(A),QA),
	copy_term(clause((QBij :- QA,Bs2),Vs),BijClause),
	copy_term(clause((QBj :- QBij),Vs),QBjClause),
	addPreds((QBj :- QA,QBij,Bs2),Ps),
	bodylitqueries(A,Ps,Bs1,Vs,Out0,Out1,I).

removelast((B,C,Bs),Bn,(B,Bs1),N) :-
	!,
	removelast((C,Bs),Bn,Bs1,N1),
	N is N1+1.
removelast((B,C),C,B,1) :-
	!.
removelast(B,B,true,1).


answerlits((logen(rescall,_),Bs),Bs1) :-
	!,
	answerlits(Bs,Bs1).
answerlits((logen(memo,_),Bs),Bs1) :-
	!,
	answerlits(Bs,Bs1).
answerlits((logen(call,B),Bs),(B,Bs1)) :-	% call to builtin
	builtin(B),
	!,
	answerlits(Bs,Bs1).
answerlits((logen(unfold,B),Bs),(B,Bs1)) :-
	!,
	answerlits(Bs,Bs1).
answerlits((B,Bs),(B,Bs1)) :-
	write(user_error,'Unknown annotation '),
	write(user_error,B),
	nl(user_error),
	answerlits(Bs,Bs1).
answerlits(logen(rescall,_),true) :-
	!.
answerlits(logen(memo,_),true) :-
	!.
answerlits(logen(call,B),B) :-
	builtin(B),
	!.
answerlits(logen(unfold,B),B) :-
	!.
answerlits(true,true) :-
	!.
answerlits(B,B) :-
	write(user_error,'Unknown annotation '),
	write(user_error,B),
	nl(user_error).
	


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

get_pred_name((H :- B),P/N,BPs) :-
	!,
	functor(H,P,N),
	body_preds(B,BPs).

body_preds(true,[]) :-
	!.
body_preds((\+ B,Bs),Ps) :-
	!,
	body_preds((B,Bs),Ps).
body_preds((B,Bs),Ps) :-
	sp_builtin(B),
	!,
	body_preds(Bs,Ps).
body_preds((call(B),Bs),Ps) :-	% call builtin 
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
body_preds(call(B),[]) :-
	sp_builtin(B),
	!.
body_preds(B,[]) :-
	sp_builtin(B),
	!.
body_preds(B,[P/N]) :-
	functor(B,P,N).
:- module(abstractBuiltins,[
		abstractCallSuccess/3,
		checkBuiltinCalls/4,
		writeInvalidCalls/2]).
		
:- use_module(setops).
:- use_module(cartprod).
:- use_module(canonical).
:- use_module(library(lists)).
:- use_module(abstractCallSucc).


abstractCallSuccess(AllTypes,ACalls,ASuccs) :-
	findall(Succ, abstractSuccess(Succ), Succs),
	findall(Call, abstractCall(Call), Calls),
	transformEachPattern(Succs, AllTypes, ASuccs),
	transformEachPattern(Calls, AllTypes, ACalls).

transformEachPattern([],_,[]).
transformEachPattern([G|Gs],Ts,Ps) :-
	functor(G,P,N),
	functor(H,P,N),
	canonical(H),
	transformPattern(G,Ts,Fs),
	transformEachPattern(Gs,Ts,Ps1),
	insertPattern((H :- Fs),Ps1,Ps).
	
transformPattern(F,Ts,Fs0) :-
	F =.. [P|Xs],
	makeDisjTypes(Xs,Ts,Ys),
	canonical(Ys),
	cartprod(Ys,Zs),
	makeAnswerList(Zs,P,Fs0).
	
makeAnswerList([],_,[]).
makeAnswerList([Z|Zs],P,[A|As]) :-
	A =.. [P|Z],
	makeAnswerList(Zs,P,As).

insertPattern((H :- Fs),[],[[(H :- Fs)]]).
insertPattern((H :- Fs),[[(H :- Gs)]|Ss],[[(H :- Hs)]|Ss]) :-
	!,
	setunion(Fs,Gs,Hs).
insertPattern(A,[[(H1 :- Fs)]|Ss],[[(H1 :- Fs)]|Ss1]) :-
	insertPattern(A,Ss,Ss1).
	
makeDisjTypes([X|Xs],Ts,[Y|Ys]) :-
	allIntersects(Ts,X,Y),
	makeDisjTypes(Xs,Ts,Ys).
makeDisjTypes([],_,[]).

allIntersects([],_,[]).
allIntersects([T|Ts],X,[T|Ys]) :-
	member(X,T),
	!,
	allIntersects(Ts,X,Ys).
allIntersects([_|Ts],X,Ys) :-
	allIntersects(Ts,X,Ys).


checkBuiltinCalls([],_,_,[]).
checkBuiltinCalls([[(Q :- Cs)]|Fs],ACalls,AllTypes,ProblemCalls) :-
	queryPred(Q,P,I,J),
	findQueryPatterns(ACalls,P,PCalls),
	!,			% some call patterns are recorded
	expandDontCares(Cs,AllTypes,Cs1,[]),
	allowedCalls(Cs1,P,PCalls,InvalidCalls,I,J),
	checkBuiltinCalls(Fs,ACalls,AllTypes,ProblemCalls1),
	setunion(InvalidCalls,ProblemCalls1,ProblemCalls).
checkBuiltinCalls([_|Fs],ACalls,AllTypes,ProblemCalls) :-
	checkBuiltinCalls(Fs,ACalls,AllTypes,ProblemCalls).
	
expandDontCares([],_,Cs,Cs).
expandDontCares([C|Cs],AllTypes,Cs0,Cs2) :-
	C =.. [P|Xs],
	convertArgs(Xs,AllTypes,Ys),
	cartprod(Ys,Zs),
	callsNoDontCares(Zs,P,Cs0,Cs1),
	expandDontCares(Cs,AllTypes,Cs1,Cs2).
	
convertArgs([],_,[]).
convertArgs([X|Xs],AllTypes,[AllTypes|Ys]) :-
	variable(X),
	!,
	convertArgs(Xs,AllTypes,Ys).
convertArgs([X|Xs],AllTypes,[[X]|Ys]) :-
	convertArgs(Xs,AllTypes,Ys).
	
callsNoDontCares([],_,Cs,Cs).
callsNoDontCares([Args|Zs],P,[Call|Cs0],Cs1) :-
	Call =.. [P|Args],
	callsNoDontCares(Zs,P,Cs0,Cs1).
	
queryPred(Q,P/N,ClauseID, LitID) :-
	functor(Q,QP,N),
	name(QP,As),
	name('_QUERY', Query),
	append(As1,Query,As),
	append(LID,[95|Rest],As1),
	name(LitID,LID),
	number(LitID),
	append(CID,[95|Rs],Rest),
	name(ClauseID,CID),
	number(ClauseID),
	name(P,Rs).
	
findQueryPatterns([[(H :- Cs)]|_],P/N,Cs) :-
	functor(H,P,N),
	!.
findQueryPatterns([_|ACalls],P/N,Cs) :-
	findQueryPatterns(ACalls,P/N,Cs).
	
allowedCalls([],_,_,[],_,_).
allowedCalls([C|Cs],P,PCalls,InvalidCalls,I,J) :-
	C =.. [_|Xs],
	legalCall(Xs,PCalls),
	!,
	%portray_clause(user_error,legalCall(Xs,PCalls)), %% Steve
	allowedCalls(Cs,P,PCalls,InvalidCalls,I,J).

allowedCalls([_|Cs],P,PCalls,InvalidCalls,I,J) :-
	write(user_output,'Invalid call to builtin: '), 
	write(user_output,P), 
	write(user_output,'. Clause number: '),
	write(user_output,I),
	write(user_output,', literal number: '),
	write(user_output,J),
	nl(user_output),
	%write(user_output, 'Allowed calls: '),
	%write(user_output,PCalls), nl(user_output),
	allowedCalls(Cs,P,PCalls,InvalidCalls1,I,J),
	setunion([invalidCall(P,I,J)],InvalidCalls1,InvalidCalls).
	
legalCall(Xs,[C|_]) :-
	C =.. [_|Ys],
	correctCallArgs(Xs,Ys),
	!.
legalCall(Xs,[_|Cs]) :-
	legalCall(Xs,Cs).
	
	
correctCallArgs([],[]).
correctCallArgs([_|Xs],[Y|Ys]) :-
	variable(Y),		% check this
	!,
	correctCallArgs(Xs,Ys).
correctCallArgs([X|Xs],[Y|Ys]) :-
	\+ variable(X),
	%subset(X,Y),
	subset(Y,X),
	correctCallArgs(Xs,Ys).
	
writeInvalidCalls(_,[]).
writeInvalidCalls(S,[C|Cs]) :-
	write(S,C),
	write(S, '.'),
	nl(S),
	writeInvalidCalls(S,Cs).
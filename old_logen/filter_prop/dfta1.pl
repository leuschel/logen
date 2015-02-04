:- module(dfta1, [dfta/5,fdfta/4,test/0, 
                 genTest/1, 
                 %writeDFTA/2, 
                 flatten_denotes/2]).

:- use_module(readprog).
:- use_module(cartprod).
:- use_module(balanced_tree).
:- use_module(library(lists)).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(sigma).
:- use_module(setops).


test :-
	fdfta('~/Desktop/Henning/parsetypes.pl','~/Desktop/Henning/mymeta.pl',outex1,d),
	fdfta('~/Research/LP/NFTA-New/dnftype.pl','~/Research/LP/Exs/dnf.pl',outex2,d),
	fdfta('Tests/matrix2.pl','Tests/trans.pl',outex3,d),
	fdfta('Tests/ringtypes.pl','Tests/podelski.pl',outex4,d),
	fdfta('Tests/matrixtype.pl','Tests/trans.pl',outex5,d),
	fdfta('Tests/pictype.pl','Tests/picsim.pl',outpic,d).

fdfta(RegTypeFile,Prog,Outfile,SDV) :-
	dfta(RegTypeFile,Prog,DFTA1,StateNames,SDV),
	open(Outfile,write,S),
	writeStateNames(S,StateNames),
	applyNames(DFTA1,StateNames,DFTA),
	writeConstrainedDFTA(S,DFTA),
	close(S).

dfta(RegTypeFile,Prog,DFTA,StateNames,SDV) :-	
	start_time,
	readprog(RegTypeFile,Cls),
	fsigma(Prog,Sigma),
	SigmaV=['$VAR'/0,'$CONST'/0|Sigma],
	makeDelta(Cls,SigmaV,Delta,SDV),
	initQMaps(Delta,QMap,QD0,StateMap,ConstMap),
	initFMap(SigmaV,FMap0),
	buildStates(QD0,SigmaV,QMap,StateMap,FMap0,QD0,FMapFinal,QDFinal),
	renameStates(QDFinal,1,root,StateNames),
	makeSmallDeltaD(SigmaV,FMapFinal,ConstMap,StateMap,DFTA,[]),
	end_time('Determinization time: ', user_output),
	writeStats(QDFinal,DFTA).

makeDelta(Cls,Sigma,Delta,d) :-
	userTransitions(Cls,Delta,DeltaAny,0,M1),
	deltaAny(Sigma,DeltaAny,[],M1,_).
makeDelta(Cls,Sigma,Delta,sd) :-
	userTransitions(Cls,Delta,DeltaAny,0,M1),
	deltaStatic(Sigma,DeltaAny,DeltaStatic,M1,M2),
	deltaAny(Sigma,DeltaStatic,[],M2,_).
makeDelta(Cls,Sigma,Delta,sdv) :-
	userTransitions(Cls,Delta,DeltaAny,0,M1),
	deltaStatic(Sigma,DeltaAny,DeltaStatic,M1,M2),
	deltaAny(Sigma,DeltaStatic,DeltaVar,M2,M3),
	deltaVar(M3,DeltaVar).
makeDelta(Cls,Sigma,Delta,dv) :-
	userTransitions(Cls,Delta,DeltaAny,0,M1),
	deltaAny(Sigma,DeltaAny,DeltaVar,M1,M2),
	deltaVar(M2,DeltaVar).
makeDelta(Cls,Sigma,Delta,sdvn) :-
	userTransitions(Cls,Delta,DeltaAny,0,M1),
	deltaNonvar(Sigma,DeltaAny,DeltaNonvar,M1,M2),
	deltaStatic(Sigma,DeltaNonvar,DeltaStatic,M2,M3),
	deltaAny(Sigma,DeltaStatic,DeltaVar,M3,M4),
	deltaVar(M4,DeltaVar).
	
userTransitions([predicates(_)|Cls],D0,D1,M0,M1) :-
	userTransitions(Cls,D0,D1,M0,M1).
userTransitions([clause(((L->R):- true),[])|Cls],[transition(M1,(L->R))|D0],D1,M0,M2) :-
	M1 is M0+1,
	userTransitions(Cls,D0,D1,M1,M2).
userTransitions([],D,D,M,M).

deltaAny([],D,D,M,M).
deltaAny([F/N|Fs],[transition(M1,(L->dynamic))|D0],D1,M0,M2) :-
	M1 is M0+1,
	anyargs(N,As),
	L=..[F|As],
	deltaAny(Fs,D0,D1,M1,M2).
	
anyargs(0,[]).
anyargs(J,[dynamic|As]) :-
	J>0,
	J1 is J-1,
	anyargs(J1,As).
	
deltaStatic([],D,D,M,M).
deltaStatic(['$VAR'/0|Fs],D0,D1,M0,M1) :-
	!,
	deltaStatic(Fs,D0,D1,M0,M1).
deltaStatic([F/N|Fs],[transition(M1,(L->static))|D0],D1,M0,M2) :-
	M1 is M0+1,
	staticargs(N,As),
	L=..[F|As],
	deltaStatic(Fs,D0,D1,M1,M2).
	
staticargs(0,[]).
staticargs(J,[static|As]) :-
	J>0,
	J1 is J-1,
	staticargs(J1,As).
	
deltaNonvar([],D,D,M,M).
deltaNonvar(['$VAR'/0|Fs],D0,D1,M0,M1) :-
	!,
	deltaNonvar(Fs,D0,D1,M0,M1).
deltaNonvar([F/N|Fs],[transition(M1,(L->nonvar))|D0],D1,M0,M2) :-
	M1 is M0+1,
	anyargs(N,As),
	L=..[F|As],
	deltaNonvar(Fs,D0,D1,M1,M2).

deltaVar(M,[transition(M,('$VAR'->var))]).

% initialise the fmap.  fmap(f,j) = a set of sets of transitions, initially empty
	
initFMap(Sigma,FMap) :-
	initFMap3(Sigma,root,FMap).
	
initFMap3([],FMap,FMap).
initFMap3([F/N|Sigma],FMap0,FMap2) :-
	initEachArg(N,F/N,FMap0,FMap1),
	initFMap3(Sigma,FMap1,FMap2).
	
initEachArg(0,_,F,F).
initEachArg(J,F,F0,F2) :-
	J>0,
	insert_tree(F0,key(F,J),[],F1),
	J1 is J-1,
	initEachArg(J1,F,F1,F2).
	

intersectAll([],[]).
intersectAll([A],A).
intersectAll([A1,A2|As],S) :-
	intersectAll([A2|As],S1),
	setintersect(A1,S1,S).
	
unionAll([],[]).
unionAll([X|Xs],Ys) :-
	unionAll(Xs,Zs),
	setunion(X,Zs,Ys).

% create the index QMap
% QMap(q,f,j) = {t1,...,tm} iff for all transitions t1...tm,
% where ti = f(qi1,..,qin) -> qi, qij = q.
% Initialise the set of states QD.

initQMaps(Delta,QMap,QD,StateMap,ConstMap) :-
	initQMaps7(Delta,root,QMap,root,ConstMap,root,StateMap),
	traverseVal_tree(ConstMap,QD1),
	makeset(QD1,QD).
	
initQMaps7([],Q,Q,QD,QD,SM,SM).
initQMaps7([transition(N,(L->R))|Delta],Q0,Q1,QD0,QD2,SM0,SM2) :-
	functor(L,C,0),
	!,
	addMapValueOrd(C,R,QD0,QD1),
	insert_tree(SM0,N,R,SM1),
	initQMaps7(Delta,Q0,Q1,QD1,QD2,SM1,SM2).
initQMaps7([transition(N,(L->R))|Delta],Q0,Q2,QD0,QD1,SM0,SM2) :-
	functor(L,F,M),
	insertMapValues(M,F/M,L,N,Q0,Q1),
	insert_tree(SM0,N,R,SM1),
	initQMaps7(Delta,Q1,Q2,QD0,QD1,SM1,SM2).

insertMapValues(0,_,_,_,Q,Q).
insertMapValues(J,F,L,N,Q0,Q2) :-
	J > 0,
	arg(J,L,Q),
	addMapValue(key(Q,F,J),N,Q0,Q1),
	J1 is J-1,
	insertMapValues(J1,F,L,N,Q1,Q2).
	
addMapValue(K,N,Q0,Q1) :-
	search_replace_tree(Q0,K,Ns,Q1,Ns1),
	!,
	setunion([N],Ns,Ns1).
addMapValue(K,N,Q0,Q1) :-
	insert_tree(Q0,K,[N],Q1).
	
addMapValueOrd(K,N,Q0,Q1) :-
	search_replace_tree(Q0,K,Ns,Q1,Ns1),
	!,
	insertOrd(N,Ns,Ns1).
addMapValueOrd(K,N,Q0,Q1) :-
	insert_tree(Q0,K,[N],Q1).
	
insertOrd(N,[],[N]).
insertOrd(N,[N|Ns],[N|Ns]) :-
	!.
insertOrd(N,[N1|Ns],[N,N1|Ns]) :-
	N @< N1,
	!.
insertOrd(N,[N1|Ns],[N1|Ns1]) :-
	insertOrd(N,Ns,Ns1).

% The main processing loop.

buildStates([],_,_,_,FMap,QD,FMap,QD) :-
	!.
buildStates(QDNew,Sigma,QMap,StateMap,FMap0,QD0,FMap2,QD2) :-
	%write('*'),nl,
	eachFunctorStates(Sigma,QDNew,QMap,StateMap,FMap0,FMap1,QD0,[],QDNew1),
	setunion(QDNew1,QD0,QD1),
	buildStates(QDNew1,Sigma,QMap,StateMap,FMap1,QD1,FMap2,QD2).
	
eachFunctorStates([],_,_,_,FMap,FMap,_,QDNew,QDNew).
eachFunctorStates([_/0|Sigma],QDNew,QMap,StateMap,FMap0,FMap1,QD,QDNew0,QDNew1) :-
	eachFunctorStates(Sigma,QDNew,QMap,StateMap,FMap0,FMap1,QD,QDNew0,QDNew1).
eachFunctorStates([F/N|Sigma],QDNew,QMap,StateMap,FMap0,FMap2,QD,QDNew0,QDNew2) :-
	N>0,
	extendFMap(N,F/N,QMap,QDNew,FMap0,FMap1,JMaps),
	cartprod(JMaps,ProdF),
	findNewStates(ProdF,QD,StateMap,QDNew0,QDNew1),
	eachFunctorStates(Sigma,QDNew,QMap,StateMap,FMap1,FMap2,QD,QDNew1,QDNew2).
	
extendFMap(0,_,_,_,FMap0,FMap0,[]).
extendFMap(J,F,QMap,QDNew,FMap0,FMap2,[JMap|Js]) :-
	J > 0,
	updateFMap(FMap0,F,J,QMap,QDNew,FMap1,JMap),
	J1 is J-1,
	extendFMap(J1,F,QMap,QDNew,FMap1,FMap2,Js).
	
updateFMap(FMap0,F,J,QMap,QDNew,FMap1,TTs) :-
	getQDMapVals(QDNew,QMap,F,J,QDMapVals),
	search_replace_tree(FMap0,key(F,J),JMap0,FMap1,JMap),
	insertQVals(QDMapVals,JMap0,JMap),
	getTTs(JMap,TTs).
	
getQDMapVals([],_,_,_,[]).
getQDMapVals([Q|QDNew],QMap,F,J,[Ts-Q|TTs]) :-
	qdmap(QMap,Q,F,J,Ts),
	getQDMapVals(QDNew,QMap,F,J,TTs).
	
insertQVals([],JMap,JMap).
insertQVals([Ts-Q|QTs],JMap0,JMap2) :-
	insertQVal(Ts,Q,JMap0,JMap1),
	insertQVals(QTs,JMap1,JMap2). 
	
insertQVal(Ts,Q,[],[Ts-[Q]]).
insertQVal(Ts,Q,[Ts-Qs|TTs],[Ts-Qs1|TTs]) :-
	!,
	insertVal(Q,Qs,Qs1).
insertQVal(Ts,Q,[Ts1-Qs|TTs],[Ts1-Qs|TTs1]) :-
	insertQVal(Ts,Q,TTs,TTs1).
	
getTTs([],[]).
getTTs([Ts-_|JMap],[Ts|TTs]) :-
	getTTs(JMap,TTs).
	
qdmap(QMap,Q,F,J,Ts) :-
	qmapVals(Q,QMap,F,J,Ts1),
	unionAll(Ts1,Ts2),
	sort(Ts2,Ts).
	
qmapVals([],_,_,_,[]).
qmapVals([Q|Qs],QMap,F,J,[TsQ|Ts]) :-
	search_tree(QMap,key(Q,F,J),TsQ),
	!,
	qmapVals(Qs,QMap,F,J,Ts).
qmapVals([_|Qs],QMap,F,J,Ts) :-
	qmapVals(Qs,QMap,F,J,Ts).
	
findNewStates([],_,_,QD,QD).
findNewStates([T|Ts],QD,StateMap,QD0,QD2) :-
	intersectAll(T,Trs),
	getStates(Trs,StateMap,S), % S is always nonempty since dynamic is present.
	checkNew(S,QD,QD0,QD1),
	findNewStates(Ts,QD,StateMap,QD1,QD2).

getStates([],_,[]).
getStates([T|Ts],StateMap,S) :-
	search_tree(StateMap,T,State),
	getStates(Ts,StateMap,S1),
	insertOrd(State,S1,S).

%checkNew([],_,QD,QD) :-
%	!.
checkNew(S,QD,QD0,QD0) :-
	member(S,QD),
	!.
checkNew(S,_,QD0,QD1) :-
	setunion([S],QD0,QD1).


	
makeSmallDeltaD([],_,_,_,D,D).
makeSmallDeltaD([F/0|Sigma],FMap,ConstMap,StateMap,[(F->S)|D0],D1) :-
	search_tree(ConstMap,F,S),
	makeSmallDeltaD(Sigma,FMap,ConstMap,StateMap,D0,D1).
makeSmallDeltaD([F/N|Sigma],FMap,ConstMap,StateMap,D0,D3) :-
	N>0,
	fmapArgs(0,N,F,FMap,FJs),
	makeDontCareTransitions(FJs,FJs1,F/N,StateMap,D0,D1),
	cartprod(FJs1,Tuples),
	makeCompactTransitions(Tuples,F,StateMap,D1,D2),
	makeSmallDeltaD(Sigma,FMap,ConstMap,StateMap,D2,D3).
	
makeCompactTransitions([],_,_,D,D).
makeCompactTransitions([T|Tuples],F,StateMap,[(L->R)|D0],D1) :-
	getQTTs(T,Qss,Ts),
	intersectAll(Ts,Trs),
	getStates(Trs,StateMap,S1),
	sort(S1,R),
	L =..[F|Qss],
	makeCompactTransitions(Tuples,F,StateMap,D0,D1).
	
fmapArgs(N,N,_,_,[]).
fmapArgs(J,N,F,FMap,[FJ1|FJs]) :-
	J < N,
	J1 is J+1,
	search_tree(FMap,key(F/N,J1),FJ1),
	fmapArgs(J1,N,F,FMap,FJs).

getQTTs([],[],[]).
getQTTs([Ts-Qs|JMap],[Qs|Qss],[Ts|TTs]) :-
	getQTTs(JMap,Qss,TTs).
	
	
	
makeDontCareTransitions(FJs,FJs1,F,StateMap,D0,D1) :-
	intersectArgs(FJs,IJs),
	findFixedArgs(FJs,IJs,1,F,StateMap,FJs1,D0,D1).
	
intersectArgs([],[]).
intersectArgs([FJ|FJs],[Ts1|TTs]) :-
	getQTTs(FJ,_,Ts),
	intersectAll(Ts,Ts1),
	intersectArgs(FJs,TTs).
	
findFixedArgs([],_,_,_,_,[],D,D).
findFixedArgs([FJ|FJs],IJs,K,F,StateMap,[FJ1|FJs1],D0,D2) :-
	removeKth(K,IJs,IJs1),
	intersectAll(IJs1,Int),
	getStates(Int,StateMap,S1),
	sort(S1,S),
	genDontCareTrans(FJ,S,F,K,StateMap,FJ1,D0,D1),
	K1 is K+1,
	findFixedArgs(FJs,IJs,K1,F,StateMap,FJs1,D1,D2).
	
genDontCareTrans([],_,_,_,_,[],D,D).
genDontCareTrans([Ts-Qs|FJ],S,F/N,J,StateMap,FJ1,[(L->S2)|D0],D2) :-
	getStates(Ts,StateMap,S1),
	sort(S1,S2),
	subset(S2,S),	% don't care condition
	!,
	functor(L,F,N),
	arg(J,L,Qs),
	dontcareVars(N,J,L),
	%genDontCare(Qs,F,J,S2,D0,D1),
	genDontCareTrans(FJ,S,F/N,J,StateMap,FJ1,D0,D2).
genDontCareTrans([X|FJ],S,F,J,StateMap,[X|FJ1],D0,D1) :-
	genDontCareTrans(FJ,S,F,J,StateMap,FJ1,D0,D1).
	
genDontCare([],_,_,_,D,D).
genDontCare([Q|Qs],F/N,J,S,[(L->S)|D0],D1) :-
	functor(L,F,N),
	arg(J,L,Q),
	%prettyvars(L), 	% does not exist in SICStus - so use dontcareVars/3
	dontcareVars(N,J,L),
	genDontCare(Qs,F/N,J,S,D0,D1).
	
dontcareVars(0,_,_).
dontcareVars(J,J,L) :-
	!,
	J1 is J-1,
	dontcareVars(J1,J,L).
dontcareVars(K,J,L) :-
	K > 0,
	arg(K,L,'$VAR'('_')),	% dont care variable
	K1 is K-1,
	dontcareVars(K1,J,L).
	
	
removeKth(1,[_|Xs],Xs) :-
	!.
removeKth(N,[X|Xs],[X|Xs1]) :-
	N1 is N-1,
	removeKth(N1,Xs,Xs1).
	
writelist([]).
writelist([X|Xs]) :-
	write(user_output,X),
	nl(user_output),
	writelist(Xs).
	
	
%---- test generation for worst case automata (see TATA book)
	
genTest(N) :-
	N > 0,
	N1 is N+1,
	makeStates(N1,Qs),
	makeTransitionsF(Qs,Fs,Gs),
	makeTransitionsG(Qs,Gs,[]),
	writeTransitions(N,Fs).
	
makeStates(0,[q]).
makeStates(J,[QJ|Qs]) :-
	J>0,
	name(q,Q),
	name(J,JN),
	append(Q,JN,QJN),
	name(QJ,QJN),
	J1 is J-1,
	makeStates(J1,Qs).
	

makeTransitionsF([q1,q],[(f(q) -> q1),(f(q) -> q)|Fs],Fs).
makeTransitionsF([Q1,Q|Qs],[(f(Q) -> Q1)|Fs0],Fs1) :-
	Q \== q,
	makeTransitionsF([Q|Qs],Fs0,Fs1).
	
makeTransitionsG([q1,q],[(g(q) -> q),(a -> q)|Fs],Fs).
makeTransitionsG([Q1,Q|Qs],[(g(Q) -> Q1)|Fs0],Fs1) :-
	Q \== q,
	makeTransitionsG([Q|Qs],Fs0,Fs1).
	
%---- end of test generation
	
renameStates([],_,S,S).
renameStates([Q|Qs],K,S0,S2) :-
	nextState(K,Qk,K1),
	insert_tree(S0,Q,Qk,S1),
	renameStates(Qs,K1,S1,S2).
	
nextState(K,Qk,K1) :-
	name(q,Pre),
	name(K,Suff),
	append(Pre,Suff,QKName),
	name(Qk,QKName),
	K1 is K+1.
	
applyNames([],_,[]).
/*
applyNames([(L->R)|Ts],SNs,[(L1->R1)|Ts1]) :-
	dontCareTrans(L),
	!,
	L=..[F|Xs],
	renameEach(Xs,Ys,SNs),
	search_tree(SNs,R,R1),
	L1=..[F|Ys],
	applyNames(Ts,SNs,Ts1).
*/
applyNames([(L->R)|Ts],SNs,[(L1->R1)|Ts1]) :-
	L=..[F|Xs],
	renameEachSet(Xs,Ys,SNs),
	search_tree(SNs,R,R1),
	L1=..[F|Ys],
	applyNames(Ts,SNs,Ts1).
	
renameEachSet([],[],_).
renameEachSet(['$VAR'(N)|Xs],['$VAR'(N)|Ys],SNs) :-	% don't care argument
	!,
	renameEachSet(Xs,Ys,SNs).
renameEachSet([Xs|Zs],[Xs1|Zs1],SNs) :-
	renameEach(Xs,Xs1,SNs),
	renameEachSet(Zs,Zs1,SNs).
	
renameEach([],[],_).
renameEach(['$VAR'(N)|Xs],['$VAR'(N)|Ys],SNs) :-	% don't care argument
	!,
	renameEach(Xs,Ys,SNs).
renameEach([X|Xs],[Y|Ys],SNs) :-
	search_tree(SNs,X,Y),
	renameEach(Xs,Ys,SNs).
	
writeTransitions(N,Fs) :-
	name(testNFTA,X),
	name(N,M),
	append(X,M,Y),
	name(F,Y),
	open(F,write,S),
	writeTrans(Fs,S),
	close(S).
	
writeTrans([],_).
writeTrans([T|Ts],S) :-
	write(S,T),
	write(S,'.'),
	nl(S),
	writeTrans(Ts,S).
	
writeStateNames(S,StateNames) :-
	traverse_tree(StateNames,SNs),
	writeStateNames2(SNs,S).
	
writeStateNames2([],_).
writeStateNames2([rec(State,Q)|SNs],S) :-
	write(S,'% '),
	write(S,Q),
	write(S, ' = '),
	write(S,State),
	nl(S),
	writeStateNames2(SNs,S).
	
writeConstrainedDFTA(_,[]).
/*
writeConstrainedDFTA(FO,[(L->R)|Xs]) :-
	dontCareTrans(L),
	!,
	flatten_denotes(denotes(L,R),Den),
	writeq(FO,Den),
	write(FO,'.'),
	nl(FO),
	writeConstrainedDFTA(FO,Xs).
	*/
writeConstrainedDFTA(FO,[(L->R)|Xs]) :-
	functor(L,F,N),
	functor(L1,F,N),
	flatten_denotes(denotes(L1,R),Den),
	makeConstraints(1,N,L,L1,Cs),
	numbervars(L1,0,_),
	writeq(FO,Den),
	writeConstraints(Cs,FO),
	writeConstrainedDFTA(FO,Xs).

dontCareTrans(L) :-
	L =.. [_|Xs],
	member('$VAR'('_'),Xs).
	
flatten_denotes(denotes(T,V),Den) :-
	T =.. [F|Xs],
	name(denotes,Dn),
	name(F,Fn),
	append(Dn,[95|Fn],DFn),
	name(DF,DFn),
	append(Xs,[V],Ys),
	Den =.. [DF|Ys].
	
makeConstraints(J,N,_,_,[]) :-
	J > N.
makeConstraints(J,N,L,L1,Cs) :-
	J =< N,
	arg(J,L,'$VAR'('_')), 	% don't care, so no need for constraint
	!,
	arg(J,L1,'$VAR'('_')),
	J1 is J+1,
	makeConstraints(J1,N,L,L1,Cs).
makeConstraints(J,N,L,L1,Cs) :-
	J =< N,
	arg(J,L,[D]), 	% singleton set, so no need for constraint
	!,
	arg(J,L1,D),
	J1 is J+1,
	makeConstraints(J1,N,L,L1,Cs).
makeConstraints(J,N,L,L1,[member(X,D)|Cs]) :-
	J =< N,
	arg(J,L,D),
	arg(J,L1,X),
	J1 is J+1,
	makeConstraints(J1,N,L,L1,Cs).
	
	
writeStats(QDFinal,DFTA) :-
	nl(user_output),
	length(QDFinal,QK),
	write(user_output,'Number of states :'), 
	write(QK),
	nl(user_output),
	%writelist(QDFinal),
	%nl(user_output),
	length(DFTA,K),
	write(user_output,'Number of transitions :'), 
	write(K),
	nl(user_output),
	%writelist(DFTA),
	write(user_output,'-------------'),
	nl(user_output).

writeConstraints([],F0) :-
	write(F0,'.'),
	nl(F0).
writeConstraints([C|Cs],F0) :-
	write(F0,' :-'),
	nl(F0),
	write(F0,'      '),
	write(F0,C),
	writeCs(Cs,F0).
	
writeCs([],F0) :-
	write(F0,'.'),
	nl(F0).
writeCs([C|Cs],F0) :-
	write(F0,','),
	nl(F0),
	write(F0,'      '),
	write(F0,C),
	writeCs(Cs,F0).
	
	

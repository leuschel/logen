:- module(dfta, [dfta/5,fdfta/4,test/0, 
                 genTest/1, 
                 writeDFTA/2, 
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
	fdfta('Tests/matrixtype.pl','Tests/trans.pl',outex5,d).

fdfta(RegTypeFile,Prog,Outfile,SDV) :-
	dfta(RegTypeFile,Prog,DFTA1,StateNames,SDV),
	open(Outfile,write,S),
	writeStateNames(S,StateNames),
	applyNames(DFTA1,StateNames,DFTA),
	writeDFTA(S,DFTA),
	close(S).

dfta(RegTypeFile,Prog,DFTA,StateNames,SDV) :-	
	start_time,
	readprog(RegTypeFile,Cls),
	fsigma(Prog,SigmaF),
	extraFunctors(Cls,SigmaF,Sigma),
	SigmaV=['$VAR'/0,'$CONST'/0|Sigma],
	makeDelta(Cls,SigmaV,Delta,SDV),
	%writelist(Delta),
	initQMaps(Delta,QMap,QD0,StateMap,ConstMap),
	initFMap(SigmaV,FMap0),
	buildStates(QD0,SigmaV,QMap,StateMap,FMap0,QD0,FMapFinal,QDFinal),
	renameStates(QDFinal,1,root,StateNames),
	%traverse_tree(FMapFinal,FMapList),
	%writelist(FMapList),
	%nl(user_output),
	%makeDeltaD(SigmaV,QMap,StateMap,ConstMap,QDFinal,DFTA,[]),
	%makeSmallDeltaD(SigmaV,FMapFinal,ConstMap,StateMap,DFTA,[]),
	makeReducedDeltaD(SigmaV,FMapFinal,ConstMap,StateMap,DFTA,[]),
	%writelist(DFTA),
	end_time('Determinization time: ', user_error),
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


extraFunctors([predicates(_)|Cls],Sig0,Sig1) :-
	extraFunctors(Cls,Sig0,Sig1).
extraFunctors([clause(((L->_):- true),[])|Cls],Sig0,Sig1) :-
	functor(L,F,N),
	member(F/N,Sig0),
	!,
	extraFunctors(Cls,Sig0,Sig1).
extraFunctors([clause(((L->_):- true),[])|Cls],Sig0,Sig1) :-
	functor(L,F,N),
	extraFunctors(Cls,[F/N|Sig0],Sig1).
extraFunctors([],Sig,Sig).

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
	
deltaNonvar([],D,D,M,M).
deltaNonvar(['$VAR'/0|Fs],D0,D1,M0,M1) :-
	!,
	deltaNonvar(Fs,D0,D1,M0,M1).
deltaNonvar([F/N|Fs],[transition(M1,(L->nonvar))|D0],D1,M0,M2) :-
	M1 is M0+1,
	anyargs(N,As),
	L=..[F|As],
	deltaNonvar(Fs,D0,D1,M1,M2).
	
staticargs(0,[]).
staticargs(J,[static|As]) :-
	J>0,
	J1 is J-1,
	staticargs(J1,As).

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
	write(user_error,'*'),nl(user_error),
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

makeDeltaD([],_,_,_,_,D,D).
makeDeltaD([F/0|Sigma],QMap,StateMap,ConstMap,QD,[(F->S)|D0],D1) :-
	search_tree(ConstMap,F,S),
	makeDeltaD(Sigma,QMap,StateMap,ConstMap,QD,D0,D1).
makeDeltaD([F/N|Sigma],QMap,StateMap,ConstMap,QD,D0,D2) :-
	N>0,
	makeTransitions(F/N,QMap,StateMap,QD,D0,D1),
	makeDeltaD(Sigma,QMap,StateMap,ConstMap,QD,D1,D2).
	
makeTransitions(F/N,QMap,StateMap,QD,D0,D1) :-
	argTuples(N,QD,QDN),
	cartprod(QDN,AllArgs),
	genTransitions(AllArgs,F/N,QMap,StateMap,D0,D1).
	
argTuples(0,_,[]).
argTuples(J,QD,[QD|Args]) :-
	J > 0,
	J1 is J-1,
	argTuples(J1,QD,Args).
	
genTransitions([],_,_,_,D,D).
genTransitions([Qs|AllArgs],F/N,QMap,StateMap,[(L->S)|D0],D1) :-
	L =.. [F|Qs],
	qdMapArgs(Qs,QMap,F/N,0,Ts),
	intersectAll(Ts,Trs),
	getStates(Trs,StateMap,S1),
	sort(S1,S),
	genTransitions(AllArgs,F/N,QMap,StateMap,D0,D1).
	
qdMapArgs([],_,_,_,[]).
qdMapArgs([Q|Qs],QMap,F,J,[Ts|TTs]) :-
	J1 is J+1,
	qdmap(QMap,Q,F,J1,Ts),
	qdMapArgs(Qs,QMap,F,J1,TTs).
	
makeSmallDeltaD([],_,_,_,D,D).
makeSmallDeltaD([F/0|Sigma],FMap,ConstMap,StateMap,[(F->S)|D0],D1) :-
	search_tree(ConstMap,F,S),
	makeSmallDeltaD(Sigma,FMap,ConstMap,StateMap,D0,D1).
makeSmallDeltaD([F/N|Sigma],FMap,ConstMap,StateMap,D0,D2) :-
	N>0,
	fmapArgs(0,N,F,FMap,FJs),
	cartprod(FJs,Tuples),
	makeCompactTransitions(Tuples,F,StateMap,D0,D1),
	makeSmallDeltaD(Sigma,FMap,ConstMap,StateMap,D1,D2).
	
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
	
	
makeReducedDeltaD([],_,_,_,D,D).
makeReducedDeltaD([F/0|Sigma],FMap,ConstMap,StateMap,[(F->S)|D0],D1) :-
	search_tree(ConstMap,F,S),
	makeReducedDeltaD(Sigma,FMap,ConstMap,StateMap,D0,D1).
makeReducedDeltaD([F/N|Sigma],FMap,ConstMap,StateMap,D0,D3) :-
	N>0,
	fmapArgs(0,N,F,FMap,FJs),
	makeDontCareTransitions(FJs,FJs1,F/N,StateMap,D0,D1),
	cartprod(FJs1,Tuples),
	enumerateTransitions(Tuples,F,StateMap,D1,D2),
	makeReducedDeltaD(Sigma,FMap,ConstMap,StateMap,D2,D3).
	
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
genDontCareTrans([Ts-Qs|FJ],S,F,J,StateMap,FJ1,D0,D2) :-
	getStates(Ts,StateMap,S1),
	sort(S1,S2),
	subset(S2,S),
	!,
	genDontCare(Qs,F,J,S2,D0,D1),
	genDontCareTrans(FJ,S,F,J,StateMap,FJ1,D1,D2).
genDontCareTrans([X|FJ],S,F,J,StateMap,[X|FJ1],D0,D1) :-
	genDontCareTrans(FJ,S,F,J,StateMap,FJ1,D0,D1).
	
/*
genDontCare([],_,_,_,D,D).
genDontCare([Q|Qs],F/N,J,S,[(L->S)|D0],D1) :-
	functor(L,F,N),
	arg(J,L,Q),
	numbervars(L,0,_),
	genDontCare(Qs,F/N,J,S,D0,D1).
*/
	
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
	
	
	
enumerateTransitions([],_,_,D,D).
enumerateTransitions([T|Tuples],F,StateMap,D0,D2) :-
	getQTTs(T,Qss,Ts),
	intersectAll(Ts,Trs),
	getStates(Trs,StateMap,S1),
	sort(S1,R),
	cartprod(Qss,As),
	enumAllTrans(As,R,F,D0,D1),
	enumerateTransitions(Tuples,F,StateMap,D1,D2).
	
enumAllTrans([],_,_,D,D).
enumAllTrans([A|As],R,F,[(L->R)|D0],D1) :-
	L=..[F|A],
	enumAllTrans(As,R,F,D0,D1).
	
removeKth(1,[_|Xs],Xs) :-
	!.
removeKth(N,[X|Xs],[X|Xs1]) :-
	N1 is N-1,
	removeKth(N1,Xs,Xs1).
	
%writelist([]).
%writelist([X|Xs]) :-
	%write(user_output,X),
	%nl(user_output),
	%writelist(Xs).
	
	
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
applyNames([(L->R)|Ts],SNs,[(L1->R1)|Ts1]) :-
	L=..[F|Xs],
	renameEach(Xs,Ys,SNs),
	search_tree(SNs,R,R1),
	L1=..[F|Ys],
	applyNames(Ts,SNs,Ts1).
	
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
	
writeDFTA(_,[]).
writeDFTA(FO,[(L->R)|Xs]) :-
	flatten_denotes(denotes(L,R),Den),
	writeq(FO,Den),write(FO,'.'),nl(FO),
	writeDFTA(FO,Xs).
	
flatten_denotes(denotes(T,V),Den) :-
	T =.. [F|Xs],
	name(denotes,Dn),
	name(F,Fn),
	append(Dn,[95|Fn],DFn),
	name(DF,DFn),
	append(Xs,[V],Ys),
	Den =.. [DF|Ys].
	
writeStats(QDFinal,DFTA) :-
	nl(user_error),
	length(QDFinal,QK),
	write(user_error,'Number of states :'), 
	write(user_error,QK),
	nl(user_error),
	%writelist(QDFinal),
	%nl(user_output),
	length(DFTA,K),
	write(user_error,'Number of transitions :'), 
	write(user_error,K),
	nl(user_error),
	%writelist(DFTA),
	write(user_error,'-------------'),
	nl(user_error).

	

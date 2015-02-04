:- module(builtinSoln, 
	[builtinSoln/3, 
	builtinModel/4]).

:- use_module(library(ciaopp)).
:- use_module(program(assrt_db), 
	[assertion_read/9, 
	assertion_body/7]).
:- use_module(typeslib(typeslib),
 	[is_ground_type/1]).
:- use_module(program(p_unit),
	[program/2]).
:- use_module(plai('domains/typeslib'),
	[get_type_defs/1]).  	
	
:- use_module(library(lists)).
%:- use_module(library(terms_check)).
:- use_module(library(terms)).


:- use_module(cartprod).
:- use_module(dfta).
:- use_module(readprog).
:- use_module(domainProg).
:- use_module(tp).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(setops).


builtinModel(F,Cs,Ss,NFTA) :-
	ciaopp_readprog(F,Cls),
	findTheBuiltins(Cls,Bs),
	buildCallSuccessTypes(Bs,Cs,[],Ss,[]),
	get_type_defs(AllDefs),
	transformTypeDefns(AllDefs,NFTA,[],Basics,[]),
	makeset(Basics,Basics1),
	writeBuiltinTypes(F,NFTA,Cs,Ss,Basics1).
	
ciaopp_readprog(F,Cls) :-
	module(F),
	program(P1,D1),
	jpg_program_format(P1,D1,Cls).
	
findTheBuiltins([predicates(Ps)|Cls],Bs) :-
	definedPreds(Cls,Qs),
	setdiff(Ps,Qs,Bs).
	
buildCallSuccessTypes([P/N|Bs],Cs0,Cs2,Ss0,Ss2) :-
	functor(Goal,P,N),
	callAndSuccessInfo(Goal,CSs),
	makeCallSuccessTypes(CSs,P/N,Cs0,Cs1,Ss0,Ss1),
	buildCallSuccessTypes(Bs,Cs1,Cs2,Ss1,Ss2).
buildCallSuccessTypes([],Cs,Cs,Ss,Ss).

% get info from assertion database
callAndSuccessInfo(Goal,CSs) :-
	findall((Goal,Call,Succ),
		(assertion_read(Goal,_M,_Status,success,Body,_VarNames,_S,_LB,_LE),
		 assertion_body(Goal,_Compat,Call,Succ,_Comp,_Comm,Body)),
		 CSs).	

makeCallSuccessTypes([],P/N,[calltype(CGoal)|Cs0],Cs0,
			[successtype(Goal)|Ss0],Ss0) :- 	% no results available
	functor(Goal,P,N),
	functor(CGoal,P,N),
	makeCallType(N,Goal,CGoal,[]),
	makeSuccessType(N,Goal,[],[]).
makeCallSuccessTypes([(Goal,Call,Succ)|CSs],P/N,[calltype(CGoal)|Cs0],Cs1,
			[successtype(Goal)|Ss0],Ss1) :-
	functor(CGoal,P,N),
	makeCallType(N,Goal,CGoal,Call),
	makeSuccessType(N,Goal,Call,Succ),
	makeCallSuccessTypes1(CSs,P/N,Cs0,Cs1,Ss0,Ss1).
	
makeCallSuccessTypes1([],_,Cs0,Cs0,Ss0,Ss0).
makeCallSuccessTypes1([(Goal,Call,Succ)|CSs],P/N,[calltype(CGoal)|Cs0],Cs1,
			[successtype(Goal)|Ss0],Ss1) :-
	functor(CGoal,P,N),
	makeCallType(N,Goal,CGoal,Call),
	makeSuccessType(N,Goal,Call,Succ),
	makeCallSuccessTypes1(CSs,P/N,Cs0,Cs1,Ss0,Ss1).


makeSuccessType(0,_,_,_).
makeSuccessType(J,Goal,Call,Succ) :- 	
	arg(J,Goal,Xj),
	member(T,Succ),
	T =.. [Q,Y],
	Xj == Y,
	!,						% Xj has a success type
	Xj=Q,
	J1 is J-1,
	makeSuccessType(J1,Goal,Call,Succ).
makeSuccessType(J,Goal,Call,Succ) :- 	
	arg(J,Goal,Xj),
	member(T,Call),
	T =.. [Q,Y],
	Xj == Y,
	!,						% Xj has a call type
	Xj=Q,
	J1 is J-1,
	makeSuccessType(J1,Goal,Call,Succ).
makeSuccessType(J,Goal,Call,Succ) :- 
	arg(J,Goal,dynamic),			% otherwise dynamic
	J1 is J-1,
	makeSuccessType(J1,Goal,Call,Succ).
	
makeCallType(0,_,_,_).
makeCallType(J,Goal,CGoal,Call) :- 	
	arg(J,Goal,Xj),
	member(T,Call),
	T =.. [Q,Y],
	Xj == Y,
	!,	
	arg(J,CGoal,Yj),				% Xj has a call type
	Yj=Q,
	J1 is J-1,
	makeCallType(J1,Goal,CGoal,Call).
makeCallType(J,Goal,CGoal,Call) :- 
	arg(J,CGoal,dynamic),			% otherwise dynamic
	J1 is J-1,
	makeCallType(J1,Goal,CGoal,Call).
	
definedPreds([clause((H :- _),_)|Cls],[P/N|Bs]) :-
	functor(H,P,N),
	definedPreds(Cls,Bs).
definedPreds([],[]).

transformTypeDefns([],NFTA,NFTA,Bs,Bs).
transformTypeDefns([(:-typedef(::=(T,TDef)))|Defs],NFTA0,NFTA2,Bs0,Bs2) :-
	transitionForm(TDef,T,NFTA0,NFTA1,Bs0,Bs1),
	transformTypeDefns(Defs,NFTA1,NFTA2,Bs1,Bs2).

transitionForm(([Q];R2),T,[([Q1|niltype] -> T),([] -> niltype)|NFTA0],NFTA1,Bs0,Bs2) :-
	!,
	checkBasicType(Q,Q1,Bs0,Bs1),
	transitionForm(R2,T,NFTA0,NFTA1,Bs1,Bs2).
transitionForm((R1;R2),T,[(R -> T)|NFTA0],NFTA1,Bs0,Bs2) :-
	!,
	checkBasicType(R1,R,Bs0,Bs1),
	transitionForm(R2,T,NFTA0,NFTA1,Bs1,Bs2).
transitionForm([Q],T,[([Q1|niltype] -> T),([] -> niltype)|NFTA0],NFTA0,Bs0,Bs1) :-
	!,
	checkBasicType(Q,Q1,Bs0,Bs1).
transitionForm(R1,T,[(R -> T)|NFTA0],NFTA0,Bs0,Bs1) :-
	checkBasicType(R1,R,Bs0,Bs1).
	
checkBasicType(^(R),R,Bs,Bs) :-
	!.
checkBasicType(R,basictype(R),[basictype(R)|Bs],Bs).
	
builtinSoln(_G,Dom,Ss) :-
	%getBuiltinSuccessTypes(G,STypes),
	findIntersects(_STypes,Dom,ITypeList),
	productArgs(ITypeList,Ss,[]).
	
findIntersects([],_,[]).
findIntersects([S|STypes],D,[S1|STypes1]) :-
	S =.. [P|Xs],
	findEachArgIntersects(Xs,D,Ys),
	S1 =.. [P|Ys],
	findIntersects(STypes,D,STypes1).

findEachArgIntersects([],_,[]).
findEachArgIntersects([X|Xs],D,[Y|Ys]) :-
	allSupersets(D,X,Y),
	findEachArgIntersects(Xs,D,Ys).
	
allSupersets([],_,[]).
allSupersets([D|Ds],X,[D|Ds1]) :-
	subset(X,D),
	!,
	allSupersets(Ds,X,Ds1).
allSupersets([_|Ds],X,Ds1) :-
	allSupersets(Ds,X,Ds1).
	
productArgs([],Ss,Ss).
productArgs([I|Is],Ss0,Ss2) :-
	I =.. [P|Xs],
	cartprod(Xs,Zs),
	buildSolnAtoms(Zs,P,Ss0,Ss1),
	productArgs(Is,Ss1,Ss2).
	
	
buildSolnAtoms([],_,Ss,Ss).
buildSolnAtoms([Z|Zs],P,[S|Ss0],Ss1) :-
	S =.. [P|Z],
	buildSolnAtoms(Zs,P,Ss0,Ss1).


writeBuiltinTypes(F,NFTA,Cs,Ss,Bs) :-
	name(F,FName),
	name('.imports',Imports),
	append(FName,Imports,GName),
	name(G,GName),
	open(G,write,S),
	write(S,'% Type definitions'), nl(S), nl(S),
	writeFacts(S,NFTA), nl(S),
	write(S,'% Success types'), nl(S), nl(S),
	writeFacts(S,Ss), nl(S),
	write(S,'% Call types'), nl(S), nl(S),
	writeFacts(S,Cs), nl(S),
	write(S,'% Basic types'), nl(S), nl(S),
	writeFacts(S,Bs),
	close(S).
	
writeFacts(_,[]).
writeFacts(FO,[X|Xs]) :-
	writeq(FO,X),write(FO,'.'),nl(FO),
	writeFacts(FO,Xs).

	
	
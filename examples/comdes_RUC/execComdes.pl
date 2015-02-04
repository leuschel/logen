:- module(execComdes,_).

% clauses are either (a) box definitions, (b) machine initialisations
% (c) stream maps, or (d) actions.  
	
:- dynamic(airPumpSystem/8).
:- dynamic(psu/2).
:- dynamic(apu/2).
:- dynamic(cu/2).
:- dynamic(cpu/2).
:- dynamic(isd11/2).
:- dynamic(osd11/2).
:- dynamic(isd21/2).
:- dynamic(isd22/2).
:- dynamic(isd23/2).

:- dynamic(osd21/2).
:- dynamic(osd22/2).
:- dynamic(osd23/2).

:- dynamic(isd31/2).
:- dynamic(isd32/2).
:- dynamic(isd33/2).
:- dynamic(isd34/2).

:- dynamic(osd31/2).
:- dynamic(osd32/2).
:- dynamic(osd33/2).
:- dynamic(osd34/2).

:- dynamic(isd41/2).
:- dynamic(isd42/2).
:- dynamic(isd43/2).
:- dynamic(isd44/2).

:- dynamic(osd41/2).
:- dynamic(osd42/2).

:- dynamic(signalCond/2).

:- dynamic(limitCheck/2).
:- dynamic(limitCheckBool/2).

:- dynamic(tyrePressureController/4).
:- dynamic(cylPressureController/3).
:- dynamic(copyStream/2).
:- dynamic(scaler1/3).
:- dynamic(scaler2/3).
:- dynamic(mux/3).

test(N) :-
	zeroList(N,L),trueList(N,B),
	exec(airPumpSystem(B,L,L,L,_,_,_,_)).
	
zeroList(0,[]).
zeroList(N,[0|Zs]) :-
	N > 0,
	N1 is N-1,
	zeroList(N1,Zs).
trueList(0,[]).
trueList(N,[true|Zs]) :-
	N > 0,
	N1 is N-1,
	trueList(N1,Zs).

exec(G) :-
	expandDesign((G,true),Gs),
	oneStepExec(Gs).
	
oneStepExec(Gs) :-
	Gs \== [],
	oneStepExpand(Gs,Gs1),
	write(Gs),nl,nl,
	oneStepExec(Gs1).
oneStepExec([]).
	
oneStepExpand([true|Gs],Gs1) :-	
	oneStepExpand(Gs,Gs1).
oneStepExpand([G|Gs],[G1|Gs1]) :-	
	component(G,P,G1),
	call(P),
	%write(P),nl,
	oneStepExpand(Gs,Gs1).
oneStepExpand([],[]).

expandDesign((G,Gs),Gs2) :-
	expandDesign(G,Bs1),
	expandDesign(Gs,Gs1),
	app(Bs1,Gs1,Gs2).
expandDesign(true,[]).
expandDesign(G,Bs1) :-
	box(G),
	clause(G,Bs),
	expandDesign(Bs,Bs1).
expandDesign(G,[InitG]) :-
	machine(G,InitG).
expandDesign(G,[G]) :-
	basicComponent(G).
	
component(G,P,G1) :-
	basicComponent(G),
	clause(G,(P,G1)). % recursive stream handler
component(G,true,true) :-
	basicComponent(G),
	clause(G,true).	% base case stream handler
	
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).
	
	
%% Control System as Message Interaction Graph 
%% MIG = <U,S,M>
%% U is a set of all the function-units in the control system
%% S is a set of signals and msgs
%% M is a set of mapping, where each member maps a function-unit to a set of function units receiving messages/signals from it.


%% FU specification, FU is a member of set U.
%% FU = <X,Y,E,A>
%% X,Y are disjoint sets of input and outputs (both messages and signals)
%% X is the set of input signals into the FU. Both physical signals and messages come under X
%% Y is the set of output signals from the FU, again both physical and message signals
%% Each message may contain one or more variables encoded, with certain type for each variable
%% Type for the message is generally a union of contained variable types

box(airPumpSystem(_,_,_,_,_,_,_,_)).
box(psu(_,_)).
box(apu(_,_)).
box(cu(_,_)).
box(cpu(_,_)).
box(isd11(_,_)).
box(osd11(_,_)).

box(isd21(_,_)).
box(isd22(_,_)).
box(isd23(_,_)).

box(osd21(_,_)).
box(osd22(_,_)).
box(osd23(_,_)).

box(isd31(_,_)).
box(isd32(_,_)).
box(isd33(_,_)).
box(isd34(_,_)).

box(osd31(_,_)).
box(osd32(_,_)).
box(osd33(_,_)).
box(osd34(_,_)).

box(isd41(_,_)).
box(isd42(_,_)).
box(isd43(_,_)).
box(isd44(_,_)).

box(osd41(_,_)).
box(osd42(_,_)).

box(signalCond(_,_)).

box(limitCheck(_,_)).
box(limitCheckBool(_,_)).

basicComponent(tyrePressureController(_,_,_,_)).
basicComponent(cylPressureController(_,_,_)).
basicComponent(copyStream(_,_)).
basicComponent(scaler1(_,_,_)).
basicComponent(scaler2(_,_,_)).
basicComponent(mux(_,_,_)).

machine(dummyMachine, init).

airPumpSystem(ResetSig,TyreSelSig,TyrePressureSig,
	CylPressureSig,CPDSig,TPDSig,
	ValveSig,PumpMotorSig):-
	%% functionunit(sentlist ,receivedlist)
	psu(TyrePressureSig,TPressure),
	apu((CylPressure,ValveSig,PumpMotorSig), (PumpSwitch,ValveSwitch,CylPressureSig)),
	cpu((Reset,CPDSig,TPDSig,PressureMax), (ResetSig,TyreSelSig,CylPressure,TPressure)),
	cu((PumpSwitch,ValveSwitch), (PressureMax,TPressure,Reset,CylPressure)).



%% composition of each function unit 
%% functionunit(sentlist ,receivedlist)



psu(TyrePressureSig,TPressure):-
	isd11(TyrePressureSig,TPRawVal),
	signalCond(TPRawVal,TPRCond),
	limitCheck(TPRCond,TPChkd),
	osd11(TPChkd,TPressure).



apu((CylPressure,ValveSig,PumpMotorSig),(PumpSwitch,ValveSwitch,CylPressureSig)):-
	isd23(PumpSwitch,PSwitchVal),
	isd21(ValveSwitch,VSwitchVal),
	isd22(CylPressureSig,CPRawVal),
	limitCheckBool(PSwitchVal,PSwitchValChkd),
	signalCond(CPRawVal,CPRCond),
	limitCheck(CPRCond,CylPressureVal),
	limitCheckBool(VSwitchVal,VSwitchValChkd),
	osd21(VSwitchValChkd,ValveSig),
	osd23(PSwitchValChkd,PumpMotorSig),
	osd22(CylPressureVal,CylPressure).
	


cu((PumpSwitch,ValveSwitch),(PressureMax,TPressure,Reset,CylPressure)):-
	isd41(PressureMax,PressureMaxVal),
	isd42(TPressure,TPressureVal),
	isd43(Reset,ResetVal),
	isd44(CylPressure,CylPressureVal),
	tyrePressureController(PressureMaxVal,TPressureVal,ResetVal,VSwitchVal),
	cylPressureController(CylPressureVal,ResetVal,PSwitchVal),
	osd41(VSwitchVal,ValveSwitch),
	osd42(PSwitchVal,PumpSwitch).
	

	
tyrePressureController([W|Ws],[X|Xs],[Y|Ys],[Z|Zs]) :-
	tyrePressureControllerLogic(W,X,Y,Z),
	tyrePressureController(Ws,Xs,Ys,Zs).
tyrePressureController([],[],[],[]).

tyrePressureControllerLogic(PressureMaxVal,TPressureVal,ResetVal,VSwitchVal) :-
	PressureMaxVal<TPressureVal,
	ResetVal=false,
	VSwitchVal=true.
tyrePressureControllerLogic(PressureMaxVal,TPressureVal,_ResetVal,VSwitchVal) :-
	PressureMaxVal>=TPressureVal,
	VSwitchVal=false.
tyrePressureControllerLogic(_PressureMaxVal,_TPressureVal,ResetVal,VSwitchVal) :-
	ResetVal=true,
	VSwitchVal=false.



cylPressureController([X|Xs],[Y|Ys],[Z|Zs]) :-
	cylPressureControllerLogic(X,Y,Z),
	cylPressureController(Xs,Ys,Zs).
cylPressureController([],[],[]).

cylPressureControllerLogic(CylPressureVal,ResetVal,PSwitchVal) :-
	maxVal(Max),
	CylPressureVal < Max,
	ResetVal=false,
	PSwitchVal=true.
cylPressureControllerLogic(_CylPressureVal,ResetVal,PSwitchVal) :-
	ResetVal=true,
	PSwitchVal=false.
cylPressureControllerLogic(CylPressureVal,_ResetVal,PSwitchVal) :-
	maxVal(Max),
	CylPressureVal >= Max,
	PSwitchVal=false.


isd11(Xs,Ys) :- copyStream(Xs,Ys).
osd11(Xs,Ys) :- copyStream(Xs,Ys).

isd21(Xs,Ys) :- copyStream(Xs,Ys).
isd22(Xs,Ys) :- copyStream(Xs,Ys).
isd23(Xs,Ys) :- copyStream(Xs,Ys).

osd21(Xs,Ys) :- copyStream(Xs,Ys).
osd22(Xs,Ys) :- copyStream(Xs,Ys).
osd23(Xs,Ys) :- copyStream(Xs,Ys).

isd41(Xs,Ys) :- copyStream(Xs,Ys).
isd42(Xs,Ys) :- copyStream(Xs,Ys).
isd43(Xs,Ys) :- copyStream(Xs,Ys).
isd44(Xs,Ys) :- copyStream(Xs,Ys).

osd41(Xs,Ys) :- copyStream(Xs,Ys).
osd42(Xs,Ys) :- copyStream(Xs,Ys).



copyStream([],[]).
copyStream([X|Xs],[Y|Xs1]) :-
	X=Y,
	copyStream(Xs,Xs1).

signalCond(Xs,Ys) :- copyStream(Xs,Ys).

limitCheck(Xs,Ys) :- copyStream(Xs,Ys).
limitCheckBool(Xs,Ys) :- copyStream(Xs,Ys).

cpu((Reset,CPDSig,TPDSig,PressureMax),(ResetSig,TyreSelSig,CylPressure,TPressure)):-
	isd31(ResetSig,ResetVal),
	isd32(TyreSelSig,TyreVal),
	isd33(CylPressure,CylPressureVal),
	isd34(TPressure,TPressureVal),
	mux(ResetVal,TyreVal,PressureMaxVal),
	scaler1(ResetVal,CylPressureVal,CylPressureValScaled),
	scaler2(ResetVal,TPressureVal,TPressureValScaled),
	osd31(PressureMaxVal,PressureMax),
	osd32(CylPressureValScaled,CPDSig),
	osd33(TPressureValScaled,TPDSig),
	osd34(ResetVal,Reset).


mux([X|Xs],[Y|Ys],[Z|Zs]):-
	muxLogic(X,Y,Z),
	mux(Xs,Ys,Zs).
mux([],[],[]).

muxLogic(ResetVal,_TyreVal,PressureMaxVal):-
	ResetVal=true,
	PressureMaxVal is 0.

muxLogic(ResetVal,TyreVal,PressureMaxVal):-
	ResetVal=false,
	tyrePressure(TyreVal,PressureMaxVal).


tyrePressure(1,20).
tyrePressure(2,25).
tyrePressure(3,28).
tyrePressure(4,35).
tyrePressure(5,40).



scaler1([X|Xs],[Y|Ys],[Z|Zs]):-
	scaler1Logic(X,Y,Z),
	scaler1(Xs,Ys,Zs).
scaler1([],[],[]).

scaler1Logic(ResetVal,_CylPressureVal,CylPressureValScaled):-
	ResetVal=true,
	CylPressureValScaled is 0.

scaler1Logic(ResetVal,CylPressureVal,CylPressureValScaled):-
	ResetVal=false,
	real(CylPressureVal),
	CylPressureValScaled is CylPressureVal/2.

scaler2([X|Xs],[Y|Ys],[Z|Zs]):-
	scaler2Logic(X,Y,Z),
	scaler2(Xs,Ys,Zs).
scaler2([],[],[]).

scaler2Logic(ResetVal,_TPressureVal,TPressureValScaled):-
	ResetVal=true,
	TPressureValScaled is 0.

scaler2Logic(ResetVal,TPressureVal,TPressureValScaled):-
	ResetVal=false,
	real(TPressureVal),
	TPressureValScaled is TPressureVal/3.

isd31(Xs,Ys) :- copyStream(Xs,Ys).
isd32(Xs,Ys) :- copyStream(Xs,Ys).
isd33(Xs,Ys) :- copyStream(Xs,Ys).
isd34(Xs,Ys) :- copyStream(Xs,Ys).

osd31(Xs,Ys) :- copyStream(Xs,Ys).
osd32(Xs,Ys) :- copyStream(Xs,Ys).
osd33(Xs,Ys) :- copyStream(Xs,Ys).
osd34(Xs,Ys) :- copyStream(Xs,Ys).
	
%float>=float.
%float<float.

real(_).

maxVal(16).

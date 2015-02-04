%:- module(picsim, [simulate/3, test/1,execute/8]).
%:- use_package(debug).
%:- use_package(trace).

%:- module(picsim,_).

test(X) :-
	asm(X,Y),
	environment(X,Z),
	simulate(Y,[],Z).

simulate(Prog, Eeprom, Environment) :-
	picwrite(Prog),picnl,
	initpic(Registers,Stack,PC,W),
	picwrite('PC is '),picwrite(PC),picnl,
	picwrite('Env used for sim.: '),picwrite(Environment),picnl,
	execute(Prog, Registers, Eeprom, Stack,PC,W,0, Environment).
	
execute(Prog,Registers,Eeprom,Stack,PC,W,Clock,Environment) :-
	fetchinst(Prog,PC,I,R1,R2),
	execinst(I,R1,R2,Eeprom,Eepromout,Registers,Registerstemp,Stack,Stackout,PC,PCout,W,Wout,ClockTicks),
	picwrite('PC is '),picwrite(PCout),picwrite(' Clock is '),picwrite(Clock),picwrite(' .... Regs: '),picwrite(Registerstemp),picnl,
	NewClock is Clock + ClockTicks,
	/* CUT - used for testing*/
	/* !, */
	simulatehw(PC,Registerstemp,Registersout,NewClock, ClockTicks,Environment,NewEnvironment),
%	write('Reg for keysort '),write(Registersout),nl,
	write('PC is '),write(PCout),nl,
	keysort(Registersout, NewRegs),
	/* !, */
	execute(Prog,NewRegs,Eepromout,Stackout,PCout,Wout,NewClock,NewEnvironment).

fetchinst([pic(PC,I,R1,R2)|_Prog],PC,I,R1,R2).
fetchinst([pic(X,_,_,_)|Prog],PC,I,R1,R2):-
	X \== PC,
	fetchinst(Prog,PC,I,R1,R2).


/* Timer should be incremented at each instruction, remember overflow */

/* register bit that must the changed accordingly C,DC,Z */
execinst(addlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	Wt is W + R1,
	Wout is Wt /\ 255, /* only 8 bits */
	C is Wt >> 8, /* carry info. */
	updatez(PC,R,Rc,Wout),
	updatec(PC,Rc,Rout,C),
	PCout is PC +1,	
	picwrite('exec addlw'),picnl,
	picwrite('W now '),picwrite(Wout),picnl,
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(rvec,R1,_,E,E,R,R,S,S,PC,R1,W,W,2) :-
	PC is 0.

/* regs. C,DC,Z */
execinst(addwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec addwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wt is X + W,
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,Ru1,Rt,Wout),
	updatec(PC,Rt,Rout,C),
	PCout is PC + 1,	
	picwrite('W now '),picwrite(Wout),picnl.

execinst(addwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec addwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Yt is X + W,
	Y is Yt /\ 255,
	C is Yt >> 8,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rt2,Y),
	updatec(PC,Rt2,Rout,C),	
	PCout is NewPC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

/* regs. Z */
execinst(andlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	Wout is W /\ R1,
	updatez(PC,R,Rout,Wout),
	picwrite('exec andlw'),picnl,
	PCout is PC +1,	
	picwrite('W now '),picwrite(Wout),picnl.

/* regs Z */
execinst(andwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec andwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is X /\ W,
	updatez(PC,Ru1,Rout,Wout),
	PCout is PC + 1,	
	picwrite('W now '),picwrite(Wout),picnl.

execinst(andwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec andwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X /\ W,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,	
	picwrite('Registers now '),picwrite(Rout),picnl.
	
execinst(bcf,R1,R2,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec bcf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X /\ (255-(1<< R2)),
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

execinst(bsf,R1,R2,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec bsf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X \/ (1 << R2),
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
      PCout is NewPC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,2) :-
	picwrite('exec btfss'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	picwrite('data retv. '),picwrite(X),picnl,
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCout is PC + 2,
	picwrite('Bit set'),picnl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,1) :-
	picwrite('exec btfss'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCout is PC + 1,
	picwrite('Bit not set'),picnl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,1) :-
	picwrite('exec btfsc'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCout is PC + 1,
	picwrite('Bit set'),picnl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,2) :-
	picwrite('exec btfsc'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCout is PC + 2,
	picwrite('Bit not set'),picnl.

/* regs. aff. none */
execinst(call,R1,_,E,E,R,R,S,Sout,PC,PCout,W,W,2) :-
	picwrite('exec call'),picnl,
	PCR is PC+1,
	pushstack(S,Sout,PCR),
	PCout is R1,
	picwrite('Stack '),picwrite(Sout),picnl.

/* regs. z */
execinst(clrf,R1,_,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec clrf'),picnl,
	updatedata(PC,NewPC,R,Rt,R1,0),
	updatez(PC,Rt,Rout,0),
	PCout is NewPC+1,
	picwrite('Regs. '),picwrite(Rout),picnl.

/* regs. z */
execinst(clrw,_,_,E,E,R,Rout,S,S,PC,PCout,_,0,1) :-
	picwrite('exec clrw'),picnl,
	updatez(PC,R,Rout,0),
	PCout is PC+1,
	picwrite('Wout 0'),picnl.

/* regs. z */
execinst(comf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec comf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatez(PC,Ru1,Rout,Wout),
	PCout is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs. z */
execinst(comf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec comf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(decf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec decf _,1'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X-1) /\ 255,
	updatez(PC,Ru1,Rout,Wout),
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs. z */
execinst(decf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec decf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X - 1) /\ 255,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

/* Begin decfsz */
execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	X == 1,  %was X 
	picwrite('exec decfsz _,0 result 0'),picnl,
	Wout is X - 1, %will be 0
	PCout is PC + 2.

execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	X \== 1,
	picwrite('exec decfsz _,0 not 0'),picnl,
	Wout is (X - 1) /\ 255,
	PCout is PC + 1.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	X is 1,
	Y is X - 1,
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 2,
	picwrite('exec decfsz _,1 result 0'),picnl.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	X \== 1, %was X \== 0
	Y is (X - 1) /\ 255,
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 1,
	picwrite('exec decfsz _,1 not 0'),picnl.
/* END decfsz */

execinst(goto,R1,_,E,E,R,R,S,S,_,R1,W,W,2) :-
	picwrite('exec goto'),picnl.

/* Begin incfsz */
execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255, /* only 8 bit values: 256=0*/
	Wout == 0,
	PCout is PC + 2,
	picwrite('exec incfsz _,0 result 0'),picnl.

execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	Wout > 0,
	PCout is PC + 1,
	picwrite('exec incfsz _,0 not 0'),picnl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y == 0,
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 2,
	picwrite('exec incfsz _,1 result 0'),picnl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y > 0,
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 1,
	picwrite('exec incfsz _,1 not 0'),picnl.

execinst(incf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	PCout is PC + 1,
	updatez(PC,Ru1,Rout,Wout),
	picwrite('exec incf _,0'),picnl.

execinst(incf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,
	picwrite('exec incf _,1'),picnl.

/* END incfsz */

/* regs z */
execinst(iorlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec iorlw'),picnl,
	Wout is W \/ R1,
	updatez(PC,R,Rout,Wout),
	PCout is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.


/* regs z */
execinst(iorwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec iorwf _,0'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is W \/ X,
	updatez(PC,Ru1,Rout,Wout),
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(iorwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec iorwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is W \/ X,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,	
	picwrite('Regs '),picwrite(Rout),picnl.

/* regs z */
execinst(movf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec movf _,0'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,Wout),
	updatez(PC,Ru1,Rout,Wout),
	picwrite('Wout is '),picwrite(Wout),picnl.
	
	
execinst(movf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec movf _,1'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,Y),
	updatez(PC,Ru1,Rout,Y),
	picwrite('Regs is '),picwrite(R),picnl.

execinst(movlw,R1,_,E,E,R,R,S,S,PC,PCout,_,R1,1) :-
	PCout is PC+1,
	picwrite('exec movlw'),picnl,
	picwrite('W is '),picwrite(R1),picnl.

execinst(movwf,R1,_,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	updatedata(PC,NewPC,R,Rout,R1,W),	
	PCout is NewPC+1,	
	picwrite('exec movwf'),picnl,
	picwrite(Rout),picnl.

execinst(nop,_,_,E,E,R,R,S,S,PC,PCout,W,W,1) :-
	picwrite('exec nop'),picnl,
	PCout is PC + 1.

execinst(retlw,R1,_,E,E,R,R,S,Sout,PC,PCout,_,R1,2) :-
	picwrite('exec retlw'),picnl,
	popstack(S,Sout,PCout),
	%PCout is PCR,
	returnpoint(PC,Rs),
	memberReturnPoints(PCout,Rs),
	picwrite('Stack '),picwrite(Sout),picnl.

/* regs. aff. none */
execinst(return,_,_,E,E,R,R,S,Sout,PC,PCout,W,W,2) :-
	picwrite('exec return'),picnl,
	popstack(S,Sout,PCout),
	%PCout is PCR,
	returnpoint(PC,Rs),
	memberReturnPoints(PCout,Rs),
	picwrite('Stack '),picwrite(Sout),picnl.

/* reg c */
execinst(rlf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec rlf'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	retrievec(PC,Ru1,Ru2,C),
	Wt is (X << 1) + C,
	Wout is Wt /\ 255, /* 8 bit plus carry */
	Carry is Wt >> 8,
	updatec(PC,Ru2,Rout,Carry), /* set carry bit if ap. */
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(rlf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec rlf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	retrievec(PC,Ru1,Ru2,C),
	Yt is (X << 1) + C,
	Y is Yt /\ 255, /* 8 bit plus carry */
	/* set carry bit if ap. */
	Carry is Yt >> 8,
	updatedata(PC,NewPC,Ru2,Rt,R1,Y),
	updatec(PC,Rt,Rout,Carry),
	PCout is NewPC + 1,	
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(rrf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec rlf'),picnl,
	PCout is PC + 1,
	retrievec(PC,R,Ru1,C),/* get carry bit if ap. */
	retrievedata(PC,Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Wout is (X >> 1) + (C << 7), /* plus carry */
	updatec(PC,Ru2,Rout,Carry),
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(rrf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec rlf'),picnl,
	retrievec(PC,R,Ru1,C), /* get carry bit if ap. */
	retrievedata(PC,Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Y is (X >> 1) + (C << 7), /* plus carry */
	updatedata(PC,NewPC,Ru2,Rt,R1,Y),
	updatec(PC,Rt,Rout,Carry),
	PCout is NewPC + 1,	
	picwrite('Regs '),picwrite(Rout),picnl.

/* C, DC, Z */
execinst(sublw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec sublw '),picnl,
	PCout is PC + 1,
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (R1 + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,R,Rt,Wout),
	updatec(PC,Rt,Rout,C), /* set carry bit if ap. */
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(subwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec subwf _,0 '),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (X + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,Ru1,Rt,Wout),
	updatec(PC,Rt,Rout,C), /* set carry bit if ap. */
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(subwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec subwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Zt is (X + Y), 
	Z is Zt /\ 255,
	C is Zt >> 8,
	updatedata(PC,NewPC,Ru1,Rt,R1,Z),
	updatez(PC,Rt,Rt2,Z),
	updatec(PC,Rt2,Rout,C),
	PCout is NewPC + 1,	
	/* set carry bit if ap. */
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(swapf,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	picwrite('exec swapf _,0'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(swapf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec swapf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	updatedata(PC,NewPC,Ru1,Rout,R1,Y),
	PCout is NewPC + 1,	
	picwrite('Regs '),picwrite(Rout),picnl.

/* regs z */
execinst(xorlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec xorlw '),picnl,
	PCout is PC + 1,
	Wout is W # R1,
	updatez(PC,R,Rout,Wout),
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs z */
execinst(xorwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	picwrite('exec xorwf _,0'),picnl,
	PCout is PC + 1,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is W # X,
	updatez(PC,Ru1,Rout,Wout),
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(xorwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	picwrite('exec xorwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is W # X,
	updatedata(PC,NewPC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCout is NewPC + 1,	
	picwrite('Regs '),picwrite(Rout),picnl.
	

/* Register updates: Z, DC, C */

updatec(PC,R,Rout,0) :-
	retrievedata(PC,R,Ru1,3,X),
	Z is X /\ 254, /* clear bit 0 */
	updatedata(PC,_NewPC,Ru1,Rout,3,Z).
	
updatec(PC,R,Rout,Y) :-
	Y \== 0,
	retrievedata(PC,R,Ru1,3,X),
	Z is X \/ 1,  /* set bit 0 */
	updatedata(PC,_NewPC,Ru1,Rout,3,Z).

updatez(PC,R,Rout,0) :-
	retrievedata(PC,R,Ru1,3,X),
	Y is X \/ 4, /* set bit 2 */
	updatedata(PC,_NewPC,Ru1,Rout,3,Y).
	
updatez(PC,R,Rout,Y) :-
	Y \== 0,
	retrievedata(PC,R,Ru1,3,X),
	Z is X /\ 251, /* clear bit 2 */
	updatedata(PC,_NewPC,Ru1,Rout,3,Z).
	
retrievec(PC,R,Rout,C) :-
	retrievedata(PC,R,Rout,3,X),
	C is X /\ 1.

/* Retrieve data from hardware or data register */
/* Register list has the format [(RegNr,[Values],[Readpicwrite])] */


retrievedata(PC,Rin,Rout,0,X) :-
	/* INDF Register - indirect addressing */
	retrievedata(PC,Rin,Rt,4,Y),
	retrievedata(PC,Rt,Rout,Y,X).

retrievedata(PC,[],[1-([0],[r(-1,PC)])],1,X) :- 
	/* Timer register - disabled */
	X is 0.
retrievedata(PC,[1-([X|VL],[H|RWs])|R],[1-([X|VL],[r(Cc,PC),H|RWs])|R],1,X) :- 
	H =.. [_,Cc|_].
retrievedata(PC,[Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],1,X) :-
	Y \== 1,
	retrievedata(PC,R,Rout,1,X).	


retrievedata(PC,[],[4-([12],[r(-1,PC)])],4,X) :- 
	/* FSR register - undefined at reset */
	/* What happens if this i 0? */
	X is 12.
retrievedata(PC,[4-([X|VL],[H|RWs])|R],[4-([X|VL],[r(Cc,PC),H|RWs])|R],4,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],4,X) :-
	Y \== 4,
	retrievedata(PC,R,Rout,4,X).	
	

retrievedata(PC,[],[5-([0],[r(-1,PC)])],5,X) :- 
	/* PORT A register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata(PC,[5-([X|VL],[H|RWs])|R],[5-([X|VL],[r(Cc,PC),H|RWs])|R],5,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],5,X) :-
	Y \== 5,
	retrievedata(PC,R,Rout,5,X).	


retrievedata(PC,[],[6-([0],[r(-1,PC)])],6,X) :- 
	/* PORT B register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata(PC,[6-([X|VL],[H|RWs])|R],[6-([X|VL],[r(Cc,PC),H|RWs])|R],6,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],6,X) :-
	Y \== 6,
	retrievedata(PC,R,Rout,6,X).	
	

	/* STATUS register - 0001 1xxx at reset */
retrievedata(PC,[],[3-([24],[r(-1,PC)])],3,24). 
retrievedata(PC,[3-([X|VL],[H|RWs])|R],[3-([X|VL],[r(Cc,PC),H|RWs])|R],3,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],3,X) :-
	Y \== 3,
	retrievedata(PC,R,Rout,3,X).	
	
	
/* remember to update Rin Rout */	
retrievedata(_PC,Rin,Rin,7,0). /* register 7 always read 0 */

retrievedata(PC,[],[R1-([0],[r(-1,PC)])],R1,0) :- R1 > 11. /* At the moment, a register reads 0 until it has been
									written to */

/* NOT IMPLEMENTED YET: Append 'R' to RW-list */
retrievedata(PC,[R1-([Y|VL],[H|RWs])|Rin],[R1-([Y|VL],[r(Cc,PC),H|RWs])|Rin],R1,Y) :- R1 > 11,
	H =.. [_F,Cc|_].
%	[F,Cc] ..= H.
	
retrievedata(PC,[R-(VL,RWL)|D],[R-(VL,RWL)|Rout],R1,Y):-
	R1 > 11,
	R \== R1,
	retrievedata(PC,D,Rout,R1,Y).


/* Register 10, mapped to the PC 5 most significant bits. Not directly readable but writeable */
retrievedata(_PC,Rin,Rin,10,0). %Not readable
%Register can be updated	
updatedata(PC,NewPC,R,R,10,Mask) :-
	NewPC is (PC /\ 255) \/ (PC /\ ((Mask/\ 31) << 8)).
/*   */

/* Register 2 mapped to 8 lsb of PC.
retrievedata(PC,Rin,Rin,2,RtPC) :-
	RtPC is PC /\ 255.
	 
updatedata(PC,NewPC,R,R,2,Mask) :-
	NewPC is ((PC /\ 992) \/ Mask).



/* No eemprom simulated yet */
updatedata(PC,PC,R,R,8,_).
updatedata(PC,PC,R,R,9,_).
updatedata(PC,PC,R,R,11,_).


retrievedata(_PC,Rin,Rin,8,0). 
retrievedata(_PC,Rin,Rin,9,0). 
retrievedata(_PC,Rin,Rin,11,0). 


/* End eeprom routines*/


/* Update value in hardware or data register */
/* INDF Register - indirect addressing */
updatedata(PC,PC,R,Rout,0,X) :-
	retrievedata(PC,R,Ru1,4,Y),
	updatedata(PC,PC,Ru1,Rout,Y,X).

/* Timer register */
updatedata(PC,PC,[], [1-([W],[w(PC)])], 1, W).
updatedata(PC,PC,[1-(VL,RWL)|D],[1-([W|VL],[w(PC)|RWL])|D],1,W).
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],1,W):-
	X \== 1,
	updatedata(PC,PC,D,Dout,1,W).


/* Status register */
updatedata(PC,PC,[], [3-([W],[w(PC)])], 3, W).
updatedata(PC,PC,[3-(VL,RWL)|D],[3-([W|VL],[w(PC)|RWL])|D],3,W).
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],3,W):-
	X \== 3,
	updatedata(PC,PC,D,Dout,3,W).

/* FSR register */
updatedata(PC,PC,[], [4-([W],[w(PC)])], 4, W).
updatedata(PC,PC,[4-(VL,RWL)|D],[4-([W|VL],[w(PC)|RWL])|D],4,W).
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],4,W):-
	X \== 4,
	updatedata(PC,PC,D,Dout,4,W).


/* PORT A register */
updatedata(PC,PC,[], [5-([W],[w(PC)])], 5, W).
updatedata(PC,PC,[5-(VL,RWL)|D],[5-([W|VL],[w(PC)|RWL])|D],5,W).
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],5,W):-
	X \== 5,
	updatedata(PC,PC,D,Dout,5,W).

/* PORT B register */
updatedata(PC,PC,[], [6-([W],[w(PC)])], 6, W).
updatedata(PC,PC,[6-(VL,RWL)|D],[6-([W|VL],[w(PC)|RWL])|D],6,W).
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],6,W):-
	X \== 6,
	updatedata(PC,PC,D,Dout,6,W).


updatedata(PC,PC,R,R,7,_). /* register 7 stores nothing */


updatedata(PC,PC,[], [R1-([W],[w(PC)])], R1, W) :-
	/* check boundries */
	R1 > 11,
	true.
updatedata(PC,PC,[R1-(VL,RWL)|D],[R1-([W|VL],[w(PC)|RWL])|D],R1,W) :-
	/* check again */
	R1 > 11,
	true.
updatedata(PC,PC,[X-(Y,RWL)|D],[X-(Y,RWL)|Dout],R1,W):-
	R1 > 11,
	X \== R1,
	updatedata(PC,PC,D,Dout,R1,W).


/* only 8 stack elements */
/* backtracking possible? */
%pushstack([],[X],X).
pushstack(S,[X|S],X).
	%S \== [].

/* last element is copied - always 8 elements on the stack */
popstack([],[],0).
popstack([X|S],S,X).

memberReturnPoints(X,[X|_]).
memberReturnPoints(X,[_|Xs]) :-
	memberReturnPoints(X,Xs).

initpic(Registers,Stack,PC,W) :-
W=0,
PC=0,
Stack=[],
%Registers=[],
usedRegs(Registers),
picnl.

usedRegs(Registers) :-
	regNums(Rs),
	initRegs(Rs,Registers).
	
initRegs([],[]).
initRegs([N|Ns],[N-([0],[r(-1,-1)])|Rs]) :-
	initRegs(Ns,Rs).
	
regNums([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28]).

%simulatehw(R,R,NC, CT,E,E) :- !.


/* Simulate HW */
simulatehw(PC,Rin,Rout,Clock,ClockTicks,Environment,NewEnvironment) :-
	simulateinput(PC,Rin,Rt1,Clock,Environment,NewEnvironment),
	simulatetimer(PC,Rt1,Rout,ClockTicks).

%simulateinput(R,R,CT,E,E) :- !.


/* Simulated input .... */
/* No update on input port */
simulateinput(_PC,R,R,Clock,EnvI,EnvI) :-
	headEnvInput(EnvI,(Ct,_),_),
	Ct > Clock.

/* update on input port */
simulateinput(PC,Rin,Rout,Clock,EnvI,NewEnvI) :-
	headEnvInput(EnvI,(Ct,V),NewEnvI),
	Ct =< Clock,
	%removeHeadEnvInput(EnvI,NewEnvI),
	updatedata(PC,_PC,Rin,Rout,6,V).
	

%	Tick is (Clock mod 64),
%	Tick == 0,
%	retrievedata(PC,Rin,Ru1,6,X),
%	Z is X+1,
%	updatedata(PC,Ru1,Rout,6,Z).

%headEnvInput([],(0,0),[]).
headEnvInput([(Ct,V)|Xs],(Ct,V),Xs).

removeHeadEnvInput([],[]).
removeHeadEnvInput([_X|Xs],Xs).


%simulateinput(Rin,Rin,Clock) :-
%	Tick is (Clock mod 64),
%	Tick > 0.

simulatetimer(PC,Rin,Rout,Ticks) :-
	retrievedata(PC,Rin,Rt1,1,X),
	Y is (X+Ticks) mod 256,
	updatedata(PC,_PC,Rt1,Rout,1,Y).

/*
	Enable/disable writes
*/
	picwrite(X) :- write(X).
	%picwrite(_).

	picnl :- nl.
	%picnl.

	
%---------------

asm(a,X) :-
X=[pic(0,rvec,5,0),pic(5,movlw,5,0),pic(6,movwf,32,0),pic(7,movwf,32,0),pic(8,movwf,32,0),pic(9,movf,33,0),pic(10,movf,33,0),pic(11,movf,34,0),pic(12,movwf,34,0),pic(13,movwf,35,0),pic(14,movf,35,0),pic(15,nop,0,0)].



asm(b,X) :-
X=[pic(0,rvec,5,0),pic(5,movlw,10,0),pic(6,movwf,32,0),pic(7,movwf,33,0),pic(8,decfsz,32,1),pic(9,goto,7,0),pic(10,movlw,10,0),pic(11,movwf,32,0),pic(12,movf,34,0),pic(13,decfsz,32,1),pic(14,goto,12,0),pic(15,movf,35,0),pic(16,movwf,35,0),pic(17,movwf,35,0),pic(18,movwf,35,0)].


asm(c,X):-
X=[pic(0,rvec,5,0),pic(5,movlw,100,0),pic(6,movwf,20,0),pic(7,movlw,75,0),
   pic(8,movwf,21,0),pic(9,movlw,50,0),pic(10,addwf,21,0),
   pic(11,movwf,20,0),pic(12,addwf,20,0),pic(13,nop,0,0) ].



asm(1,X) :-
X=[pic(0,rvec,5,0),pic(5,movlw,130,0),pic(6,movwf,20,0),pic(7,rlf,20,1),pic(8,rlf,20,1)].

asm(3,X) :-
X=[pic(0,rvec,5,0),pic(5,movlw,32,0),pic(6,movwf,4,0),pic(7,clrf,0,0),pic(8,incf,4,1),pic(9,btfss,4,4),pic(10,goto,7,0)].

asm(2,X) :-
X=[pic(0,rvec,5,0),pic(5,goto,92,0),pic(6,movwf,17,0),pic(7,movlw,9,0),pic(8,movwf,18,0)
,pic(9,bcf,5,2),pic(10,goto,17,0),pic(11,rrf,17,1),pic(12,btfsc,3,0),pic(13,goto,16,0)
,pic(14,bcf,5,2),pic(15,goto,17,0),pic(16,bsf,5,2),pic(17,movlw,209,0),pic(18,movwf,1,0)
,pic(19,movf,1,0),pic(20,btfss,3,2),pic(22,movf,18,1),pic(23,btfsc,3,2),pic(24,return,0,0)
,pic(25,decfsz,18,1),pic(26,goto,11,0),pic(27,bsf,5,2),pic(28,goto,17,0),pic(29,btfsc,6,0)
,pic(30,goto,29,0),pic(31,movlw,232,0),pic(32,movwf,1,0),pic(33,movf,1,0),pic(34,btfss,3,2)
,pic(35,nop,0,0) /* instr. 35 missing? */
,pic(36,btfsc,6,0),pic(37,goto,29,0),pic(38,movlw,209,0),pic(39,movwf,1,0),pic(40,movlw,128,0)
,pic(41,movwf,16,0),pic(42,movlw,209,0),pic(43,movwf,1,0),pic(44,movf,1,0),pic(45,btfss,3,2)
,pic(46,goto,44,0),pic(47,rrf,16,1),pic(48,bcf,16,7),pic(49,btfsc,6,0),pic(50,bsf,16,7)
,pic(51,btfss,3,0),pic(53,movlw,209,0),pic(54,movwf,1,0),pic(55,movf,1,0),pic(56,btfss,3,2)
,pic(58,return,0,0),pic(59,movlw,7,0),pic(60,movwf,22,0),pic(61,clrf,24,0),pic(62,movlw,1,0)
,pic(63,movwf,25,0),pic(64,btfsc,5,0),pic(65,goto,64,0),pic(66,btfss,5,0),pic(67,goto,66,0)
,pic(68,decfsz,22,1),pic(69,goto,64,0),pic(70,btfsc,5,0),pic(71,goto,70,0),pic(72,btfss,5,0)
,pic(73,goto,72,0),pic(74,btfsc,5,1),pic(75,bsf,24,0),pic(76,btfsc,5,0),pic(77,goto,76,0)
,pic(78,btfss,5,0),pic(79,goto,78,0),pic(80,rlf,25,1),pic(81,bcf,25,0),pic(82,btfsc,5,1)
,pic(83,bsf,25,0),pic(84,btfss,3,0),pic(86,movlw,209,0),pic(87,movwf,1,0),pic(88,movf,1,0)
,pic(89,btfss,3,2),pic(91,return,0,0),pic(92,clrf,4,0),pic(93,bsf,5,2),pic(94,bsf,3,5)
,pic(95,movlw,3,0),pic(96,movwf,5,0),pic(97,movlw,1,0),pic(98,movwf,6,0),pic(99,movlw,1,0)
,pic(100,movwf,1,0),pic(101,bcf,3,5),pic(102,bcf,6,2),pic(103,bsf,6,4),pic(104,bsf,6,7)
,pic(105,bsf,6,5),pic(106,bsf,6,3),pic(107,bsf,5,2),pic(108,call,29,0),pic(109,movf,16,0)
,pic(110,xorlw,36,0),pic(111,btfss,3,2),pic(112,goto,108,0),pic(113,call,29,0),pic(114,movf,16,0)
,pic(115,xorlw,83,0),pic(116,btfss,3,2),pic(117,goto,108,0),pic(118,call,29,0),pic(119,movf,16,0)
,pic(120,xorlw,67,0),pic(121,btfss,3,2),pic(122,goto,108,0),pic(123,call,29,0),pic(124,movf,16,0)
,pic(125,xorlw,48,0),pic(126,btfss,3,2),pic(127,goto,108,0),pic(128,bcf,6,1),pic(129,call,59,0)
,pic(130,movlw,36,0),pic(131,call,6,0),pic(132,movlw,82,0),pic(133,call,6,0),pic(134,movlw,67,0)
,pic(135,call,6,0),pic(136,movlw,48,0),pic(137,call,6,0),pic(138,movf,24,0),pic(139,call,6,0)
,pic(140,movf,25,0),pic(141,call,6,0),pic(142,movlw,10,0),pic(143,call,6,0),pic(144,movlw,13,0)
,pic(145,call,6,0),pic(146,goto,108,0)].

%returnpoint(24,[132,134,136,138,140,142,144,146]).
%returnpoint(58,[109,114,119,124]).
%returnpoint(91,[130]).

/* Case study from Bristol. */

asm(d,X) :-
X=[pic(0,rvec,5,0),pic(5,goto,269,0),pic(6,movwf,9,0),pic(7,movlw,9,0),pic(8,movwf,10,0)
,pic(9,bcf,6,1),pic(10,goto,17,0),pic(11,rrf,9,1),pic(12,btfsc,3,0),pic(13,goto,16,0)
,pic(14,bcf,6,1),pic(15,goto,17,0),pic(16,bsf,6,1),pic(17,movlw,195,0),pic(18,movwf,1,0)
,pic(19,movf,1,0),pic(20,btfss,3,2)
,pic(21,nop,0,0) /*missing instr. */
,pic(22,movf,10,1),pic(23,btfsc,3,2),pic(24,return,0,0)
,pic(25,decfsz,10,1),pic(26,goto,11,0),pic(27,bsf,6,1),pic(28,goto,17,0),pic(29,btfsc,6,0)
,pic(30,goto,29,0),pic(31,movlw,225,0),pic(32,movwf,1,0),pic(33,movf,1,0),pic(34,btfss,3,2)
,pic(35,nop,0,0) /*missing instr. */
,pic(36,btfsc,6,0),pic(37,goto,29,0),pic(38,movlw,195,0),pic(39,movwf,1,0),pic(40,movlw,128,0)
,pic(41,movwf,8,0),pic(42,movlw,195,0),pic(43,movwf,1,0),pic(44,movf,1,0),pic(45,btfss,3,2)
,pic(46,goto,44,0),pic(47,rrf,8,1),pic(48,bcf,8,7),pic(49,btfsc,6,0),pic(50,bsf,8,7)
,pic(51,btfss,3,0)
,pic(52,nop,0,0) /*missing instr. */
,pic(53,movlw,195,0),pic(54,movwf,1,0),pic(55,movf,1,0),pic(56,btfss,3,2)
,pic(57,nop,0,0) /*missing instr. */
,pic(58,return,0,0),pic(59,call,29,0),pic(60,movf,8,0),pic(61,return,0,0),pic(62,clrf,14,0)
,pic(63,clrf,15,0),pic(64,movf,15,0),pic(65,addwf,13,0),pic(66,movwf,15,0),pic(67,incf,14,0)
,pic(68,movwf,14,0),pic(69,xorwf,12,0),pic(70,btfss,3,2),pic(71,goto,64,0),pic(72,return,0,0)
,pic(73,movlw,48,0),pic(74,subwf,16,0),pic(75,return,0,0),pic(76,movlw,10,0),pic(77,movwf,12,0)
,pic(78,movwf,16,0),pic(79,call,73,0),pic(80,movwf,13,0),pic(81,call,62,0),pic(82,movwf,16,0)
,pic(83,call,73,0),pic(84,addwf,15,0),pic(85,movlw,10,0),pic(86,movwf,12,0),pic(87,movwf,16,0)
,pic(88,call,73,0),pic(89,movwf,13,0),pic(90,call,62,0),pic(91,movwf,16,0),pic(92,call,73,0)
,pic(93,addwf,15,0),pic(94,return,0,0),pic(95,movlw,36,0),pic(96,call,6,0),pic(97,movlw,82,0)
,pic(98,call,6,0),pic(99,movlw,65,0),pic(100,call,6,0),pic(101,movlw,48,0),pic(102,call,6,0)
,pic(103,movf,18,0),pic(104,call,6,0),pic(105,movf,19,0),pic(106,call,6,0),pic(107,movf,20,0)
,pic(108,call,6,0),pic(109,movlw,59,0),pic(110,call,6,0),pic(111,movlw,10,0),pic(112,call,6,0)
,pic(113,return,0,0),pic(114,movlw,64,0),pic(115,movwf,23,0),pic(116,movwf,21,0),pic(117,clrf,22,0)
,pic(118,clrf,24,0),pic(119,clrf,27,0),pic(120,clrf,28,0),pic(121,call,233,0),pic(122,movf,18,0)
,pic(123,addlw,192,0),pic(124,movwf,18,0),pic(125,bsf,3,0),pic(126,rlf,23,0),pic(127,subwf,24,1)
,pic(128,btfss,3,0),pic(129,decf,23,1),pic(130,bcf,3,0),pic(131,rlf,18,0),pic(132,addwf,24,1)
,pic(133,btfsc,3,0),pic(134,incf,23,1),pic(135,bcf,3,0),pic(136,rrf,21,0),pic(137,movwf,20,0)
,pic(138,bcf,3,0),pic(139,rrf,20,0),pic(140,subwf,21,0),pic(141,movwf,25,0),pic(142,bcf,3,0)
,pic(143,rrf,18,0),pic(144,movwf,20,0),pic(145,bcf,3,0),pic(146,rrf,20,0),pic(147,addwf,25,1)
,pic(148,bcf,3,0),pic(149,rlf,21,0),pic(150,movwf,20,0),pic(151,bcf,3,0),pic(152,rlf,20,1)
,pic(153,bcf,3,0),pic(154,rlf,20,1),pic(155,bcf,3,0),pic(156,rlf,20,0),pic(157,bcf,3,0)
,pic(158,rlf,20,1),pic(159,bcf,3,0),pic(160,rlf,20,0),pic(161,subwf,22,0),pic(162,movwf,26,0)
,pic(163,btfss,3,0),pic(164,decf,25,1),pic(165,bcf,3,0),pic(166,rlf,18,0),pic(167,movwf,20,0)
,pic(168,bcf,3,0),pic(169,rlf,20,1),pic(170,bcf,3,0),pic(171,rlf,20,1),pic(172,bcf,3,0)
,pic(173,rlf,20,0),pic(174,bcf,3,0),pic(175,rlf,20,1),pic(176,bcf,3,0),pic(177,rlf,20,0)
,pic(178,addwf,26,1),pic(179,btfsc,3,0),pic(180,incf,25,1),pic(181,bcf,3,0),pic(182,rrf,22,0)
,pic(183,movwf,20,0),pic(184,bcf,3,0),pic(185,rrf,20,0),pic(186,subwf,26,1),pic(187,btfss,3,0)
,pic(188,decf,25,1),pic(189,movf,26,0),pic(190,movwf,22,0),pic(191,movf,25,0),pic(192,movwf,21,0)
,pic(193,movf,27,1),pic(194,btfsc,3,2)
,pic(195,nop,0,0) /*missing instr. */
,pic(196,movf,23,0),pic(197,addlw,255,0),pic(198,subwf,21,0)
,pic(199,btfss,3,0),pic(200,clrf,27,0),pic(201,goto,223,0),pic(202,movf,21,0),pic(203,addlw,255,0)
,pic(204,subwf,23,0),pic(205,btfsc,3,0)
,pic(206,nop,0,0) /*missing instr. */
,pic(207,comf,27,1),pic(208,movlw,36,0),pic(209,call,6,0)
,pic(210,movlw,82,0),pic(211,call,6,0),pic(212,movlw,7,0),pic(213,call,6,0),pic(214,incf,28,1)
,pic(215,movf,28,0),pic(216,andlw,7,0),pic(217,addlw,48,0),pic(218,call,6,0),pic(219,movlw,59,0)
,pic(220,call,6,0),pic(221,movlw,10,0),pic(222,call,6,0),pic(223,movlw,160,0),pic(224,movwf,20,0)
,pic(225,movlw,195,0),pic(226,movwf,1,0),pic(227,movf,1,0),pic(228,btfss,3,2)
,pic(229,nop,0,0) /*missing instr. */
,pic(230,decfsz,20,1)
,pic(231,goto,225,0),pic(232,goto,121,0),pic(233,clrf,17,0),pic(234,btfsc,5,1),pic(235,goto,234,0)
,pic(236,btfss,5,1),pic(237,goto,236,0),pic(238,nop,0,0),pic(239,nop,0,0),pic(240,incf,17,1)
,pic(241,btfsc,5,1),pic(242,goto,238,0),pic(243,movf,17,0),pic(244,movwf,18,0),pic(245,clrf,17,0)
,pic(246,btfsc,5,0),pic(247,goto,246,0),pic(248,btfss,5,0),pic(249,goto,248,0),pic(250,nop,0,0)
,pic(251,nop,0,0),pic(252,incf,17,1),pic(253,btfsc,5,0),pic(254,goto,250,0),pic(255,movf,17,0)
,pic(256,movwf,19,0),pic(257,nop,0,0),pic(258,nop,0,0),pic(259,incf,17,1),pic(260,btfss,5,0)
,pic(261,goto,257,0),pic(262,movf,17,0),pic(263,movwf,20,0),pic(264,movf,18,0),pic(265,movwf,18,0)
,pic(266,movf,19,0),pic(267,movwf,19,0),pic(268,return,0,0),pic(269,clrf,4,0),pic(270,bsf,6,1)
,pic(271,bsf,3,5),pic(272,movlw,3,0),pic(273,movwf,5,0),pic(274,movlw,1,0),pic(275,movwf,6,0)
,pic(276,movlw,2,0),pic(277,movwf,1,0),pic(278,bcf,3,5),pic(279,bsf,6,1),pic(280,call,29,0)
,pic(281,movf,8,0),pic(282,xorlw,36,0),pic(283,btfss,3,2),pic(284,goto,280,0),pic(285,call,29,0)
,pic(286,movf,8,0),pic(287,xorlw,83,0),pic(288,btfss,3,2),pic(289,goto,280,0),pic(290,call,29,0)
,pic(291,movf,8,0),pic(292,xorlw,65,0),pic(293,btfss,3,2),pic(294,goto,280,0),pic(295,call,29,0)
,pic(296,movf,8,0),pic(297,xorlw,48,0),pic(298,btfss,3,2),pic(299,goto,280,0),pic(300,call,29,0)
,pic(301,movf,8,0),pic(302,xorlw,72,0),pic(303,btfss,3,2),pic(304,goto,306,0),pic(305,goto,323,0)
,pic(306,movf,8,0),pic(307,xorlw,76,0),pic(308,btfss,3,2),pic(309,goto,311,0),pic(310,goto,325,0)
,pic(311,movf,8,0),pic(312,xorlw,83,0),pic(313,btfss,3,2),pic(314,goto,316,0),pic(315,goto,114,0)
,pic(316,movf,8,0),pic(317,xorlw,80,0),pic(318,btfss,3,2),pic(319,goto,280,0),pic(320,call,233,0)
,pic(321,call,95,0),pic(322,goto,280,0),pic(323,bsf,6,1),pic(324,goto,280,0),pic(325,bcf,6,1)
,pic(326,goto,280,0)].

environment(a,X) :-
X=[(2,1),(4,2),(6,3),(8,4)].
environment(b,X) :-
X=[(0,1),(200,2)].
environment(c,X) :-
X=[(0,0)].
environment(1,X) :-
X=[(2,1),(4,2),(6,3),(8,4)].
environment(d,_X). 
%X=[(0,0)].

% Two empty return points? 
/*
returnpoint(24, [97,99,101,103,105,107,109,111,113,210,212,214,219,221,223]).
returnpoint(58, [60,281,286,291,296,301]).
returnpoint(61, [44]). %No value, 44 is for test
returnpoint(72, [82,91]).
returnpoint(75, [80,84,89,93]).
returnpoint(94, [44]). %No value 44 is for test
returnpoint(113, [322]).
returnpoint(268, [122,233,321]).
*/

%list of possible returnpoints [60,80,82,84,89,91,93,97,99,101,103,105,107,109,111,113,122,210,212,214,219,221,223,281,286,291,296,301,321,322]
%missing returnpoints []

%returnpoint(24,[132,134,136,138,140,142,144,146]).
%returnpoint(58,[109,114,119,124]).
%returnpoint(91,[130]).

returnpoint(_,[60,80,82,84,89,91,93,97,99,101,103,105,107,109,111,
               113,122,210,212,214,219,221,223,281,286,291,296,
               301,321,322]).

/* Generate a list of return points for a given PIC progam */
genreturnpoints(X):-
	asm(X,Y),
	genretpoints(Y,Z),
	write(Z).


genretpoints([],[]).
genretpoints([pic(PC,call,_,_)|Xs], [PCN|PCs]) :-
	!,
	PCN is PC +1,
	genretpoints(Xs,PCs).
genretpoints([pic(_,_,_,_)|Xs], PCs) :-
	genretpoints(Xs,PCs).




/* Generate list of PCs in asm */
genpclist(X) :-
	asm(X,Y),
	genpclist2(Y,Z),
	write(Z),nl.
	
genpclist2([],[]).
genpclist2([pic(PC,_,_,_)|Xs],[PC|PCs]) :-
	genpclist2(Xs,PCs).
	
:- filter test(static).


% sics_sp
% '~/cleo/asap/PICsim/picsim.pl'.
% test(b).
% 'insttest2sp.pl'.
%
%

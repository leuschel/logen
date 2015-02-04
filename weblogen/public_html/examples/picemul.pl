/* ------------------------------------- */
/* An Emulator for PIC Assembly language */
/* ------------------------------------- */

specialize_this :-
	asm(pic,Y),
	environment(pic,Z),
	simulate(Y,[],Z).

simulate(Prog, Eeprom, Environment) :-
	picwrite(Prog),picnl,
	initpic(Registers,Stack,PC,W),
	picwrite('PC is '),picwrite(PC),picnl,
	picwrite('Env used for sim.: '),picwrite(Environment),picnl,
	calcregistersfrompc(PC,Registers,Registers_wpc), %husk at rette nedenunder		
	execute(Prog, Registers_wpc, Eeprom, Stack,PC,W,0, Environment).
	
execute(Prog,Registers,Eeprom,Stack,PC,W,Clock,Environment) :-
	fetchinst(Prog,PC,I,R1,R2),
	execinst(I,R1,R2,Eeprom,Eepromout,Registers,Registerstemp,Stack,Stackout,PC,NewPC,W,Wout,ClockTicks),
	picwrite('PC is '),picwrite(NewPC),picwrite(' Clock is '),picwrite(Clock),picwrite(' .... Regs: '),picwrite(Registerstemp),picnl,
	NewClock is Clock + ClockTicks,
	/* CUT - used for testing*/
	/* !, */
	simulatehw(PC,Registerstemp,Registersout,NewClock, ClockTicks,Environment,NewEnvironment),
	keysort(Registersout, NewRegs),
	/* !, */
	execute(Prog,NewRegs,Eepromout,Stackout,NewPC,Wout,NewClock,NewEnvironment).

fetchinst([pic(PC,I,R1,R2)|_Prog],PC,I,R1,R2).
fetchinst([pic(_X,_,_,_)|Prog],PC,I,R1,R2):-
	%X \== PC,
	fetchinst(Prog,PC,I,R1,R2).


/* Update Register 2 and 10 with low and high order bits of the PC */

calcregistersfrompc(PC,Rin,Rout) :-
	%Low order in reg.2
	Lpc is PC /\ 255,
	updatedata(PC,Rin,Rt,2,Lpc),
	%High in reg10
	Hpc is ((PC /\ 992) >> 8),
	updatedata(PC,Rt,Rout,10,Hpc).

calcpcfromregisters(PC,NewPC,R) :-
	retrievedata(PC,R,_Rout,10,Hpc),
	retrievedata(PC,R,_Rout,2,Lpc),
	NewPC is ((Hpc * 256) + Lpc).
	
%	write('Calculated PC is '),write(NewPC),nl.
		

/* Timer should be incremented at each instruction, remember overflow */

/* register bit that must the changed accordingly C,DC,Z */
execinst(addlw,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	Wt is W + R1,
	Wout is Wt /\ 255, /* only 8 bits */
	C is Wt >> 8, /* carry info. */
	updatez(PC,R,Rc,Wout),
	updatec(PC,Rc,Rout,C),
	PCOut is PC + 1,
	picwrite('exec addlw'),picnl,
	picwrite('W now '),picwrite(Wout),picnl,
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(rvec,R1,_,E,E,R,R,S,S,PC,R1,W,W,2) :-
	PC is 0.

/* regs. C,DC,Z */
execinst(addwf,R1,0,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec addwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wt is X + W,
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,Ru1,Rt,Wout),
	updatec(PC,Rt,Rout,C),
	PCOut is PC + 1,
	picwrite('W now '),picwrite(Wout),picnl.

execinst(addwf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec addwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Yt is X + W,
	Y is Yt /\ 255,
	C is Yt >> 8,
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rt2,Y),
	updatec(PC,Rt2,Rout,C),	
	PCOut is PC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

/* regs. Z */
execinst(andlw,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	Wout is W /\ R1,
	updatez(PC,R,Rout,Wout),
	PCOut is PC + 1,
	picwrite('exec andlw'),picnl,
	picwrite('W now '),picwrite(Wout),picnl.

/* regs Z */
execinst(andwf,R1,0,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec andwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is X /\ W,
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('W now '),picwrite(Wout),picnl.

execinst(andwf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec andwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X /\ W,
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.
	
execinst(bcf,R1,R2,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec bcf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X /\ (255-(1<< R2)),
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

execinst(bsf,R1,R2,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec bsf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is X \/ (1 << R2),
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 1,
	picwrite('Registers now '),picwrite(Rout),picnl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCOut,W,W,2) :-
	picwrite('exec btfss'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	picwrite('data retv. '),picwrite(X),picnl,
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCOut is PC + 2,
	picwrite('Bit set'),picnl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec btfss'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCOut is PC + 1,
	picwrite('Bit not set'),picnl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec btfsc'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCOut is PC + 1,
	picwrite('Bit set'),picnl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCOut,W,W,2) :-
	picwrite('exec btfsc'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCOut is PC + 2,
	picwrite('Bit not set'),picnl.

/* regs. aff. none */
execinst(call,R1,_,E,E,R,R,S,Sout,PC,R1,W,W,2) :-
	picwrite('exec call'),picnl,
	PCR is PC+1,
	pushstack(S,Sout,PCR),
	picwrite('Stack '),picwrite(Sout),picnl.

/* regs. z */
execinst(clrf,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec clrf'),picnl,
	updatedata(PC,R,Rt,R1,0),
	updatez(PC,Rt,Rout,0),
	PCOut is PC + 1,
	picwrite('Regs. '),picwrite(Rout),picnl.

/* regs. z */
execinst(clrw,_,_,E,E,R,Rout,S,S,PC,PCOut,_,0,1) :-
	picwrite('exec clrw'),picnl,
	updatez(PC,R,Rout,0),
	PCOut is PC + 1,
	picwrite('Wout 0'),picnl.

/* regs. z */
execinst(comf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec comf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs. z */
execinst(comf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec comf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(decf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec decf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X-1) /\ 255,
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs. z */
execinst(decf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec decf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X - 1) /\ 255,
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

/* Begin decfsz */
execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCOut,_,Wout,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	X == 1,  %was X 
	PCOut is PC + 2,
	picwrite('exec decfsz _,0 result 0'),picnl,
	Wout is 0. %will be 0

execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCOut,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	X \== 1,
	picwrite('exec decfsz _,0 not 0'),picnl,
	Wout is (X - 1) /\ 255,
	PCOut is PC + 1.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	X is 1,
	Y is X - 1,
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 2,
	picwrite('exec decfsz _,1 result 0'),picnl.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	X \== 1, %was X \== 0
	Y is (X - 1) /\ 255,
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 1,
	picwrite('exec decfsz _,1 not 0'),picnl.
/* END decfsz */

execinst(goto,R1,_,E,E,R,R,S,S,_PC,R1,W,W,2) :-
	picwrite('exec goto'),picnl.

/* Begin incfsz */
execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCOut,_,Wout,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255, /* only 8 bit values: 256=0*/
	Wout == 0,
	PCOut is PC + 2,
	picwrite('exec incfsz _,0 result 0'),picnl.

execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCOut,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	Wout > 0,
	PCOut is PC + 1,
	picwrite('exec incfsz _,0 not 0'),picnl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,2) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y == 0,
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 2,
	picwrite('exec incfsz _,1 result 0'),picnl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y > 0,
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 1,
	picwrite('exec incfsz _,1 not 0'),picnl.

execinst(incf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('exec incf _,0'),picnl.

execinst(incf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	retrievedata(PC,R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('exec incf _,1'),picnl.

/* END incfsz */

/* regs z */
execinst(iorlw,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec iorlw'),picnl,
	Wout is W \/ R1,
	updatez(PC,R,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.


/* regs z */
execinst(iorwf,R1,0,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec iorwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is W \/ X,
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(iorwf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec iorwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is W \/ X,
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

/* regs z */
execinst(movf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec movf '),picwrite(R1),picwrite(',0'),picnl,
	retrievedata(PC,R,Ru1,R1,Wout),
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout is '),picwrite(Wout),picnl.
	
	
execinst(movf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec movf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,Y),
	updatez(PC,Ru1,Rout,Y),
	PCOut is PC + 1,
	picwrite('Regs is '),picwrite(R),picnl.

execinst(movlw,R1,_,E,E,R,R,S,S,PC,PCOut,_,R1,1) :-
	PCOut is PC + 1,
	picwrite('exec movlw'),picnl,
	picwrite('W is '),picwrite(R1),picnl.

execinst(movwf,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	updatedata(PC,R,Rout,R1,W),	
	PCOut is PC + 1,
	picwrite('exec movwf'),picnl,
	picwrite(Rout),picnl.

execinst(nop,_,_,E,E,R,R,S,S,PC,PCOut,W,W,1) :-
	PCOut is PC + 1,
	picwrite('exec nop'),picnl.

execinst(retlw,R1,_,E,E,R,R,S,Sout,PC,PCout,_,R1,2) :-
	picwrite('exec retlw'),picnl,
	popstack(S,Sout,PCout),
	%returnpoint(PC,Rs),
	%memberReturnPoints(PCout,Rs),
	picwrite('Stack '),picwrite(Sout),picnl.

/* regs. aff. none */
execinst(return,_,_,E,E,R,R,S,Sout,PC,PCout,W,W,2) :-
	picwrite('exec return'),picnl,
	popstack(S,Sout,PCout),
	%returnpoint(PC,Rs),
	%memberReturnPoints(PCout,Rs),
	picwrite('Stack '),picwrite(Sout),picnl.

/* reg c */
execinst(rlf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec rlf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	retrievec(PC,Ru1,Ru2,C),
	Wt is (X << 1) + C,
	Wout is Wt /\ 255, /* 8 bit plus carry */
	Carry is Wt >> 8,
	updatec(PC,Ru2,Rout,Carry), /* set carry bit if ap. */
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(rlf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec rlf'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	retrievec(PC,Ru1,Ru2,C),
	Yt is (X << 1) + C,
	Y is Yt /\ 255, /* 8 bit plus carry */
	/* set carry bit if ap. */
	Carry is Yt >> 8,
	updatedata(PC,Ru2,Rt,R1,Y),
	updatec(PC,Rt,Rout,Carry),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(rrf,R1,0,E,E,R,Rout,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec rlf'),picnl,
	retrievec(PC,R,Ru1,C),/* get carry bit if ap. */
	retrievedata(PC,Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Wout is (X >> 1) + (C << 7), /* plus carry */
	updatec(PC,Ru2,Rout,Carry),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(rrf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec rlf'),picnl,
	retrievec(PC,R,Ru1,C), /* get carry bit if ap. */
	retrievedata(PC,Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Y is (X >> 1) + (C << 7), /* plus carry */
	updatedata(PC,Ru2,Rt,R1,Y),
	updatec(PC,Rt,Rout,Carry),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

/* C, DC, Z */
execinst(sublw,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec sublw '),picnl,
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (R1 + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,R,Rt,Wout),
	updatec(PC,Rt,Rout,C), /* set carry bit if ap. */
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(subwf,R1,0,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec subwf _,0 '),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (X + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(PC,Ru1,Rt,Wout),
	updatec(PC,Rt,Rout,C), /* set carry bit if ap. */
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(subwf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec subwf _,1'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Zt is (X + Y), 
	Z is Zt /\ 255,
	C is Zt >> 8,
	updatedata(PC,Ru1,Rt,R1,Z),
	updatez(PC,Rt,Rt2,Z),
	updatec(PC,Rt2,Rout,C),
	PCOut is PC + 1,
	/* set carry bit if ap. */
	picwrite('Regs '),picwrite(Rout),picnl.

execinst(swapf,R1,0,E,E,R,Ru1,S,S,PC,PCOut,_,Wout,1) :-
	picwrite('exec swapf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(swapf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec swapf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	updatedata(PC,Ru1,Rout,R1,Y),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.

/* regs z */
execinst(xorlw,R1,_,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec xorlw '),picnl,
	Wout is #(W,R1),
	updatez(PC,R,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

/* regs z */
execinst(xorwf,R1,0,E,E,R,Rout,S,S,PC,PCOut,W,Wout,1) :-
	picwrite('exec xorwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Wout is #(W,X),
	updatez(PC,Ru1,Rout,Wout),
	PCOut is PC + 1,
	picwrite('Wout '),picwrite(Wout),picnl.

execinst(xorwf,R1,1,E,E,R,Rout,S,S,PC,PCOut,W,W,1) :-
	picwrite('exec xorwf _,0'),picnl,
	retrievedata(PC,R,Ru1,R1,X),
	Y is #(W,X),
	updatedata(PC,Ru1,Rt,R1,Y),
	updatez(PC,Rt,Rout,Y),
	PCOut is PC + 1,
	picwrite('Regs '),picwrite(Rout),picnl.
	

/* Register updates: Z, DC, C */

updatec(PC,R,Rout,0) :-
	retrievedata(PC,R,Ru1,3,X),
	Z is X /\ 254, /* clear bit 0 */
	updatedata(PC,Ru1,Rout,3,Z).
	
updatec(PC,R,Rout,Y) :-
	Y \== 0,
	retrievedata(PC,R,Ru1,3,X),
	Z is X \/ 1,  /* set bit 0 */
	updatedata(PC,Ru1,Rout,3,Z).

updatez(PC,R,Rout,0) :-
	retrievedata(PC,R,Ru1,3,X),
	Y is X \/ 4, /* set bit 2 */
	updatedata(PC,Ru1,Rout,3,Y).
	
updatez(PC,R,Rout,Y) :-
	Y \== 0,
	retrievedata(PC,R,Ru1,3,X),
	Z is X /\ 251, /* clear bit 2 */
	updatedata(PC,Ru1,Rout,3,Z).
	
retrievec(PC,R,Rout,C) :-
	retrievedata(PC,R,Rout,3,X),
	C is X /\ 1.

/* Retrieve data from hardware or data register */
/* Register list has the format [RegNr-([Values],[Readpicwrite])] */


retrievedata(PC,Rin,Rout,0,X) :-
	/* INDF Register - indirect addressing */
	retrievedata(PC,Rin,Rt,4,Y),
	retrievedata(PC,Rt,Rout,Y,X).
retrievedata(PC,[],[1-([0],[r(-1,PC)])],1,X) :- 
	/* Timer register - disabled */
	X is 0.
retrievedata(PC,[1-([X|VL],[H|RWs])|R],[1-([X|VL],[r(Cc,PC),H|RWs])|R],1,X) :- 
	H =.. [_,Cc|_].
	
/* Register 2 mapped to 8 lsb of PC. */
retrievedata(_PC,[2-([X|VL],RWL)|R],[2-([X|VL],RWL)|R],2,X).
/* STATUS register - 0001 1xxx at reset */
retrievedata(PC,[],[3-([24],[r(-1,PC)])],3,24). 
retrievedata(PC,[3-([X|VL],[H|RWs])|R],[3-([X|VL],[r(Cc,PC),H|RWs])|R],3,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[],[4-([12],[r(-1,PC)])],4,X) :- 
	/* FSR register - undefined at reset */
	/* What happens if this i 0? */
	X is 12.
retrievedata(PC,[4-([X|VL],[H|RWs])|R],[4-([X|VL],[r(Cc,PC),H|RWs])|R],4,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[],[5-([0],[r(-1,PC)])],5,X) :- 
	/* PORT A register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata(PC,[5-([X|VL],[H|RWs])|R],[5-([X|VL],[r(Cc,PC),H|RWs])|R],5,X) :-
	H =.. [_,Cc|_].
retrievedata(PC,[],[6-([0],[r(-1,PC)])],6,X) :- 
	/* PORT B register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata(PC,[6-([X|VL],[H|RWs])|R],[6-([X|VL],[r(Cc,PC),H|RWs])|R],6,X) :-
	H =.. [_,Cc|_].

/* remember to update Rin Rout */	
retrievedata(_PC,Rin,Rin,7,0). /* register 7 always read 0 */

retrievedata(PC,[],[R1-([0],[r(-1,PC)])],R1,0) :- 
	R1 > 11. 

retrievedata(_PC,Rin,Rin,8,0). 
retrievedata(_PC,Rin,Rin,9,0). 

/* Register 10, mapped to the PC 5 most significant bits. Not directly readable but writeable */

retrievedata(_PC,[10-([X|VL],RWL)|R],[10-([X|VL],RWL)|R],10,X).

retrievedata(_PC,Rin,Rin,11,0). 

/* At the moment, a register reads 0 until it has been written to */
/* NOT IMPLEMENTED YET: Append 'R' to RW-list */
retrievedata(PC,[R1-([Y|VL],[H|RWs])|Rin],[R1-([Y|VL],[r(Cc,PC),H|RWs])|Rin],R1,Y) :- 
	R1 > 11,
	H =.. [_F,Cc|_].
	
retrievedata(PC,[R-Z|D],[R-Z|Rout],R1,Y):-
	R < R1,
	retrievedata(PC,D,Rout,R1,Y).


/* Update value in hardware or data register */
/* INDF Register - indirect addressing */
updatedata(PC,R,Rout,0,X) :-
	retrievedata(PC,R,Ru1,4,Y),
	updatedata(PC,Ru1,Rout,Y,X).

/* Timer register */
updatedata(PC,[], [1-([W],[w(PC)])], 1, W).
updatedata(PC,[1-(VL,RWL)|D],[1-([W|VL],[w(PC)|RWL])|D],1,W).


/* For register 2, we only keep track of current value */
updatedata(_PC,[], [2-([W],[])], 2, W).
updatedata(_PC,[2-(_VL,RWL)|D],[2-([W],RWL)|D],2,W).

/* Status register */
updatedata(PC,[], [3-([W],[w(PC)])], 3, W).
updatedata(PC,[3-(VL,RWL)|D],[3-([W|VL],[w(PC)|RWL])|D],3,W).

/* FSR register */
updatedata(PC,[], [4-([W],[w(PC)])], 4, W).
updatedata(PC,[4-(VL,RWL)|D],[4-([W|VL],[w(PC)|RWL])|D],4,W).

/* PORT A register */
updatedata(PC,[], [5-([W],[w(PC)])], 5, W).
updatedata(PC,[5-(VL,RWL)|D],[5-([W|VL],[w(PC)|RWL])|D],5,W).

/* PORT B register */
updatedata(PC,[], [6-([W],[w(PC)])], 6, W).
updatedata(PC,[6-(VL,RWL)|D],[6-([W|VL],[w(PC)|RWL])|D],6,W).

updatedata(_PC,R,R,7,_). /* register 7 stores nothing */

/* No eemprom simulated yet */
updatedata(_PC,R,R,8,_).
updatedata(_PC,R,R,9,_).

/* For register 10, we only keep track of current value */
updatedata(_PC,[], [10-([W],[])], 10, W).
updatedata(_PC,[10-(_VL,RWL)|D],[10-([W],RWL)|D],10,W).

updatedata(_PC,R,R,11,_).
/* End eeprom routines*/


updatedata(PC,[], [R1-([W],[w(PC)])], R1, W) :-
	/* check boundries */
	R1 > 11.
updatedata(PC,[R1-(VL,RWL)|D],[R1-([W|VL],[w(PC)|RWL])|D],R1,W) :-
	/* check again */
	R1 > 11.
updatedata(PC,[X-Z|D],[X-Z|Dout],R1,W):-
	X < R1,
	updatedata(PC,D,Dout,R1,W).


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
	
regNums([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32]).

%simulatehw(R,R,NC, CT,E,E) :- !.


/* Simulate HW */
simulatehw(PC,Rin,Rout,Clock,ClockTicks,Environment,NewEnvironment) :-
	simulateinput(PC,Rin,Rt1,Clock,Environment,NewEnvironment),
	simulatetimer(PC,Rt1,Rout,ClockTicks).

%simulateinput(R,R,CT,E,E) :- !.
simulateinput(PC,Rin,Rout,Clock,Env0,Env1) :-
	retrievedata(PC,Rin,Rt1,6,X),
	checkInputStream(Env0,Clock,X,Y,Env1),
	updatedata(PC,Rt1,Rout,6,Y).

checkInputStream([(Ct,V)|Xs],Clock,X,X,[(Ct,V)|Xs]) :-	% not yet time to read
	Ct =< Clock.
checkInputStream([(Ct,V)|Xs],Clock,_,V,Xs) :-			% get value from input
	Ct > Clock.
	
/* Simulated input .... */
/* No update on input port */
%simulateinput(_PC,R,R,Clock,EnvI,EnvI) :-
%	headEnvInput(EnvI,(Ct,_),_),
%	Ct > Clock.

/* update on input port */
%simulateinput(PC,Rin,Rout,Clock,EnvI,NewEnvI) :-
%	headEnvInput(EnvI,(Ct,V),NewEnvI),
%	Ct =< Clock,
%	%removeHeadEnvInput(EnvI,NewEnvI),
%	updatedata(PC,Rin,Rout,6,V).
	

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
	updatedata(PC,Rt1,Rout,1,Y).

/*
	Enable/disable writes
*/
	%picwrite(X) :- write(X).
	picwrite(_).

	%picnl :- nl.
	picnl.

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


environment(pic,_X).

asm(pic,X):-X=[pic(-1,nop,0,0),pic(0,goto,264,0),pic(1,movwf,9,0),pic(2,movlw,9,0),pic(3,movwf,10,0)
,pic(4,bcf,6,1),pic(5,goto,12,0),pic(6,rrf,9,1),pic(7,btfsc,3,0),pic(8,goto,11,0)
,pic(9,bcf,6,1),pic(10,goto,12,0),pic(11,bsf,6,1),pic(12,movlw,195,0),pic(13,movwf,1,0)
,pic(14,movf,1,0),pic(15,btfss,3,2),pic(16,goto,14,0),pic(17,movf,10,1),pic(18,btfsc,3,2)
,pic(19,return,0,0),pic(20,decfsz,10,1),pic(21,goto,6,0),pic(22,bsf,6,1),pic(23,goto,12,0)
,pic(24,btfsc,6,0),pic(25,goto,24,0),pic(26,movlw,225,0),pic(27,movwf,1,0),pic(28,movf,1,0)
,pic(29,btfss,3,2),pic(30,goto,28,0),pic(31,btfsc,6,0),pic(32,goto,24,0),pic(33,movlw,195,0)
,pic(34,movwf,1,0),pic(35,movlw,128,0),pic(36,movwf,8,0),pic(37,movlw,195,0),pic(38,movwf,1,0)
,pic(39,movf,1,0),pic(40,btfss,3,2),pic(41,goto,39,0),pic(42,rrf,8,1),pic(43,bcf,8,7)
,pic(44,btfsc,6,0),pic(45,bsf,8,7),pic(46,btfss,3,0),pic(47,goto,37,0),pic(48,movlw,195,0)
,pic(49,movwf,1,0),pic(50,movf,1,0),pic(51,btfss,3,2),pic(52,goto,50,0),pic(53,return,0,0)
,pic(54,call,24,0),pic(55,movf,8,0),pic(56,return,0,0),pic(57,clrf,14,0),pic(58,clrf,15,0)
,pic(59,movf,15,0),pic(60,addwf,13,0),pic(61,movwf,15,0),pic(62,incf,14,0),pic(63,movwf,14,0)
,pic(64,xorwf,12,0),pic(65,btfss,3,2),pic(66,goto,59,0),pic(67,return,0,0),pic(68,movlw,48,0)
,pic(69,subwf,16,0),pic(70,return,0,0),pic(71,movlw,10,0),pic(72,movwf,12,0),pic(73,movwf,16,0)
,pic(74,call,68,0),pic(75,movwf,13,0),pic(76,call,57,0),pic(77,movwf,16,0),pic(78,call,68,0)
,pic(79,addwf,15,0),pic(80,movlw,10,0),pic(81,movwf,12,0),pic(82,movwf,16,0),pic(83,call,68,0)
,pic(84,movwf,13,0),pic(85,call,57,0),pic(86,movwf,16,0),pic(87,call,68,0),pic(88,addwf,15,0)
,pic(89,return,0,0),pic(90,movlw,36,0),pic(91,call,1,0),pic(92,movlw,82,0),pic(93,call,1,0)
,pic(94,movlw,65,0),pic(95,call,1,0),pic(96,movlw,48,0),pic(97,call,1,0),pic(98,movf,18,0)
,pic(99,call,1,0),pic(100,movf,19,0),pic(101,call,1,0),pic(102,movf,20,0),pic(103,call,1,0)
,pic(104,movlw,59,0),pic(105,call,1,0),pic(106,movlw,10,0),pic(107,call,1,0),pic(108,return,0,0)
,pic(109,movlw,64,0),pic(110,movwf,23,0),pic(111,movwf,21,0),pic(112,clrf,22,0),pic(113,clrf,24,0)
,pic(114,clrf,27,0),pic(115,clrf,28,0),pic(116,call,228,0),pic(117,movf,18,0),pic(118,addlw,192,0)
,pic(119,movwf,18,0),pic(120,bsf,3,0),pic(121,rlf,23,0),pic(122,subwf,24,1),pic(123,btfss,3,0)
,pic(124,decf,23,1),pic(125,bcf,3,0),pic(126,rlf,18,0),pic(127,addwf,24,1),pic(128,btfsc,3,0)
,pic(129,incf,23,1),pic(130,bcf,3,0),pic(131,rrf,21,0),pic(132,movwf,20,0),pic(133,bcf,3,0)
,pic(134,rrf,20,0),pic(135,subwf,21,0),pic(136,movwf,25,0),pic(137,bcf,3,0),pic(138,rrf,18,0)
,pic(139,movwf,20,0),pic(140,bcf,3,0),pic(141,rrf,20,0),pic(142,addwf,25,1),pic(143,bcf,3,0)
,pic(144,rlf,21,0),pic(145,movwf,20,0),pic(146,bcf,3,0),pic(147,rlf,20,1),pic(148,bcf,3,0)
,pic(149,rlf,20,1),pic(150,bcf,3,0),pic(151,rlf,20,0),pic(152,bcf,3,0),pic(153,rlf,20,1)
,pic(154,bcf,3,0),pic(155,rlf,20,0),pic(156,subwf,22,0),pic(157,movwf,26,0),pic(158,btfss,3,0)
,pic(159,decf,25,1),pic(160,bcf,3,0),pic(161,rlf,18,0),pic(162,movwf,20,0),pic(163,bcf,3,0)
,pic(164,rlf,20,1),pic(165,bcf,3,0),pic(166,rlf,20,1),pic(167,bcf,3,0),pic(168,rlf,20,0)
,pic(169,bcf,3,0),pic(170,rlf,20,1),pic(171,bcf,3,0),pic(172,rlf,20,0),pic(173,addwf,26,1)
,pic(174,btfsc,3,0),pic(175,incf,25,1),pic(176,bcf,3,0),pic(177,rrf,22,0),pic(178,movwf,20,0)
,pic(179,bcf,3,0),pic(180,rrf,20,0),pic(181,subwf,26,1),pic(182,btfss,3,0),pic(183,decf,25,1)
,pic(184,movf,26,0),pic(185,movwf,22,0),pic(186,movf,25,0),pic(187,movwf,21,0),pic(188,movf,27,1)
,pic(189,btfsc,3,2),pic(190,goto,197,0),pic(191,movf,23,0),pic(192,addlw,255,0),pic(193,subwf,21,0)
,pic(194,btfss,3,0),pic(195,clrf,27,0),pic(196,goto,218,0),pic(197,movf,21,0),pic(198,addlw,255,0)
,pic(199,subwf,23,0),pic(200,btfsc,3,0),pic(201,goto,218,0),pic(202,comf,27,1),pic(203,movlw,36,0)
,pic(204,call,1,0),pic(205,movlw,82,0),pic(206,call,1,0),pic(207,movlw,7,0),pic(208,call,1,0)
,pic(209,incf,28,1),pic(210,movf,28,0),pic(211,andlw,7,0),pic(212,addlw,48,0),pic(213,call,1,0)
,pic(214,movlw,59,0),pic(215,call,1,0),pic(216,movlw,10,0),pic(217,call,1,0),pic(218,movlw,160,0)
,pic(219,movwf,20,0),pic(220,movlw,195,0),pic(221,movwf,1,0),pic(222,movf,1,0),pic(223,btfss,3,2)
,pic(224,goto,222,0),pic(225,decfsz,20,1),pic(226,goto,220,0),pic(227,goto,116,0),pic(228,clrf,17,0)
,pic(229,btfsc,5,1),pic(230,goto,229,0),pic(231,btfss,5,1),pic(232,goto,231,0),pic(233,nop,0,0)
,pic(234,nop,0,0),pic(235,incf,17,1),pic(236,btfsc,5,1),pic(237,goto,233,0),pic(238,movf,17,0)
,pic(239,movwf,18,0),pic(240,clrf,17,0),pic(241,btfsc,5,0),pic(242,goto,241,0),pic(243,btfss,5,0)
,pic(244,goto,243,0),pic(245,nop,0,0),pic(246,nop,0,0),pic(247,incf,17,1),pic(248,btfsc,5,0)
,pic(249,goto,245,0),pic(250,movf,17,0),pic(251,movwf,19,0),pic(252,nop,0,0),pic(253,nop,0,0)
,pic(254,incf,17,1),pic(255,btfss,5,0),pic(256,goto,252,0),pic(257,movf,17,0),pic(258,movwf,20,0)
,pic(259,movf,18,0),pic(260,movwf,18,0),pic(261,movf,19,0),pic(262,movwf,19,0),pic(263,return,0,0)
,pic(264,clrf,4,0),pic(265,bsf,6,1),pic(266,bsf,3,5),pic(267,movlw,3,0),pic(268,movwf,5,0)
,pic(269,movlw,1,0),pic(270,movwf,6,0),pic(271,movlw,2,0),pic(272,movwf,1,0),pic(273,bcf,3,5)
,pic(274,bsf,6,1),pic(275,call,24,0),pic(276,movf,8,0),pic(277,xorlw,36,0),pic(278,btfss,3,2)
,pic(279,goto,275,0),pic(280,call,24,0),pic(281,movf,8,0),pic(282,xorlw,83,0),pic(283,btfss,3,2)
,pic(284,goto,275,0),pic(285,call,24,0),pic(286,movf,8,0),pic(287,xorlw,65,0),pic(288,btfss,3,2)
,pic(289,goto,275,0),pic(290,call,24,0),pic(291,movf,8,0),pic(292,xorlw,48,0),pic(293,btfss,3,2)
,pic(294,goto,275,0),pic(295,call,24,0),pic(296,movf,8,0),pic(297,xorlw,72,0),pic(298,btfss,3,2)
,pic(299,goto,301,0),pic(300,goto,318,0),pic(301,movf,8,0),pic(302,xorlw,76,0),pic(303,btfss,3,2)
,pic(304,goto,306,0),pic(305,goto,320,0),pic(306,movf,8,0),pic(307,xorlw,83,0),pic(308,btfss,3,2)
,pic(309,goto,311,0),pic(310,goto,109,0),pic(311,movf,8,0),pic(312,xorlw,80,0),pic(313,btfss,3,2)
,pic(314,goto,275,0),pic(315,call,228,0),pic(316,call,90,0),pic(317,goto,275,0),pic(318,bsf,6,1)
,pic(319,goto,275,0),pic(320,bcf,6,1),pic(321,goto,275,0)].

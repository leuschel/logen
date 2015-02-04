test(X) :-
	asm(X,Y),
	simulate(Y,[],[]).

simulate(Prog, Eeprom, _Data) :-
	write(Prog),nl,
	initpic(Registers,Stack,PC,W),
	write('PC is '),write(PC),nl,
	execute(Prog, Eeprom,Registers, Stack,PC,W,0).
	
execute(Prog,Eeprom,Registers,Stack,PC,W,Clock) :-
	fetchinst(Prog,PC,I,R1,R2),
	execinst(I,R1,R2,Eeprom,Eepromout,Registers,Registerstemp,Stack,Stackout,PC,PCout,W,Wout,ClockTicks),
	write('PC is '),write(PCout),write(' Clock is '),write(Clock),write(' .... Regs: '),write(Registerstemp),nl,
	NewClock is Clock + ClockTicks,
	/* CUT - used for testing*/
	/* !, */
	simulatehw(Registerstemp,Registersout,NewClock, ClockTicks),
	keysort(Registersout, NewRegs),
	/*!, */
	execute(Prog,Eepromout,NewRegs,Stackout,PCout,Wout,NewClock).

fetchinst([pic(PC,I,R1,R2)|_Prog],PC,I,R1,R2).
fetchinst([pic(X,_,_,_)|Prog],PC,I,R1,R2):-
	X \== PC,
	fetchinst(Prog,PC,I,R1,R2).


/* Timer should be incremented at each instruction, remember overflow */

/* register bit that must the changed accordingly C,DC,Z */
execinst(addlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	PCout is PC +1,
	Wt is W + R1,
	Wout is Wt /\ 255, /* only 8 bits */
	C is Wt >> 8, /* carry info. */
	updatez(R,Rc,Wout),
	updatec(Rc,Rout,C),
	write('exec addlw'),nl,
	write('W now '),write(Wout),nl,
	write('Regs '),write(Rout),nl.

execinst(rvec,R1,_,E,E,R,R,S,S,PC,R1,W,W,2) :-
	PC is 0.

/* regs. C,DC,Z */
execinst(addwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec addwf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wt is X + W,
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(Ru1,Rt,Wout),
	updatec(Rt,Rout,C),
	write('W now '),write(Wout),nl.

execinst(addwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec addwf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Yt is X + W,
	Y is Yt /\ 255,
	C is Yt >> 8,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rt2,Y),
	updatec(Rt2,Rout,C),	
	write('Registers now '),write(Rout),nl.

/* regs. Z */
execinst(andlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	PCout is PC +1,
	Wout is W /\ R1,
	updatez(R,Rout,Wout),
	write('exec andlw'),nl,
	write('W now '),write(Wout),nl.

/* regs Z */
execinst(andwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec andwf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is X /\ W,
	updatez(Ru1,Rout,Wout),
	write('W now '),write(Wout),nl.

execinst(andwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec andwf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is X /\ W,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	write('Registers now '),write(Rout),nl.
	
execinst(bcf,R1,R2,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec bcf'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is X /\ (255-(1<< R2)),
	updatedata(Ru1,Rout,R1,Y),
	write('Registers now '),write(Rout),nl.

execinst(bsf,R1,R2,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec bsf'),nl,
    PCout is PC + 1,    
	retrievedata(R,Ru1,R1,X),
	Y is X \/ (1 << R2),
	updatedata(Ru1,Rout,R1,Y),
	write('Registers now '),write(Rout),nl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,2) :-
	write('exec btfss'),nl,
	retrievedata(R,Ru1,R1,X),
	write('data retv. '),write(X),nl,
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCout is PC + 2,
	write('Bit set'),nl.

execinst(btfss,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,1) :-
	write('exec btfss'),nl,
	retrievedata(R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCout is PC + 1,
	write('Bit not set'),nl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,1) :-
	write('exec btfsc'),nl,
	retrievedata(R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y >= 1,
	PCout is PC + 1,
	write('Bit set'),nl.

execinst(btfsc,R1,R2,E,E,R,Ru1,S,S,PC,PCout,W,W,2) :-
	write('exec btfsc'),nl,
	retrievedata(R,Ru1,R1,X),
	Y is (X /\ (1 << R2)),
	Y is 0,
	PCout is PC + 2,
	write('Bit not set'),nl.

/* regs. aff. none */
execinst(call,R1,_,E,E,R,R,S,Sout,PC,PCout,W,W,2) :-
	write('exec call'),nl,
	PCR is PC+1,
	pushstack(S,Sout,PCR),
	PCout is R1,
	write('Stack '),write(Sout),nl.

/* regs. z */
execinst(clrf,R1,_,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec clrf'),nl,
	PCout is PC+1,
	updatedata(R,Rt,R1,0),
	updatez(Rt,Rout,0),
	write('Regs. '),write(Rout),nl.

/* regs. z */
execinst(clrw,_,_,E,E,R,Rout,S,S,PC,PCout,_,0,1) :-
	write('exec clrw'),nl,
	PCout is PC+1,
	updatez(R,Rout,0),
	write('Wout 0'),nl.

/* regs. z */
execinst(comf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	write('exec comf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatez(Ru1,Rout,Wout),
	write('Wout '),write(Wout),nl.

/* regs. z */
execinst(comf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec comf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is (\X) /\ 255, /* only first 8 bits - otherwise Wout will be negative */
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	write('Regs '),write(Rout),nl.

execinst(decf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	write('exec decf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is (X-1) /\ 255,
	updatez(Ru1,Rout,Wout),
	write('Wout '),write(Wout),nl.

/* regs. z */
execinst(decf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec decf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is (X - 1) /\ 255,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	write('Regs '),write(Rout),nl.

/* Begin decfsz */
execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,2) :-
	retrievedata(R,Ru1,R1,X),
	X == 1,
	write('exec decfsz _,0 result 0'),nl,
	Wout is X - 1,
	PCout is PC + 2.

execinst(decfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(R,Ru1,R1,X),
	X \== 0,
	write('exec decfsz _,0 not 0'),nl,
	Wout is (X - 1) /\ 255,
	PCout is PC + 1.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,2) :-
	retrievedata(R,Ru1,R1,X),
	X is 1,
	Y is X - 1,
	updatedata(Ru1,Rout,R1,Y),
	PCout is PC + 2,
	write('exec decfsz _,1 result 0'),nl.

execinst(decfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(R,Ru1,R1,X),
	X \== 0,
	Y is (X - 1) /\ 255,
	updatedata(Ru1,Rout,R1,Y),
	PCout is PC + 1,
	write('exec decfsz _,1 not 0'),nl.
/* END decfsz */

execinst(goto,R1,_,E,E,R,R,S,S,_,R1,W,W,2) :-
	write('exec goto'),nl.

/* Begin incfsz */
execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,2) :-
	retrievedata(R,Ru1,R1,X),
	Wout is (X + 1) /\ 255, /* only 8 bit values: 256=0*/
	Wout == 0,
	PCout is PC + 2,
	write('exec incfsz _,0 result 0'),nl.

execinst(incfsz,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	Wout > 0,
	PCout is PC + 1,
	write('exec incfsz _,0 not 0'),nl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,2) :-
	retrievedata(R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y == 0,
	updatedata(Ru1,Rout,R1,Y),
	PCout is PC + 2,
	write('exec incfsz _,1 result 0'),nl.

execinst(incfsz,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	Y > 0,
	updatedata(Ru1,Rout,R1,Y),
	PCout is PC + 1,
	write('exec incfsz _,1 not 0'),nl.

execinst(incf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	retrievedata(R,Ru1,R1,X),
	Wout is (X + 1) /\ 255,
	PCout is PC + 1,
	updatez(Ru1,Rout,Wout),
	write('exec incf _,0'),nl.

execinst(incf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	retrievedata(R,Ru1,R1,X),
	Y is (X + 1) /\ 255,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	PCout is PC + 1,
	write('exec incf _,1'),nl.

/* END incfsz */

/* regs z */
execinst(iorlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec iorlw'),nl,
	PCout is PC + 1,
	Wout is W \/ R1,
	updatez(R,Rout,Wout),
	write('Wout '),write(Wout),nl.


/* regs z */
execinst(iorwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec iorwf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is W \/ X,
	updatez(Ru1,Rout,Wout),
	write('Wout '),write(Wout),nl.

execinst(iorwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec iorwf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is W \/ X,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	write('Regs '),write(Rout),nl.

/* regs z */
execinst(movf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	write('exec movf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,Wout),
	updatez(Ru1,Rout,Wout),
	write('Wout is '),write(Wout),nl.
	
	
execinst(movf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec movf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,Y),
	updatez(Ru1,Rout,Y),
	write('Regs is '),write(R),nl.

execinst(movlw,R1,_,E,E,R,R,S,S,PC,PCout,_,R1,1) :-
	PCout is PC+1,
	write('exec movlw'),nl,
	write('W is '),write(R1),nl.

execinst(movwf,R1,_,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	PCout is PC+1,
	updatedata(R,Rout,R1,W),
	write('exec movwf'),nl,
	write(Rout),nl.

execinst(nop,_,_,E,E,R,R,S,S,PC,PCout,W,W,1) :-
	write('exec nop'),nl,
	PCout is PC + 1.

execinst(retlw,R1,_,E,E,R,R,S,Sout,PC,PCout,_,R1,2) :-
	write('exec retlw'),nl,
	popstack(S,Sout,PCout),
	%PCout is PCR,
	returnpoint(PC,Rs),
	memberReturnPoints(PCout,Rs),
	write('Stack '),write(Sout),nl.

/* regs. aff. none */
execinst(return,_,_,E,E,R,R,S,Sout,PC,PCout,W,W,2) :-
	write('exec return'),nl,
	popstack(S,Sout,PCout),
	%PCout is PCR,
	returnpoint(PC,Rs),
	memberReturnPoints(PCout,Rs),
	write('Stack '),write(Sout),nl.

/* reg c */
execinst(rlf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	write('exec rlf'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	retrievec(Ru1,Ru2,C),
	Wt is (X << 1) + C,
	Wout is Wt /\ 255, /* 8 bit plus carry */
	Carry is Wt >> 8,
	updatec(Ru2,Rout,Carry), /* set carry bit if ap. */
	write('Wout '),write(Wout),nl.

execinst(rlf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec rlf'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	retrievec(Ru1,Ru2,C),
	Yt is (X << 1) + C,
	Y is Yt /\ 255, /* 8 bit plus carry */
	/* set carry bit if ap. */
	Carry is Yt >> 8,
	updatedata(Ru2,Rt,R1,Y),
	updatec(Rt,Rout,Carry),
	write('Regs '),write(Rout),nl.

execinst(rrf,R1,0,E,E,R,Rout,S,S,PC,PCout,_,Wout,1) :-
	write('exec rlf'),nl,
	PCout is PC + 1,
	retrievec(R,Ru1,C),/* get carry bit if ap. */
	retrievedata(Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Wout is (X >> 1) + (C << 7), /* plus carry */
	updatec(Ru2,Rout,Carry),
	write('Wout '),write(Wout),nl.

execinst(rrf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec rlf'),nl,
	PCout is PC + 1,
	retrievec(R,Ru1,C), /* get carry bit if ap. */
	retrievedata(Ru1,Ru2,R1,X),
	Carry is X /\ 1,
	Y is (X >> 1) + (C << 7), /* plus carry */
	updatedata(Ru2,Rt,R1,Y),
	updatec(Rt,Rout,Carry),
	write('Regs '),write(Rout),nl.

/* C, DC, Z */
execinst(sublw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec sublw '),nl,
	PCout is PC + 1,
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (R1 + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(R,Rt,Wout),
	updatec(Rt,Rout,C), /* set carry bit if ap. */
	write('Wout '),write(Wout),nl.

execinst(subwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec subwf _,0 '),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Wt is (X + Y),
	Wout is Wt /\ 255,
	C is Wt >> 8,
	updatez(Ru1,Rt,Wout),
	updatec(Rt,Rout,C), /* set carry bit if ap. */
	write('Wout '),write(Wout),nl.

execinst(subwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec subwf _,1'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	/* subtract 2-compl. */
	Y is (\W) + 1,
	Zt is (X + Y), 
	Z is Zt /\ 255,
	C is Zt >> 8,
	updatedata(Ru1,Rt,R1,Z),
	updatez(Rt,Rt2,Z),
	updatec(Rt2,Rout,C),
	/* set carry bit if ap. */
	write('Regs '),write(Rout),nl.

execinst(swapf,R1,0,E,E,R,Ru1,S,S,PC,PCout,_,Wout,1) :-
	write('exec swapf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	write('Wout '),write(Wout),nl.

execinst(swapf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec swapf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is ((X /\ 240) >> 4) \/ ((X /\ 15) << 4),
	updatedata(Ru1,Rout,R1,Y),
	write('Regs '),write(Rout),nl.

/* regs z */
execinst(xorlw,R1,_,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec xorlw '),nl,
	PCout is PC + 1,
	Wout is W # R1,
	updatez(R,Rout,Wout),
	write('Wout '),write(Wout),nl.

/* regs z */
execinst(xorwf,R1,0,E,E,R,Rout,S,S,PC,PCout,W,Wout,1) :-
	write('exec xorwf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Wout is W # X,
	updatez(Ru1,Rout,Wout),
	write('Wout '),write(Wout),nl.

execinst(xorwf,R1,1,E,E,R,Rout,S,S,PC,PCout,W,W,1) :-
	write('exec xorwf _,0'),nl,
	PCout is PC + 1,
	retrievedata(R,Ru1,R1,X),
	Y is W # X,
	updatedata(Ru1,Rt,R1,Y),
	updatez(Rt,Rout,Y),
	write('Regs '),write(Rout),nl.
	

/* Register updates: Z, DC, C */

updatec(R,Rout,0) :-
	retrievedata(R,Ru1,3,X),
	Z is X /\ 254, /* clear bit 0 */
	updatedata(Ru1,Rout,3,Z).
	
updatec(R,Rout,Y) :-
	Y \== 0,
	retrievedata(R,Ru1,3,X),
	Z is X \/ 1,  /* set bit 0 */
	updatedata(Ru1,Rout,3,Z).

updatez(R,Rout,0) :-
	retrievedata(R,Ru1,3,X),
	Y is X \/ 4, /* set bit 2 */
	updatedata(Ru1,Rout,3,Y).
	
updatez(R,Rout,Y) :-
	Y \== 0,
	retrievedata(R,Ru1,3,X),
	Z is X /\ 251, /* clear bit 2 */
	updatedata(Ru1,Rout,3,Z).
	
retrievec(R,Rout,C) :-
	retrievedata(R,Rout,3,X),
	C is X /\ 1.

/* Retrieve data from hardware or data register */
/* Register list has the format [(RegNr,[Values],[ReadWrite])] */


retrievedata(Rin,Rout,0,X) :-
	/* INDF Register - indirect addressing */
	retrievedata(Rin,Rt,4,Y),
	retrievedata(Rt,Rout,Y,X).

retrievedata([],[1-([0],[r])],1,X) :- 
	/* Timer register - disabled */
	X is 0.
retrievedata([1-([X|VL],RWL)|R],[1-([X|VL],[r|RWL])|R],1,X).
retrievedata([Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],1,X) :-
	Y \== 1,
	retrievedata(R,Rout,1,X).	


retrievedata([],[4-([12],[r])],4,X) :- 
	/* FSR register - undefined at reset */
	/* What happens if this i 0? */
	X is 12.
retrievedata([4-([X|VL],RWL)|R],[4-([X|VL],[r|RWL])|R],4,X).
retrievedata([Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],4,X) :-
	Y \== 4,
	retrievedata(R,Rout,4,X).	
	

retrievedata([],[5-([0],[r])],5,X) :- 
	/* PORT A register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata([5-([X|VL],RWL)|R],[5-([X|VL],[r|RWL])|R],5,X).
retrievedata([Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],5,X) :-
	Y \== 5,
	retrievedata(R,Rout,5,X).	


retrievedata([],[6-([0],[r])],6,X) :- 
	/* PORT B register */
	/* Value should not be fetched from regs, but "Input data" */
	X is 0.
retrievedata([6-([X|VL],RWL)|R],[6-([X|VL],[r|RWL])|R],6,X).
retrievedata([Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],6,X) :-
	Y \== 6,
	retrievedata(R,Rout,6,X).	
	

	/* STATUS register - 0001 1xxx at reset */
retrievedata([],[3-([24],[r])],3,24). 
retrievedata([3-([X|VL],RWL)|R],[3-([X|VL],[r|RWL])|R],3,X).
retrievedata([Y-(VL,RWL)|R],[Y-(VL,RWL)|Rout],3,X) :-
	Y \== 3,
	retrievedata(R,Rout,3,X).	
	
	
/* remember to update Rin Rout */	
retrievedata(Rin,Rin,7,0). /* register 7 always read 0 */

retrievedata([],[R1-([0],[r])],R1,0) :- R1 > 11. /* At the moment, a register reads 0 until it has been
									written to */

/* NOT IMPLEMENTED YET: Append 'R' to RW-list */
retrievedata([R1-([Y|VL],RWL)|Rin],[R1-([Y|VL],[r|RWL])|Rin],R1,Y) :- R1 > 11.
retrievedata([R-(VL,RWL)|D],[R-(VL,RWL)|Rout],R1,Y):-
	R1 > 11,
	R \== R1,
	retrievedata(D,Rout,R1,Y).


/* Update value in hardware or data register */
/* INDF Register - indirect addressing */
updatedata(R,Rout,0,X) :-
	retrievedata(R,Ru1,4,Y),
	updatedata(Ru1,Rout,Y,X).

/* Timer register */
updatedata([], [1-([W],[w])], 1, W).
updatedata([1-(VL,RWL)|D],[1-([W|VL],[w|RWL])|D],1,W).
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],1,W):-
	X \== 1,
	updatedata(D,Dout,1,W).


/* Status register */
updatedata([], [3-([W],[w])], 3, W).
updatedata([3-(VL,RWL)|D],[3-([W|VL],[w|RWL])|D],3,W).
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],3,W):-
	X \== 3,
	updatedata(D,Dout,3,W).

/* FSR register */
updatedata([], [4-([W],[w])], 4, W).
updatedata([4-(VL,RWL)|D],[4-([W|VL],[w|RWL])|D],4,W).
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],4,W):-
	X \== 4,
	updatedata(D,Dout,4,W).


/* PORT A register */
updatedata([], [5-([W],[w])], 5, W).
updatedata([5-(VL,RWL)|D],[5-([W|VL],[w|RWL])|D],5,W).
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],5,W):-
	X \== 5,
	updatedata(D,Dout,5,W).

/* PORT B register */
updatedata([], [6-([W],[w])], 6, W).
updatedata([6-(VL,RWL)|D],[6-([W|VL],[w|RWL])|D],6,W).
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],6,W):-
	X \== 6,
	updatedata(D,Dout,6,W).


updatedata(R,R,7,_). /* register 7 stores nothing */


updatedata([], [R1-([W],[w])], R1, W) :-
	/* check boundries */
	R1 > 11,
	true.
updatedata([R1-(VL,RWL)|D],[R1-([W|VL],[w|RWL])|D],R1,W) :-
	/* check again */
	R1 > 11,
	true.
updatedata([X-(Y,RWL)|D],[X-(Y,RWL)|Dout],R1,W):-
	R1 > 11,
	X \== R1,
	updatedata(D,Dout,R1,W).


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
Registers=[],
nl.


/* Simulate HW */
simulatehw(Rin,Rout,Clock,ClockTicks) :-
	simulateinput(Rin,Rt1,Clock),
	simulatetimer(Rt1,Rout,ClockTicks).


/* Simulated input .... useable? */
simulateinput(Rin,Rout,Clock) :-
	Tick is (Clock mod 64),
	Tick == 0,
	retrievedata(Rin,Ru1,6,X),
	Z is X+1,
	updatedata(Ru1,Rout,6,Z).


simulateinput(Rin,Rin,Clock) :-
	Tick is (Clock mod 64),
	Tick > 0.

simulatetimer(Rin,Rout,Ticks) :-
	retrievedata(Rin,Rt1,1,X),
	Y is (X+Ticks) mod 255,
	updatedata(Rt1,Rout,1,Y).


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


returnpoint(24,[132,134,136,138,140,142,144,146]).
returnpoint(58,[109,114,119,124]).
returnpoint(91,[130]).
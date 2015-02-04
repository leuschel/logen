specialize_this :-
      asm(pic,A),
      environment(pic,B),
      simulate(A,[],B).
simulate(A,B,C) :-
      picwrite(A),
      picnl,
      initpic(D,E,F,G),
      execute(A,D,F,G).
execute(A,B,C,D) :-
      fetchinst(A,C,E,F,G),
      projectLive(C,B,H),
      reachableInstr(instr(C,I)),
      execinst(E,F,G,J,K,H,L,M,N,C,O,D,P,Q),
      R is S+Q,
      simulatehw(C,L,T,R,Q,U,V),
      projectLive(O,W,T),
      execute(A,W,O,P).
maxReg(19) :-
      true.
projectLive(A,B,C) :-
      genLiveState(A,0,B,C).
genLiveState(A,B,C,[B-D|E]) :-
      maxReg(F),
      B<F,
      dead(A,B),
      G is B+1,
      genLiveState(A,G,C,E).
genLiveState(A,B,[B-C|D],[B-E|F]) :-
      maxReg(G),
      B<G,
      live(A,B),
      E=C,
      H is B+1,
      genLiveState(A,H,D,F).
genLiveState(A,B,[],[]) :-
      maxReg(B).
fetchinst([pic(A,B,C,D)|E],A,B,C,D) :-
      true.
fetchinst([pic(A,B,C,D)|E],F,G,H,I) :-
      fetchinst(E,F,G,H,I).
reachableInstr(A) :-
      true.
reduceBits(A,A) :-
      true.
execinst(addlw,A,B,C,C,D,E,F,F,G,H,I,J,1) :-
      K is I+A,
      reduceBits(K,J),
      L is K>>8,
      updatez(G,D,M,J),
      updatec(G,M,E,L),
      H is G+1.
execinst(rvec,A,B,C,C,D,D,E,E,F,A,G,G,2) :-
      F is 0.
execinst(addwf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec addwf _,0'),
      picnl,
      retrievedata(F,C,J,A,K),
      L is K+H,
      reduceBits(L,I),
      M is L>>8,
      updatez(F,J,N,I),
      updatec(F,N,D,M),
      G is F+1.
execinst(addwf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec addwf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is J+H,
      reduceBits(K,L),
      M is K>>8,
      updatedata(F,I,N,A,L),
      updatez(F,N,O,L),
      updatec(F,O,D,M),
      G is F+1.
execinst(andlw,A,B,C,C,D,E,F,F,G,H,I,J,1) :-
      J is I/\A,
      updatez(G,D,E,J),
      H is G+1.
execinst(andwf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec andwf _,0'),
      picnl,
      retrievedata(F,C,J,A,K),
      I is K/\H,
      updatez(F,J,D,I),
      G is F+1.
execinst(andwf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec andwf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is J/\H,
      updatedata(F,I,L,A,K),
      updatez(F,L,D,K),
      G is F+1.
execinst(bcf,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      picwrite('exec bcf'),
      picnl,
      retrievedata(G,D,J,A,K),
      L is K/\(255-1<<B),
      updatedata(G,J,E,A,L),
      H is G+1.
execinst(bsf,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      picwrite('exec bsf'),
      picnl,
      retrievedata(G,D,J,A,K),
      L is K\/1<<B,
      updatedata(G,J,E,A,L),
      H is G+1.
execinst(btfss,A,B,C,C,D,E,F,F,G,H,I,I,2) :-
      picwrite('exec btfss'),
      picnl,
      retrievedata(G,D,E,A,J),
      K is J/\1<<B,
      K>=1,
      H is G+2.
execinst(btfss,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      picwrite('exec btfss'),
      picnl,
      retrievedata(G,D,E,A,J),
      K is J/\1<<B,
      K is 0,
      H is G+1,
      picwrite('Bit not set'),
      picnl.
execinst(btfsc,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      picwrite('exec btfsc'),
      picnl,
      retrievedata(G,D,E,A,J),
      K is J/\1<<B,
      K>=1,
      H is G+1,
      picwrite('Bit set'),
      picnl.
execinst(btfsc,A,B,C,C,D,E,F,F,G,H,I,I,2) :-
      picwrite('exec btfsc'),
      picnl,
      retrievedata(G,D,E,A,J),
      K is J/\1<<B,
      K is 0,
      H is G+2,
      picwrite('Bit not set'),
      picnl.
execinst(call,A,B,C,C,D,D,E,F,G,A,H,H,2) :-
      picwrite('exec call'),
      picnl,
      I is G+1,
      pushstack(E,F,I),
      picwrite('Stack '),
      picwrite(F),
      picnl.
execinst(clrf,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      picwrite('exec clrf'),
      picnl,
      updatedata(G,D,J,A,0),
      updatez(G,J,E,0),
      H is G+1,
      picwrite('Regs. '),
      picwrite(E),
      picnl.
execinst(clrw,A,B,C,C,D,E,F,F,G,H,I,0,1) :-
      picwrite('exec clrw'),
      picnl,
      updatez(G,D,E,0),
      H is G+1,
      picwrite('Wout 0'),
      picnl.
execinst(comf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec comf _,0'),
      picnl,
      retrievedata(F,C,J,A,K),
      L is\K,
      reduceBits(L,I),
      updatez(F,J,D,I),
      G is F+1.
execinst(comf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec comf _,0'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is\J,
      reduceBits(K,L),
      updatedata(F,I,M,A,L),
      updatez(F,M,D,L),
      G is F+1.
execinst(decf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec decf _,1'),
      picnl,
      retrievedata(F,C,J,A,K),
      L is K-1,
      reduceBits(L,I),
      updatez(F,J,D,I),
      G is F+1.
execinst(decf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec decf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is J-1,
      reduceBits(K,L),
      updatedata(F,I,M,A,L),
      updatez(F,M,D,L),
      G is F+1.
execinst(decfsz,A,0,B,B,C,D,E,E,F,G,H,I,2) :-
      retrievedata(F,C,D,A,J),
      J==1,
      G is F+2,
      picwrite('exec decfsz _,0 result 0'),
      picnl,
      I is 0.
execinst(decfsz,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      retrievedata(F,C,D,A,J),
      J\==1,
      picwrite('exec decfsz _,0 not 0'),
      picnl,
      K is J-1,
      reduceBits(K,I),
      G is F+1.
execinst(decfsz,A,1,B,B,C,D,E,E,F,G,H,H,2) :-
      retrievedata(F,C,I,A,J),
      J is 1,
      K is J-1,
      updatedata(F,I,D,A,K),
      G is F+2,
      picwrite('exec decfsz _,1 result 0'),
      picnl.
execinst(decfsz,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      retrievedata(F,C,I,A,J),
      J\==1,
      K is J-1,
      reduceBits(K,L),
      updatedata(F,I,D,A,L),
      G is F+1,
      picwrite('exec decfsz _,1 not 0'),
      picnl.
execinst(goto,A,B,C,C,D,D,E,E,F,A,G,G,2) :-
      picwrite('exec goto'),
      picnl.
execinst(incfsz,A,0,B,B,C,D,E,E,F,G,H,I,2) :-
      retrievedata(F,C,D,A,J),
      K is J+1,
      reduceBits(K,I),
      I==0,
      G is F+2,
      picwrite('exec incfsz _,0 result 0'),
      picnl.
execinst(incfsz,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      retrievedata(F,C,D,A,J),
      K is J+1,
      reduceBits(K,I),
      I>0,
      G is F+1,
      picwrite('exec incfsz _,0 not 0'),
      picnl.
execinst(incfsz,A,1,B,B,C,D,E,E,F,G,H,H,2) :-
      retrievedata(F,C,I,A,J),
      K is J+1,
      reduceBits(K,L),
      L==0,
      updatedata(F,I,D,A,L),
      G is F+2,
      picwrite('exec incfsz _,1 result 0'),
      picnl.
execinst(incfsz,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      retrievedata(F,C,I,A,J),
      K is J+1,
      reduceBits(K,L),
      L>0,
      updatedata(F,I,D,A,L),
      G is F+1,
      picwrite('exec incfsz _,1 not 0'),
      picnl.
execinst(incf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      retrievedata(F,C,J,A,K),
      L is K+1,
      reduceBits(L,I),
      updatez(F,J,D,I),
      G is F+1,
      picwrite('exec incf _,0'),
      picnl.
execinst(incf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      retrievedata(F,C,I,A,J),
      K is J+1,
      reduceBits(K,L),
      updatedata(F,I,M,A,N),
      updatez(F,M,D,N),
      G is F+1,
      picwrite('exec incf _,1'),
      picnl.
execinst(iorlw,A,B,C,C,D,E,F,F,G,H,I,J,1) :-
      picwrite('exec iorlw'),
      picnl,
      J is I\/A,
      updatez(G,D,E,J),
      H is G+1.
execinst(iorwf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec iorwf _,0'),
      picnl,
      retrievedata(F,C,J,A,K),
      I is H\/K,
      updatez(F,J,D,I),
      G is F+1.
execinst(iorwf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec iorwf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is H\/J,
      updatedata(F,I,L,A,K),
      updatez(F,L,D,K),
      G is F+1.
execinst(movf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec movf '),
      picwrite(A),
      picwrite(',0'),
      picnl,
      retrievedata(F,C,J,A,I),
      updatez(F,J,D,I),
      G is F+1.
execinst(movf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec movf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      updatez(F,I,D,J),
      G is F+1.
execinst(movlw,A,B,C,C,D,D,E,E,F,G,H,A,1) :-
      G is F+1,
      picwrite('exec movlw'),
      picnl.
execinst(movwf,A,B,C,C,D,E,F,F,G,H,I,I,1) :-
      updatedata(G,D,E,A,I),
      H is G+1,
      picwrite('exec movwf'),
      picnl.
execinst(nop,A,B,C,C,D,D,E,E,F,G,H,H,1) :-
      G is F+1,
      picwrite('exec nop'),
      picnl.
execinst(retlw,A,B,C,C,D,D,E,F,G,H,I,A,2) :-
      picwrite('exec retlw'),
      picnl,
      popstack(E,F,H),
      picwrite('Stack '),
      picwrite(F),
      picnl.
execinst(return,A,B,C,C,D,D,E,F,G,H,I,I,2) :-
      picwrite('exec return'),
      picnl,
      popstack(E,F,H),
      picwrite('Stack '),
      picwrite(F),
      picnl.
execinst(rlf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec rlf'),
      picnl,
      retrievedata(F,C,J,A,K),
      retrievec(F,J,L,M),
      N is K<<1+M,
      reduceBits(N,I),
      O is N>>8,
      updatec(F,L,D,O),
      G is F+1.
execinst(rlf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec rlf'),
      picnl,
      retrievedata(F,C,I,A,J),
      retrievec(F,I,K,L),
      M is J<<1+L,
      reduceBits(M,N),
      O is M>>8,
      updatedata(F,K,P,A,N),
      updatec(F,P,D,O),
      G is F+1.
execinst(rrf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec rlf'),
      picnl,
      retrievec(F,C,J,K),
      retrievedata(F,J,L,A,M),
      N is M/\1,
      I is M>>1+K<<7,
      updatec(F,L,D,N),
      G is F+1.
execinst(rrf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec rlf'),
      picnl,
      retrievec(F,C,I,J),
      retrievedata(F,I,K,A,L),
      M is L/\1,
      N is L>>1+J<<7,
      updatedata(F,K,O,A,N),
      updatec(F,O,D,M),
      G is F+1.
execinst(sublw,A,B,C,C,D,E,F,F,G,H,I,J,1) :-
      picwrite('exec sublw '),
      picnl,
      K is\I+1,
      L is A+K,
      reduceBits(L,J),
      M is L>>8,
      updatez(G,D,N,J),
      updatec(G,N,E,M),
      H is G+1.
execinst(subwf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec subwf _,0 '),
      picnl,
      retrievedata(F,C,J,A,K),
      L is\H+1,
      M is K+L,
      reduceBits(M,I),
      N is M>>8,
      updatez(F,J,O,I),
      updatec(F,O,D,N),
      G is F+1.
execinst(subwf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec subwf _,1'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is\H+1,
      L is J+K,
      reduceBits(L,M),
      N is L>>8,
      updatedata(F,I,O,A,M),
      updatez(F,O,P,M),
      updatec(F,P,D,N),
      G is F+1.
execinst(swapf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec swapf _,0'),
      picnl,
      retrievedata(F,C,D,A,J),
      I is(J/\240)>>4\/(J/\15)<<4,
      G is F+1.
execinst(swapf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec swapf _,0'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is(J/\240)>>4\/(J/\15)<<4,
      updatedata(F,I,D,A,K),
      G is F+1.
execinst(xorlw,A,B,C,C,D,E,F,F,G,H,I,J,1) :-
      picwrite('exec xorlw '),
      picnl,
      J is I#A,
      updatez(G,D,E,J),
      H is G+1.
execinst(xorwf,A,0,B,B,C,D,E,E,F,G,H,I,1) :-
      picwrite('exec xorwf _,0'),
      picnl,
      retrievedata(F,C,J,A,K),
      I is H#K,
      updatez(F,J,D,I),
      G is F+1.
execinst(xorwf,A,1,B,B,C,D,E,E,F,G,H,H,1) :-
      picwrite('exec xorwf _,0'),
      picnl,
      retrievedata(F,C,I,A,J),
      K is H#J,
      updatedata(F,I,L,A,K),
      updatez(F,L,D,K),
      G is F+1.
updatec(A,B,C,0) :-
      retrievedata(A,B,D,3,E),
      F is E/\254,
      updatedata(A,D,C,3,F).
updatec(A,B,C,D) :-
      D\==0,
      retrievedata(A,B,E,3,F),
      G is F\/1,
      updatedata(A,E,C,3,G).
updatez(A,B,C,0) :-
      retrievedata(A,B,D,3,E),
      F is E\/4,
      updatedata(A,D,C,3,F).
updatez(A,B,C,D) :-
      D\==0,
      retrievedata(A,B,E,3,F),
      G is F/\251,
      updatedata(A,E,C,3,G).
retrievec(A,B,C,D) :-
      retrievedata(A,B,C,3,E),
      D is E/\1.
retrievedata(A,B,B,C,D) :-
      retrievedatareg(0,B,C,D).
retrievedatareg(A,[A-B|C],D,E) :-
      maxReg(F),
      D<F,
      D\==A,
      G is A+1,
      retrievedatareg(G,C,D,E).
retrievedatareg(A,[A-B|C],A,B) :-
      maxReg(D),
      A<D,
      A>15.
retrievedatareg(A,B,C,0) :-
      maxReg(D),
      A is D.
retrievedatareg(0,A,0,B) :-
      retrievedata(C,A,D,4,E),
      retrievedata(C,A,F,E,B).
retrievedatareg(1,A,1,0) :-
      true.
retrievedatareg(2,[2-A|B],2,A) :-
      true.
retrievedatareg(3,[3-A|B],3,24) :-
      true.
retrievedatareg(4,[4-A|B],4,A) :-
      true.
retrievedatareg(5,[5-A|B],5,A) :-
      true.
retrievedatareg(6,[6-A|B],6,A) :-
      true.
retrievedatareg(7,[7-A|B],7,0) :-
      true.
retrievedatareg(8,[8-A|B],8,0) :-
      true.
retrievedatareg(9,[9-A|B],9,0) :-
      true.
retrievedatareg(10,[10-A|B],10,A) :-
      true.
retrievedatareg(11,[11-A|B],11,0) :-
      true.
retrievedatareg(12,[12-A|B],12,A) :-
      true.
retrievedatareg(13,[13-A|B],13,A) :-
      true.
retrievedatareg(14,[15-A|B],14,A) :-
      true.
retrievedatareg(15,[15-A|B],15,A) :-
      true.
updatedata(A,B,C,D,E) :-
      updatedatareg(0,B,C,D,E).
updatedatareg(A,[A-B|C],[A-B|D],E,F) :-
      maxReg(G),
      E<G,
      E\==A,
      H is A+1,
      updatedatareg(H,C,D,E,F).
updatedatareg(A,[A-B|C],[A-D|C],A,D) :-
      maxReg(E),
      A<E,
      A>15.
updatedatareg(0,A,B,0,C) :-
      retrievedata(D,A,E,4,F),
      updatedata(D,E,B,F,C).
updatedatareg(1,[1-A|B],[1-C|B],1,C) :-
      true.
updatedatareg(2,[2-A|B],[2-C|B],2,C) :-
      true.
updatedatareg(3,[3-A|B],[3-C|B],3,C) :-
      true.
updatedatareg(4,[4-A|B],[4-C|B],4,C) :-
      true.
updatedatareg(5,[5-A|B],[5-C|B],5,C) :-
      true.
updatedatareg(6,[6-A|B],[6-C|B],6,C) :-
      true.
updatedatareg(7,[7-A|B],[7-0|B],7,C) :-
      true.
updatedatareg(8,[8-A|B],[8-0|B],8,C) :-
      true.
updatedatareg(9,[9-A|B],[9-0|B],9,C) :-
      true.
updatedatareg(10,[10-A|B],[10-C|B],10,C) :-
      true.
updatedatareg(11,[11-A|B],[11-0|B],11,C) :-
      true.
updatedatareg(12,[12-A|B],[12-C|B],12,C) :-
      true.
updatedatareg(13,[13-A|B],[13-C|B],13,C) :-
      true.
updatedatareg(14,[14-A|B],[14-C|B],14,C) :-
      true.
updatedatareg(15,[15-A|B],[15-C|B],15,C) :-
      true.
pushstack(A,[B|A],B) :-
      true.
popstack([],[],0) :-
      true.
popstack([A|B],B,A) :-
      true.
memberReturnPoints(A,[A|B]) :-
      true.
memberReturnPoints(A,[B|C]) :-
      memberReturnPoints(A,C).
initpic(A,B,C,D) :-
      D=0,
      C=0,
      B=[],
      usedRegs(A),
      picnl.
usedRegs(A) :-
      genInit(0,A).
genInit(A,[A-0|B]) :-
      maxReg(C),
      A<C,
      live(0,A),
      D is A+1,
      genInit(D,B).
genInit(A,B) :-
      maxReg(C),
      A<C,
      dead(0,A),
      D is A+1,
      genInit(D,B).
genInit(A,[]) :-
      maxReg(A).
simulatehw(A,B,B,C,D,E,E) :-
      true.
simulateinput(A,B,C,D,E,F) :-
      retrievedata(A,B,G,6,H),
      checkInputStream(E,D,H,I,F),
      updatedata(A,G,C,6,I).
checkInputStream([(A,B)|C],D,E,E,[(A,B)|C]) :-
      A=<D.
checkInputStream([(A,B)|C],D,E,B,C) :-
      A>D.
headEnvInput([(A,B)|C],(A,B),C) :-
      true.
removeHeadEnvInput([],[]) :-
      true.
removeHeadEnvInput([A|B],B) :-
      true.
simulatetimer(A,B,C,D) :-
      retrievedata(A,B,E,1,F),
      G is(F+D)mod 256,
      updatedata(A,E,C,1,G).
genreturnpoints(A) :-
      asm(A,B),
      genretpoints(B,C),
      write(C).
genretpoints([],[]) :-
      true.
genretpoints([pic(A,call,B,C)|D],[E|F]) :-
      !,
      E is A+1,
      genretpoints(D,F).
genretpoints([pic(A,B,C,D)|E],F) :-
      genretpoints(E,F).
genpclist(A) :-
      asm(A,B),
      genpclist2(B,C),
      write(C),
      nl.
genpclist2([],[]) :-
      true.
genpclist2([pic(A,B,C,D)|E],[A|F]) :-
      genpclist2(E,F).
picwrite(A) :-
      true.
picnl :-
      true.
test :-
      specialize_this.
environment(pic,A) :-
      true.
live(2,16) :-
      true.
live(3,16) :-
      true.
live(3,17) :-
      true.
live(4,16) :-
      true.
live(5,16) :-
      true.
live(5,17) :-
      true.
live(6,16) :-
      true.
live(7,16) :-
      true.
live(6,17) :-
      true.
live(8,16) :-
      true.
live(9,16) :-
      true.
live(10,16) :-
      true.
live(11,16) :-
      true.
live(8,18) :-
      true.
live(9,18) :-
      true.
live(10,18) :-
      true.
live(11,18) :-
      true.
dead(0,16) :-
      true.
dead(1,16) :-
      true.
dead(0,17) :-
      true.
dead(1,17) :-
      true.
dead(2,17) :-
      true.
dead(0,18) :-
      true.
dead(1,18) :-
      true.
dead(2,18) :-
      true.
dead(3,18) :-
      true.
dead(4,17) :-
      true.
dead(7,17) :-
      true.
dead(4,18) :-
      true.
dead(5,18) :-
      true.
dead(6,18) :-
      true.
dead(7,18) :-
      true.
dead(8,17) :-
      true.
dead(9,17) :-
      true.
dead(10,17) :-
      true.
dead(11,17) :-
      true.
dead(12,16) :-
      true.
dead(12,17) :-
      true.
dead(12,18) :-
      true.
dead(A,0) :-
      true.
dead(A,1) :-
      true.
dead(A,2) :-
      true.
live(A,3) :-
      true.
dead(A,4) :-
      true.
dead(A,5) :-
      true.
dead(A,6) :-
      true.
dead(A,7) :-
      true.
dead(A,8) :-
      true.
dead(A,9) :-
      true.
dead(A,10) :-
      true.
dead(A,11) :-
      true.
dead(A,12) :-
      true.
dead(A,13) :-
      true.
dead(A,14) :-
      true.
dead(A,15) :-
      true.
asm(pic,A) :-
      A=[pic(0,nop,0,0),pic(1,movlw,64,0),pic(2,movwf,16,0),pic(3,movwf,17,0),pic(4,addwf,16,0),pic(5,movwf,17,0),pic(6,addwf,17,0),pic(7,movlw,4,0),pic(8,movwf,18,0),pic(9,addwf,16,1),pic(10,decfsz,18,1),pic(11,goto,9,0),pic(12,nop,0,0)].
returnpoint(A,[]) :-
      true.
execute(A,B,C,D,E,F,G,H) :-
      execute(A,B,E,F).





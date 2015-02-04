




comppeepopt(Pil,OptPil,Preds) :-    
     comppopt1(Pil, Pil1),
     comppopt4(Pil1,[],Dum,Preds,OptPil).

comppopt1([], []).
comppopt1([Inst|Rest], Pil1) :- 
   comppopt11(Inst, Rest, Pil1).

comppopt11(puttvar(T,R), [getstr(S,R)|PRest], [putstr(S,T)|OptPRest]) :-
   comppopt1a(PRest, OptPRest).
comppopt11(puttvar(T,R), [getlist(R)|PRest], [putlist(T)|OptPRest]) :-
   comppopt1a(PRest, OptPRest).
comppopt11(movreg(T,R),[Inst|PRest],OptInstList) :-
   T =:= R,
   comppopt11(Inst,PRest,OptInstList).
comppopt11(movreg(T,R),[Inst|PRest],OptInstList) :-
   T \== R,
   poptmovreg(Inst,R,T,PRest,OptInstList).
comppopt11(putpvar(V,R), [getpval(V,R)|PRest], [putpvar(V,R)|OptPRest]) :-
   comppopt1(PRest, OptPRest).     
comppopt11(putpvar(V,R), [getstr(Str,R)|PRest], [putstrv(Str,V)|OptPRest]) :-
   comppopt1a(PRest, OptPRest).
comppopt11(putpval(V,R), [getstr(Str,R)|PRest], [getstrv(Str,V)|OptPRest]) :-
   comppopt1(PRest, OptPRest).
comppopt11(getlist(R), [unitvar(R1),unitvar(R2)|PRest],
[getlisttvartvar(R,R1,R2)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(getcomma(R), [unitvar(R1),unitvar(R2)|PRest],
[getcommatvartvar(R,R1,R2)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(getlistk(R), [unitvar(R1),unitvar(R2)|PRest],
[getlistktvartvar(R,R1,R2)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(gettval(R,R), PRest,OptPRest) :-
   comppopt1(PRest, OptPRest).
comppopt11(unitvar(R), [movreg(R,S)|PRest], [unitvar(S)|OptPRest]) :-
   peepchk(PRest,R),
   comppopt1(PRest, OptPRest).
comppopt11(unitvar(R), [movreg(R,S)|PRest], 
[unitvar(R),movreg(R,S)|OptPRest]) :-
   comppopt1(PRest, OptPRest).
comppopt11(jump(L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(jump(Addr), [jump(Dum7)|PRest],  [jump(Addr)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(jumpz(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(jumpnz(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(jumplt(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest, OptPRest).
comppopt11(jumple(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest, OptPRest).
comppopt11(jumpgt(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(jumpge(Dum6,L), [label(L)|PRest],  [label(L)|OptPRest]) :-
   comppopt1(PRest,OptPRest).
comppopt11(Inst, PRest, [Inst|OptPRest]) :- 
   comppopt1(PRest, OptPRest).

comppopt1av([], []).
comppopt1a([Inst|PRest], [BldInst|OptPRest]) :-
  poptuni2bld(Inst,BldInst),
  comppopt1a(PRest,OptPRest).
comppopt1a([Inst|PRest], OptPList) :-
     comppopt11(Inst,PRest,OptPList).

poptuni2bld(unipvar(X), bldpvar(X)).
poptuni2bld(unipval(X), bldpval(X)).
poptuni2bld(unitvar(X), bldtvar(X)).
poptuni2bld(unitval(X), bldtval(X)).
poptuni2bld(unicon(X), bldcon(X)).
poptuni2bld(uninil, bldnil).
poptuni2bld(uninumcon(X), bldnumcon(X)).
poptuni2bld(unifloatcon(X), bldfloatcon(X)).

comppopt4([],Dum,Dum2,Preds,[]).
comppopt4([Inst|IRest],RCont,Seen,Preds,OList) :-
   poptbuiltin(Inst,Preds,OList,ORest),
   RCont1 = RCont,
   comppopt4(IRest,RCont1,Seen,Preds,ORest).
comppopt4([Inst|IRest],RCont,Seen,Preds,OList) :-
    peepredundant(Inst,IRest,RCont,RCont1,Seen,El),
    El =:= 1,
    OList = ORest,
   comppopt4(IRest,RCont1,Seen,Preds,ORest).
comppopt4([Inst|IRest],RCont,Seen,Preds,OList) :-
    peepredundant(Inst,IRest,RCont,RCont1,Seen,El),
    El \== 1,
    OList = [Inst|ORest],
    comppopt4(IRest,RCont1,Seen,Preds,ORest).

poptbuiltin(call(P,N,Dum7),Preds,[builtin(Bno)|IRest],IRest) :-
    compbuiltin(P,N,Bno),
    notmember1(slash(P,N),Preds).
poptbuiltin(calld(P,N,Dum7),Preds,[builtin(Bno)|IRest],IRest) :-
    compbuiltin(P,N,Bno),
    notmember1(slash(P,N),Preds).
poptbuiltin(execute(P,N),Preds,[builtin(Bno),proceed|IRest],IRest) :-
    compbuiltin(P,N,Bno),
    notmember1(slash(P,N),Preds).

compbuiltin(a,3,12).
compbuiltin(b,4,13).

notmember1(X,[]).
notmember1(X,[F|T]) :-
   X \== F,
   notmember1(X,T).

poptmovreg(Inst,R,T,PRest,OptInstList) :-
    poptmovreg0(Inst,R,T,OptInst), 
    peepchk(PRest,R),
    OptInstList = [OptInst|OptInstRest],
    comppopt1(PRest, OptInstRest).

poptmovreg(Inst,R,T,PRest,OptInstList) :-
    OptInstList = [movreg(T,R),Inst|OptInstRest],
    comppopt1(PRest, OptInstRest).

poptmovreg0(getstr(S,R),R,T,getstr(S,T)).
poptmovreg0(puttbreg(R),R,T,puttbreg(T)).
poptmovreg0(addreg(R,S),R,T,addreg(T,S)).
poptmovreg0(subreg(R,S),R,T,subreg(T,S)).
poptmovreg0(mulreg(R,S),R,T,mulreg(T,S)).
poptmovreg0(divreg(R,S),R,T,divreg(T,S)).
poptmovreg0(idivreg(R,S),R,T,idivreg(T,S)).
poptmovreg0(gettag(R,S),R,T,gettag(T,S)).
poptmovreg0(arg(R,R2,R3),R,T,arg(T,R2,R3)).
poptmovreg0(arg(R1,R,R3),R,T,arg(R1,T,R3)).
poptmovreg0(arg(R1,R2,R),R,T,arg(R1,R2,T)).
poptmovreg0(arg0(R,R2,R3),R,T,arg0(T,R2,R3)).
poptmovreg0(arg0(R1,R,R3),R,T,arg0(R1,T,R3)).
poptmovreg0(arg0(R1,R2,R),R,T,arg0(R1,R2,T)).
poptmovreg0(testunifiable(R,R2,R3),R,T,testunifiable(T,R2,R3)).
poptmovreg0(testunifiable(R1,R,R3),R,T,testunifiable(R1,T,R3)).
poptmovreg0(testunifiable(R1,R2,R),R,T,testunifiable(R1,R2,T)).


poptchkmember(P,[P|L1],1).
poptchkmember(P,[P|L1],0).
poptchkmember(P,[P1|L1],0) :-
  poptchkmember(P,L1,Flag).

peepuse(getcon(Dum6,R),R).
peepuse(getnumcon(Dum6,R),R).
peepuse(getfloatcon(Dum6,R),R).
peepuse(getpval(Dum6,R),R).
peepuse(gettval(Dum6,R),R).
peepuse(gettval(R,Dum7),R).
peepuse(gettbreg(R),R).
peepuse(getpbreg(R),R).
peepuse(getstr(Dum6,R),R).
peepuse(getstrv(Dum6,R),R).
peepuse(getlist(R),R).
peepuse(getlisttvartvar(R,Dum,Dum7),R).
peepuse(getcomma(R),R).
peepuse(getcommatvartvar(R,Dum,Dum7),R).
peepuse(gettag(R,Dum7),R).
peepuse(unitval(R),R).
peepuse(unipval(R),R).
peepuse(bldtval(R),R).
peepuse(bldpval(R),R).
peepuse(arg(R,Dum,Dum7),R).
peepuse(arg(Dum6,R,Dum7),R).
peepuse(arg(Dum6,Dum,R),R).
peepuse(arg0(R,Dum,Dum7),R).
peepuse(arg0(Dum6,R,Dum7),R).
peepuse(arg0(Dum6,Dum,R),R).
peepuse(testunifiable(R,Dum,Dum7),R).
peepuse(testunifiable(Dum6,R,Dum7),R).
peepuse(and(R,Dum7),R).
peepuse(and(Dum6,R),R).
peepuse(negate(R),R).
peepuse(or(R,Dum7),R).
peepuse(or(Dum6,R),R).
peepuse(lshiftl(R,Dum7),R).
peepuse(lshiftl(Dum6,R),R).
peepuse(lshiftr(R,Dum7),R).
peepuse(lshiftr(Dum6,R),R).
peepuse(addreg(R,Dum7),R).
peepuse(addreg(Dum6,R),R).
peepuse(subreg(R,Dum7),R).
peepuse(subreg(Dum6,R),R).
peepuse(mulreg(R,Dum7),R).
peepuse(mulreg(Dum6,R),R).
peepuse(divreg(R,Dum7),R).
peepuse(divreg(Dum6,R),R).
peepuse(idivreg(R,Dum7),R).
peepuse(idivreg(Dum6,R),R).
peepuse(movreg(R,Dum7),R).
peepuse(switchonterm(R,Dum,Dum7),R).
peepuse(switchonlist(R,Dum,Dum7),R).
peepuse(switchonbound(R,Dum,Dum7),R).
peepuse(jump(Dum7),Dum6).
peepuse(jumpeq(R,L),R) :- L \== abs(1).
peepuse(jumpne(R,L),R) :- L \== abs(1).
peepuse(jumplt(R,L),R) :- L \== abs(1).
peepuse(jumple(R,L),R) :- L \== abs(1).
peepuse(jumpgt(R,L),R) :- L \== abs(1).
peepuse(jumpge(R,L),R) :- L \== abs(1).

peepchk([],Dum7).
peepchk([Inst|Rest],R) :-
   peepterm(Inst,R).
peepchk([Inst|Rest],R) :-
    peepchk(Rest,R).

peepterm(call(Dum6,Dum,Dum7),Dum8).
peepterm(calld(Dum6,Dum,Dum7),Dum8).
peepterm(execute(Dum7),Dum8).
peepterm(execmarker,Dum7).
peepterm(putcon(R),R).
peepterm(putnumcon(R),R).
peepterm(putfloatcon(R),R).
peepterm(puttvar(R,Dum8),R).
peepterm(putpvar(Dum6,R),R).
peepterm(putdval(Dum6,R),R).
peepterm(putuval(Dum6,R),R).
peepterm(puttbreg(R),R).
peepterm(putpval(Dum6,R),R).
peepterm(putstr(Dum6,R),R).
peepterm(putstrv(Dum6,R),R).
peepterm(putlist(R),R).
peepterm(putnil(R),R).
peepterm(gettag(Dum6,R),R).
peepterm(movreg(Dum6,R),R).
peepterm(bldtvar(R),R).
peepterm(testunifiable(Dum6,Dum,R),R).

peepredundant(execmarker,Dum,R,R,Dum1,1).
peepredundant(Inst,IRest,RCont,RCont1,Seen,El) :-
   peepelim(Inst,IRest,RCont,RCont1,Seen,El).
peepredundant(Inst,IRest,RCont,RCont1,Seen,El) :-
       RCont1 = RCont, El = 0.

peepelim(getpvar(V,R),Dum,RCont,[r(R,v(V))|RCont],Dum2,0).
peepelim(getpval(V,R),Dum,RCont,RCont1,Seen,El) :-
   member1(r(R,v(V)),RCont),
        El = 1, RCont1 = RCont.
peepelim(getpval(V,R),Dum,RCont,RCont1,Seen,El) :-
        El = 0, RCont1 = [r(R,v(V))|RCont].
peepelim(getcon(C,R),Dum,RCont,RCont1,Seen,El) :-
   member1(r(R,c(C)),RCont), 
   El = 1 , RCont1 = RCont.
peepelim(getcon(C,R),Dum,RCont,RCont1,Seen,El) :-
        El = 0 , RCont1 = [r(R,c(C))|RCont].
peepelim(getnumcon(N,R),Dum,RCont,RCont1,Seen,El) :-
   member1(r(R,n(N)),RCont),
   El = 1 , RCont1 = RCont .
peepelim(getnumcon(N,R),Dum,RCont,RCont1,Seen,El) :-
   El = 0 , RCont1 = [r(R,n(N))|RCont].
peepelim(getfloatcon(N,R),Dum,RCont,RCont1,Seen,El) :-
   member1(r(R,nf(N)),RCont),
   El = 1 , RCont1 = RCont.
peepelim(getfloatcon(N,R),Dum,RCont,RCont1,Seen,El) :-
   El = 0 , RCont1 = [r(R,nf(N))|RCont].
peepelim(getnil(R),Dum,RCont,RCont1,Seen,El) :-
   member1(r(R,c([])),RCont),
   El = 1 , RCont1 = RCont.
peepelim(getnil(R),Dum,RCont,RCont1,Seen,El) :-
   El = 0 , RCont1 = [r(R,c([]))|RCont].
peepelim(putpvar(V,R),Dum,L0,L1,Dum2,0) :- 
        peepelimupd(L0,R,v(V),L1).
peepelim(putpval(V,R),Dum,RCont,RCont1,Dum2,El) :-
   member1(r(R,v(V)),RCont),
   El = 1 , RCont1 = RCont .
peepelim(putpval(V,R),Dum,RCont,RCont1,Dum2,El) :-
   El = 0 , peepelimupd(RCont,R,v(V),RCont1).
peepelim(puttvar(R,R1),Dum,L0,L1,Dum2,0) :-
   peepdel(L0,r(R,Dum7),L2), peepdel(L2,r(R1,Dum7),L1).
peepelim(putcon(C,R),Dum,RCont,RCont1,Dum2,El) :-
   member1(r(R,c(C)),RCont),
   El = 1 , RCont1 = RCont .
peepelim(putcon(C,R),Dum,RCont,RCont1,Dum2,El) :-
   El = 0 , peepelimupd(RCont,R,c(C),RCont1).
peepelim(putnumcon(N,R),Dum,RCont,RCont1,Dum2,El) :-
   member1(r(R,n(N)),RCont),
   El = 1 , RCont1 = RCont .
peepelim(putnumcon(N,R),Dum,RCont,RCont1,Dum2,El) :-
   El = 0 , peepelimupd(RCont,R,n(N),RCont1).
peepelim(putfloatcon(N,R),Dum,RCont,RCont1,Dum2,El) :-
   member1(r(R,nf(N)),RCont),
   El = 1 , RCont1 = RCont .
peepelim(putfloatcon(N,R),Dum,RCont,RCont1,Dum2,El) :-
   El = 0 , peepelimupd(RCont,R,nf(N),RCont1).
peepelim(putnil(R),Dum,RCont,RCont1,Dum2,El) :-
   member1(r(R,c([])),RCont),
   El = 1 , RCont1 = RCont .
peepelim(putnil(R),Dum,RCont,RCont1,Dum2,El) :-
   El = 0 , peepelimupd(RCont,R,c([]),RCont1).
peepelim(putstr(F,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(putlist(R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(and(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(or(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(negate(R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(lshiftr(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(lshiftl(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(addreg(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(subreg(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(mulreg(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(divreg(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(idivreg(Dum6,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(movreg(R,R1),Dum,L0,L1,Dum2,0) :- peepelimupd(L0,R1,r(R),L1).
peepelim(gettbreg(R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(putdval(V,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(putuval(V,R),Dum,L0,L1,Dum2,0) :- peepdel(L0,r(R,Dum7),L1).
peepelim(label(P,N,K),Dum,Dum2,[],Seen,0) :-
   N >= 0,
        member1(s(P,N),Seen).
peepelim(call(Dum6,Dum,Dum7),Dum2,Dum4,[],Dum5,0).
peepelim(proceed,Dum,Dum2,[],Dum4,0).
peepelim(execute(s(P,N)),IRest,Dum,[],Seen,El) :-
   IRest = [label(P,N,K)|Dummy],
        N >= 0,
   poptchkmember(s(P,N),Seen,El).
peepelim(execute(s(P,N)),IRest,Dum,[],Seen,El) :-
   El = 0.
peepelim(calld(Dum6,Dum2,Dum7),Dum,Dum4,[],Dum5,0).
peepelim(builtin(Dum7),Dum,Dum2,[],Dum4,0).
peepelim(trymeelse(Dum6,Dum7),Dum,Dum2,[],Dum5,0).
peepelim(retrymeelse(Dum6,Dum7),Dum,Dum2,[],Dum4,0).
peepelim(trustmeelsefail(Dum7),Dum,Dum2,[],Dum4,0).
peepelim(try(Dum6,Dum7),Dum,Dum2,[],Dum4,0).
peepelim(retry(Dum6,Dum7),Dum,Dum2,[],Dum4,0).
peepelim(trust(Dum7),Dum,Dum2,[],Dum4,0).
peepelim(jump(Dum7),Dum,Dum2,[],Dum4,0).
peepelim(jumpz(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0 .
peepelim(jumpz(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [] .
peepelim(jumpnz(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0 .
peepelim(jumpnz(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [] .
peepelim(jumplt(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0.
peepelim(jumplt(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [].
peepelim(jumple(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0.
peepelim(jumple(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [].
apeepelim(jumpgt(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0.
peepelim(jumpgt(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [].
peepelim(jumpge(Dum6,L),Dum,R0,R1,Dum2,0) :- L = abs(1), R1 = R0.
peepelim(jumpge(Dum6,L),Dum,R0,R1,Dum2,0) :- R1 = [].
peepelim(switchonterm(Dum6,Dum,Dum7),Dum2,Dum4,[],Dum1,0).
peepelim(switchonlist(Dum6,Dum,Dum7),Dum2,Dum4,[],Dum5,0).
peepelim(switchonbound(Dum6,Dum,Dum7),Dum2,Dum4,[],Dum5,0).

peepdel([],Dum,[]).
peepdel([X|L],Y,L1) :- 
   X = Y,
        L1 = L1Rest,
   peepdel(L,Y,L1Rest).
peepdel([X|L],Y,L1) :- 
        L1 = [X|L1Rest],
   peepdel(L,Y,L1Rest).

peepelimupd(L0,R,Cont,[r(R,Cont)|L1]) :- 
        peepdel(L0,r(R,Dum7),L1).

member1(X,[X|Y]).
member1(X,[F|T]) :-
   member1(X,T).



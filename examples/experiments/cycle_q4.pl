/* cycle.pl with rule for q/1 added */

:- ensure_loaded(xsb_emu_for_sics).

:- table(tcp/2).
tcp(X1,X2):-p(X1,X2).
tcp(X1,X2):-p(X1,X3),tcp(X3,X2).

:- table(cycle/2).
cycle(X1,X2):-p(X1,X2).
cycle(X1,X2):-cycle(X1,X3),p(X3,X2).

:- table(q/1).
q(X) :- p(X,Y), \+(q(Y)).

%%%%%%%%%%%%%%%%%%%%%%

ura(steve,r1).

active(steve,r1).

pra(r53,read,p(_,_)).
pra(r53,read,cycle(_,_)).
pra(r53,read,tcp(_,_)).
pra(r53,read,q(_)).

:- table(holds_read/2).

holds_read(User,not(Object)) :- \+(holds_read(User,Object)).
holds_read(_User,Object) :- built_in(Object).
holds_read(User,Object) :- permitted(User,read,Object), fact(Object),
  call(Object).
holds_read(User,Object) :- permitted(User,read,Object), derived(Object), holds_read_rule(User,Object).

holds_read_rule(User,Object) :- rule(Object,Body),
  l_holds_read(User,Body).

l_holds_read(_U,[]).
l_holds_read(U,[H|T]) :- holds_read(U,H), l_holds_read(U,T).

built_in('='(X,X)).
built_in('is'(X,Y)) :- X is Y.

holds(U,O):- holds_read(U,O).

permitted(User,Op,Obj) :- ura(User,Role),  active(User,Role),
                           seniorto(Role,R2), pra(R2,Op,Obj).

fact(p(_X,_Y)).
derived(cycle(_,_)).
derived(tcp(_,_)).
derived(q(_)).

rule(cycle(X1,X2),[p(X1,X2)]).
rule(cycle(X1,X2),[cycle(X1,X3),p(X3,X2)]).
rule(tcp(X1,X2),[p(X1,X2)]).
rule(tcp(X1,X2),[p(X1,X3),tcp(X3,X2)]).
rule(q(X),[p(X,Y),not(q(Y))]).


b2 :- holds_read(steve,cycle(_,_)),fail.
b2.

bench :- ensure_loaded('database_cycle'),
    abolish_all_tables,
        cputime(T1),
	b2,
	cputime(T2), R is T2-T1, print(R),nl.
	
	
b4 :- holds_read(steve,q(_X)),fail.
b4.

bench4 :- ensure_loaded('database_cycle'),
    abolish_all_tables,
        cputime(T1),
	b4,
	cputime(T2), R is T2-T1, print(R),nl.
	

bench4_optimal :- ensure_loaded('database_cycle'),
    abolish_all_tables,
        cputime(T1),
	b4_opt,
	cputime(T2), R is T2-T1, print(R),nl.
	
b4_opt :- q(_X),fail.
b4_opt.

%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(database_cycle).

%%%%%%%%%%%%%%%%%%%%%%%

/*
How to do experiments:

UNPSPECIALISED:

SICS:
SICStus 3.11.1 (powerpc-darwin-7.2.0): Fri Feb 20 18:44:10 CET 2004
Licensed to ecs.soton.ac.uk
| ?- bench4.
9570
yes
| ?- bench4_optimal.
50

XSB:

| ?- [cycle_q4].
[Compiling ./cycle_q4]
++Warning[XSB]: [Compiler] ./cycle_q4: Singleton variable X in a clause of b4/0
++Warning[XSB]: [Compiler] ./cycle_q4: Singleton variable X in a clause of b4_opt/0
% Specialising partially instantiated calls to holds_read/2
++Warning[XSB]: [Compiler] ./cycle_q4: The specialisation of holds_read/2 will cause double tabling !
           (possible source of inefficiency)
[cycle_q4 compiled, cpu time used: 0.1000 seconds]
[cycle_q4 loaded]
[database_cycle loaded]

yes
| ?- bench4.
0.1110

yes
| ?- bench4_opt.
++Error[XSB/Runtime/P]: Undefined predicate: bench4_opt/0
Aborting...
| ?- bench4_optimal.
0.0200

yes

*/



/*


seniorto(r1, r1).
seniorto(r2, r2).
seniorto(r3, r3).
seniorto(r4, r4).
seniorto(r5, r5).
seniorto(r6, r6).
seniorto(r7, r7).
seniorto(r8, r8).
seniorto(r9, r9).
seniorto(r10, r10).
seniorto(r11, r11).
seniorto(r12, r12).
seniorto(r13, r13).
seniorto(r14, r14).
seniorto(r15, r15).
seniorto(r16, r16).
seniorto(r17, r17).
seniorto(r18, r18).
seniorto(r19, r19).
seniorto(r20, r20).
seniorto(r21, r21).
seniorto(r22, r22).
seniorto(r23, r23).
seniorto(r24, r24).
seniorto(r25, r25).
seniorto(r26, r26).
seniorto(r27, r27).
seniorto(r28, r28).
seniorto(r29, r29).
seniorto(r30, r30).
seniorto(r31, r31).
seniorto(r32, r32).
seniorto(r33, r33).
seniorto(r34, r34).
seniorto(r35, r35).
seniorto(r36, r36).
seniorto(r37, r37).
seniorto(r38, r38).
seniorto(r39, r39).
seniorto(r40, r40).
seniorto(r41, r41).
seniorto(r42, r42).
seniorto(r43, r43).
seniorto(r44, r44).
seniorto(r45, r45).
seniorto(r46, r46).
seniorto(r47, r47).
seniorto(r48, r48).
seniorto(r49, r49).
seniorto(r50, r50).
seniorto(r51, r51).
seniorto(r52, r52).
seniorto(r53, r53).
seniorto(r1, r2).
seniorto(r1, r3).
seniorto(r1, r4).
seniorto(r2, r5).
seniorto(r2, r6).
seniorto(r2, r7).
seniorto(r3, r8).
seniorto(r3, r9).
seniorto(r3, r10).
seniorto(r4, r11).
seniorto(r4, r12).
seniorto(r4, r13).
seniorto(r5, r14).
seniorto(r5, r15).
seniorto(r5, r16).
seniorto(r6, r17).
seniorto(r6, r18).
seniorto(r6, r19).
seniorto(r7, r20).
seniorto(r7, r21).
seniorto(r7, r22).
seniorto(r8, r23).
seniorto(r8, r24).
seniorto(r8, r25).
seniorto(r9, r26).
seniorto(r9, r27).
seniorto(r9, r28).
seniorto(r10, r29).
seniorto(r10, r30).
seniorto(r10, r31).
seniorto(r11, r32).
seniorto(r11, r33).
seniorto(r11, r34).
seniorto(r12, r35).
seniorto(r12, r36).
seniorto(r12, r37).
seniorto(r13, r38).
seniorto(r13, r39).
seniorto(r13, r40).
seniorto(r14, r41).
seniorto(r15, r41).
seniorto(r16, r41).
seniorto(r17, r42).
seniorto(r18, r42).
seniorto(r19, r42).
seniorto(r20, r43).
seniorto(r21, r43).
seniorto(r22, r43).
seniorto(r23, r44).
seniorto(r24, r44).
seniorto(r25, r44).
seniorto(r26, r45).
seniorto(r27, r45).
seniorto(r28, r45).
seniorto(r29, r46).
seniorto(r30, r46).
seniorto(r31, r46).
seniorto(r32, r47).
seniorto(r33, r47).
seniorto(r34, r47).
seniorto(r35, r48).
seniorto(r36, r48).
seniorto(r37, r48).
seniorto(r38, r49).
seniorto(r39, r49).
seniorto(r40, r49).
seniorto(r41, r50).
seniorto(r42, r50).
seniorto(r43, r50).
seniorto(r44, r51).
seniorto(r45, r51).
seniorto(r46, r51).
seniorto(r47, r52).
seniorto(r48, r52).
seniorto(r49, r52).
seniorto(r50, r53).
seniorto(r51, r53).
seniorto(r52, r53).
seniorto(r1, r5).
seniorto(r1, r6).
seniorto(r1, r7).
seniorto(r1, r14).
seniorto(r1, r15).
seniorto(r1, r16).
seniorto(r1, r41).
seniorto(r1, r50).
seniorto(r1, r53).
seniorto(r1, r17).
seniorto(r1, r18).
seniorto(r1, r19).
seniorto(r1, r42).
seniorto(r1, r20).
seniorto(r1, r21).
seniorto(r1, r22).
seniorto(r1, r43).
seniorto(r1, r8).
seniorto(r1, r9).
seniorto(r1, r10).
seniorto(r1, r23).
seniorto(r1, r24).
seniorto(r1, r25).
seniorto(r1, r44).
seniorto(r1, r51).
seniorto(r1, r26).
seniorto(r1, r27).
seniorto(r1, r28).
seniorto(r1, r45).
seniorto(r1, r29).
seniorto(r1, r30).
seniorto(r1, r31).
seniorto(r1, r46).
seniorto(r1, r11).
seniorto(r1, r12).
seniorto(r1, r13).
seniorto(r1, r32).
seniorto(r1, r33).
seniorto(r1, r34).
seniorto(r1, r47).
seniorto(r1, r52).
seniorto(r1, r35).
seniorto(r1, r36).
seniorto(r1, r37).
seniorto(r1, r48).
seniorto(r1, r38).
seniorto(r1, r39).
seniorto(r1, r40).
seniorto(r1, r49).
seniorto(r2, r14).
seniorto(r2, r15).
seniorto(r2, r16).
seniorto(r2, r41).
seniorto(r2, r50).
seniorto(r2, r53).
seniorto(r2, r17).
seniorto(r2, r18).
seniorto(r2, r19).
seniorto(r2, r42).
seniorto(r2, r20).
seniorto(r2, r21).
seniorto(r2, r22).
seniorto(r2, r43).
seniorto(r3, r23).
seniorto(r3, r24).
seniorto(r3, r25).
seniorto(r3, r44).
seniorto(r3, r51).
seniorto(r3, r53).
seniorto(r3, r26).
seniorto(r3, r27).
seniorto(r3, r28).
seniorto(r3, r45).
seniorto(r3, r29).
seniorto(r3, r30).
seniorto(r3, r31).
seniorto(r3, r46).
seniorto(r4, r32).
seniorto(r4, r33).
seniorto(r4, r34).
seniorto(r4, r47).
seniorto(r4, r52).
seniorto(r4, r53).
seniorto(r4, r35).
seniorto(r4, r36).
seniorto(r4, r37).
seniorto(r4, r48).
seniorto(r4, r38).
seniorto(r4, r39).
seniorto(r4, r40).
seniorto(r4, r49).
seniorto(r5, r41).
seniorto(r5, r50).
seniorto(r5, r53).
seniorto(r6, r42).
seniorto(r6, r50).
seniorto(r6, r53).
seniorto(r7, r43).
seniorto(r7, r50).
seniorto(r7, r53).
seniorto(r8, r44).
seniorto(r8, r51).
seniorto(r8, r53).
seniorto(r9, r45).
seniorto(r9, r51).
seniorto(r9, r53).
seniorto(r10, r46).
seniorto(r10, r51).
seniorto(r10, r53).
seniorto(r11, r47).
seniorto(r11, r52).
seniorto(r11, r53).
seniorto(r12, r48).
seniorto(r12, r52).
seniorto(r12, r53).
seniorto(r13, r49).
seniorto(r13, r52).
seniorto(r13, r53).
seniorto(r14, r50).
seniorto(r14, r53).
seniorto(r15, r50).
seniorto(r15, r53).
seniorto(r16, r50).
seniorto(r16, r53).
seniorto(r17, r50).
seniorto(r17, r53).
seniorto(r18, r50).
seniorto(r18, r53).
seniorto(r19, r50).
seniorto(r19, r53).
seniorto(r20, r50).
seniorto(r20, r53).
seniorto(r21, r50).
seniorto(r21, r53).
seniorto(r22, r50).
seniorto(r22, r53).
seniorto(r23, r51).
seniorto(r23, r53).
seniorto(r24, r51).
seniorto(r24, r53).
seniorto(r25, r51).
seniorto(r25, r53).
seniorto(r26, r51).
seniorto(r26, r53).
seniorto(r27, r51).
seniorto(r27, r53).
seniorto(r28, r51).
seniorto(r28, r53).
seniorto(r29, r51).
seniorto(r29, r53).
seniorto(r30, r51).
seniorto(r30, r53).
seniorto(r31, r51).
seniorto(r31, r53).
seniorto(r32, r52).
seniorto(r32, r53).
seniorto(r33, r52).
seniorto(r33, r53).
seniorto(r34, r52).
seniorto(r34, r53).
seniorto(r35, r52).
seniorto(r35, r53).
seniorto(r36, r52).
seniorto(r36, r53).
seniorto(r37, r52).
seniorto(r37, r53).
seniorto(r38, r52).
seniorto(r38, r53).
seniorto(r39, r52).
seniorto(r39, r53).
seniorto(r40, r52).
seniorto(r40, r53).
seniorto(r41, r53).
seniorto(r42, r53).
seniorto(r43, r53).
seniorto(r44, r53).
seniorto(r45, r53).
seniorto(r46, r53).
seniorto(r47, r53).
seniorto(r48, r53).
seniorto(r49, r53).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%











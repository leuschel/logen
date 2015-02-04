
:- include('../logen_source/logen').

:- residual sclause/2.
:- filter sclause(semi,semi):sclause.
logen(sclause, sclause(p(X),X=a)).
logen(sclause, sclause(r(X),(X=a;X=b))).
%logen(sclause, sclause(r(X),(X=b))).
logen(sclause, sclause(z(X),X=c)).
logen(sclause, sclause(q(X),(not(mycall(z(X))),not(mycall(p(X))),mycall(r(X))))).

logen(sclause, sclause(mem(X,List), (when((nonvar(List),nonvar(X)),List = [X|_]); (List = [_|Tail], mycall(mem(X,Tail)))))).

logen(sclause, sclause(memNew(X,List), (List = [XPrime|Tail], (X = XPrime ; mycall(memNew(X,Tail)))))).

logen(sclause, sclause(app(A,B,C),((A = [], C = B) ; (A = [X|Tail], C = [CX|NewTail], X=CX, mycall(app(Tail, B, NewTail)))))).


:- residual or/2.
:- filter or(semi,semi):or.
logen(or, or(A,_)) :-
        logen(unfold, safe(A)).
logen(or, or(A,B)) :-
        logen(unfold, safe_not(A)),
        logen(unfold, safe(B)).

:- residual and/2.
:- filter and(semi,semi):and.
logen(and, and(A,B)) :-
        logen(unfold, safe(A)),
        logen(unfold, safe(B)).

:- residual safe/1.
:- filter safe(semi):safe.
logen(safe, safe((A,B))) :-
        logen(unfold, and(A,B)).
logen(safe, safe((A;B))) :-
        logen(unfold, or(A,B)).
logen(safe, safe(X=Y)) :-
        logen(rescall, X=Y).
logen(safe, safe(when(Trigger, Call))) :-
	logen(rescall, when(Trigger, safe(Call))).
logen(safe, safe(not(Call))) :-
        logen(unfold, safe_not(Call)).
logen(safe, safe(mycall(Call))) :-
	logen(memo, smycall(Call)).


:- residual smycall/1.
:- filter smycall(nonvar):smycall.
logen(smycall, smycall(Call)) :- 
        logen(unfold, sclause(Call,Body)),
        logen(unfold, safe(Body)).

:- residual snmycall/1.
:- filter snmycall(nonvar):snmycall.
logen(snmycall, snmycall(Call)) :-
	logen(unfold, sclause(Call,Body)),
	logen(unfold, safe_not(Body)).



:- residual safe_not/1.
:- filter safe_not(semi):safe_not.
logen(safe_not, safe_not((A,B))) :-
        logen(unfold, or(not(A),not(B))).
logen(safe_not, safe_not((A;B))) :-
        logen(unfold, and(not(A),not(B))).
logen(safe_not, safe_not(X=Y)) :-
        logen(rescall, when(nonvar(X),X\=Y)).
logen(safe_not, safe_not(when(Trigger, Call))) :-
	logen(rescall, when(Trigger, safe(not(Call)))).
logen(safe_not, safe_not(not(Call))) :-
        logen(unfold, safe(Call)).
logen(safe_not, safe_not(mycall(Call))) :-
	logen(memo, snmycall(Call)).








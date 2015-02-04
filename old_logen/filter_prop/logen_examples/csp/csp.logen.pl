
/* ------------------------------------- */
/*            A CSP INTERPRETER          */
/* ------------------------------------- */
/* simplified version to be fed into LOGEN and/or ECCE */
/* can also be plugged into CTL interpreter */
/* (c) 2000,2001 Michael Leuschel */
      
trans(stop,_,_) :- fail.
trans(skip,tau,stop).

trans(ccs_choice(X,_Y),A,X1) :- trans(X,A,X1).  /* WARNING: still CCS choice */
trans(ccs_choice(_X,Y),A,Y1) :- trans(Y,A,Y1).

trans(choice(X,_Y),A,X1) :- trans(X,A,X1),dif(A,tau).
trans(choice(_X,Y),A,Y1) :- trans(Y,A,Y1),dif(A,tau).
trans(choice(X,Y),tau,choice(X1,Y)) :- trans(X,tau,X1).
trans(choice(X,Y),tau,choice(X,Y1)) :- trans(Y,tau,Y1).

trans(int_choice(X,_Y),tau,X).
trans(int_choice(_X,Y),tau,Y).

trans(if(Test,Then,_Else),A,X1) :- test(Test), trans(Then,A,X1).
trans(if(Test,_Then,Else),A,X1) :- \+(test(Test)), trans(Else,A,X1).
trans(if(Test,Then),A,X1) :- test(Test), trans(Then,A,X1).

trans(prefix(V,Ch,X),  io(V,Ch) ,X).
trans(prefix(V,Ch,Constraint,X), io(V,Ch) ,X) :-
    test(Constraint).


trans(seq(P,Q),A,seq(P1,Q)) :- trans(P,A,P1),dif(A,tick).
trans(seq(P,Q),tau,Q) :- trans(P,tick,_).
     
trans(interleave(X,Y),A,R) :- trans(par(X,[],Y),A,R).

trans(timeout(P,_Q),A,P1) :- dif(A,tau),trans(P,A,P1).
trans(timeout(P,Q),tau,timeout(P1,Q)) :- trans(P,tau,P1).
trans(timeout(_P,Q),tau,Q).

trans(interrupt(P,Q),A,interrupt(P1,Q)) :- dif(A,tick),trans(P,A,P1).
trans(interrupt(P,_),tick,omega) :- trans(P,tick,_).
trans(interrupt(_,Q),i,Q).
   
trans(par(X,CList,Y), io(V,Ch), par(X1,CList,Y1)) :-
     trans(X, io(V1,Ch), X1),
     trans(Y, io(V2,Ch), Y1), 
     unify_values(V1,V2,V),
     hidden(io(V,Ch),CList).
trans(par(X,CList,Y), A, par(X1,CList,Y) ) :- 
  trans(X,A,X1),dif(A,tick),not_hidden(A,CList). /* covers tau */
trans(par(X,CList,Y), A, par(X,CList,Y1) ) :- 
  trans(Y,A,Y1),dif(A,tick),not_hidden(A,CList). /* covers tau */
trans(par(X,CList,Y), tau, par(omega,CList,Y) ) :- trans(X,tick,_).
trans(par(X,CList,Y), tau, par(X,CList,omega) ) :- trans(Y,tick,_).
trans(par(omega,_,omega), tick, omega ).

trans(agent_call(X),A,X1) :- evaluate_agent_call(X,EX),
 agent(EX,AE),trans(AE,A,X1).

trans(hide(Expr,CList), A, hide(X,CList) ) :-
	trans(Expr,A,X),dif(A,tick),not_hidden(A,CList).
trans(hide(Expr,CList), tau, hide(X,CList) ) :-
	trans(Expr,A,X),hidden(A,CList).
trans(hide(Expr,_CList), tick, omega) :- 
	trans(Expr,tick,_X).

trans(rename(Expr,RenList), RA, rename(X,RenList) ) :- 
	trans(Expr,A,X),
	rename_action(A,RenList,RA).

trans(let(V,VExp,CExp),tau,CExp) :-
  evaluate_argument(VExp,EVExp),
  V=EVExp.
	
unify_values([],[],[]).
unify_values([V1|T1],[V2|T2],[V|T]) :- unify_value(V1,V2,V),
 unify_values(T1,T2,T).

/* just necessary for properly printing values */
unify_value(in(X),in(X),in(X)).
unify_value(in(X),out(X),dot(X)).
unify_value(in(X),dot(X),dot(X)).
unify_value(out(X),in(X),dot(X)).
unify_value(out(X),out(X),out(X)).
unify_value(out(X),dot(X),dot(X)).
unify_value(dot(X),in(X),dot(X)).
unify_value(dot(X),out(X),dot(X)).
unify_value(dot(X),dot(X),dot(X)).

/* the real negation of unify_value: */
dif_value(in(X),in(Y)) :- dif(X,Y).
dif_value(in(X),out(Y)) :- dif(X,Y).
dif_value(in(X),dot(Y)) :- dif(X,Y).
dif_value(out(X),in(Y)) :- dif(X,Y).
dif_value(out(X),out(Y)) :- dif(X,Y).
dif_value(out(X),dot(Y)) :- dif(X,Y).
dif_value(dot(X),in(Y)) :- dif(X,Y).
dif_value(dot(X),out(Y)) :- dif(X,Y).
dif_value(dot(X),dot(Y)) :- dif(X,Y).



evaluate_agent_call(AgentCall,EAgentCall) :-
    AgentCall =..[AName|Par],
    l_evaluate_arguments(Par,EPar),
    EAgentCall =.. [AName|EPar].

l_evaluate_arguments([],[]).
l_evaluate_arguments([A|T],[EA|ET]) :-
    evaluate_argument(A,EA),l_evaluate_arguments(T,ET).

evaluate_argument(eval(V),EV) :-
  evaluate_argument2(V,EV).
evaluate_argument(keep(V),V).

evaluate_argument2('+'(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   Res is EX+EY.
evaluate_argument2('-'(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   Res is EX+EY.
evaluate_argument2(union(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   multi_set_union(EX,EY,Res).
evaluate_argument2(diff(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   multi_set_difference(EX,EY,Res).
evaluate_argument2(rem(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   multi_set_difference(EX,[EY],Res).
evaluate_argument2(add(X,Y),Res) :- 
   evaluate_argument(X,EX),evaluate_argument(Y,EY),
   multi_set_union(EX,[EY],Res).
evaluate_argument2(card(X),Res) :- 
   evaluate_argument(X,EX),
   multi_set_cardinality(EX,Res).
   

test(true).
test(false) :- fail.
test(not(X)) :- when( ground(X), \+(test(X))).
test(and(X,Y)) :- test(X),test(Y).
test(or(X,_Y)) :- test(X).
test(or(_X,Y)) :- test(Y).
test(=(X,X)).
test('!='(X,Y)) :- dif(X,Y).
test('>'(X,Y)) :- when( ground((X,Y)), X>Y).
test('<'(X,Y)) :- when( ground((X,Y)), X<Y).
test(in(X,Set)) :- when( nonvar(Set), member_test(X,Set)).

member_test(X,[X|_]).
member_test(X,[_|T]) :- when( nonvar(T), member_test(X,T)).

not_hidden(tau,_).
not_hidden(tick,_).
not_hidden(io(V,Ch),CList) :-
     not_hidden_test(io(V,Ch),CList).
not_hidden_test(_,[]).
not_hidden_test(X,[X2|T]) :- not_match_action(X,X2),not_hidden_test(X,T).
not_match_action(io(_,Ch),io(_,Ch2)) :- dif(Ch,Ch2).
not_match_action(io(V,Ch),io(V2,Ch)) :- not_match_action2(V,V2).
not_match_action2([H1|_X],[H2|_Y]) :- dif_value(H1,H2).
not_match_action2([_H1|X],[_H2|Y]) :- not_match_action2(X,Y).
  
hidden(tau,_) :- fail.
hidden(tick,_) :- fail.
hidden(io(V,Ch),  CList) :- 
     when( nonvar(CList), hidden_test(io(V,Ch),CList)).
hidden_test(X,[X2|_]) :- match_action(X,X2).
hidden_test(X,[_|T]) :-  hidden_test(X,T).
match_action(io(V,Ch),io(V2,Ch)) :- 
 when( nonvar(V2), match_action2(V,V2)).
match_action2(_X,[]).
match_action2([H1|X],[H2|Y]) :-
   when(nonvar(H1), unify_value(H1,H2,_)),
   when(nonvar(Y), match_action2(X,Y)).


rename_action(tau,_,tau).
rename_action(tick,_,tick).
rename_action(A,RList,RA) :- dif(A,tau), dif(A,tick),
  rename_action2(A,RList,RA,not_found).
  
rename_action2(X,[],X,not_found). /* only allow if X not yet found */
rename_action2(X,['<-'(X2,RX2)|_],Res,_NF) :-
         match_action(X,X2),
         rename_action3(X,X2,RX2,Res).
rename_action2(X,['<-'(X2,_RX2)|T],RX,_NF) :-
         match_action(X,X2),  /* allow relational renaming */
	 rename_action2(X,T,RX,found).
rename_action2(X,['<-'(_X2,_RX2)|T],RX,NF) :-
         not_match_action(X,_X2),
	 rename_action2(X,T,RX,NF).

rename_action3(io(V1,Ch),io(V2,Ch),io(V3,Ch3),io(Res,Ch3)) :-
       rename_action4(V1,V2,V3,Res).
rename_action4(V1,[],V3,Res) :- append(V3,V1,Res).
rename_action4([_|V1],[_|V2],V3,Res) :- 
   rename_action4(V1,V2,V3,Res).


prop(State,A) :- trans(State,A,_).
prop(State,deadlock) :- \+(live(State)).
live(State) :- trans(State,_A,_S).  

/* --------------------- */
/* auxiliary definitions */
/* --------------------- */

app([],L,L).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

rev([],A,A).
rev([H|X],A,R) :- rev(X,[H|A],R).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).


member_nr(X,[X|_],1).
member_nr(X,[_|T],N) :-  member_nr(X,T,TN), N is TN + 1.

append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).

delete_el([X|T],X,T).
delete_el([Y|T],X,[Y|DT]) :- /* \+(X=Y), */  delete_el(T,X,DT).

remove([],_X,[]).
remove([X|T],X,T).
remove([Y|T],X,[Y|DT]) :-  \+(X=Y),   remove(T,X,DT).

multi_set_difference(S1,[],S1).
multi_set_difference(S1,[X|S2],Res) :-
   remove(S1,X,S11),
   multi_set_difference(S11,S2,Res).

multi_set_union(S1,S2,Res) :- append(S1,S2,Res).

multi_set_cardinality([],0).
multi_set_cardinality([_H|T],R) :- multi_set_cardinality(T,CT), R is CT+1.

string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).

   
/* -------------------------------------------------------------------- */


/* ------------------------ */
/* Sample Agent Definitions */
/* ------------------------ */

:- dynamic agent/2.

agent(a_TrafficLight ,
   prefix([],yellow,prefix([],green,agent_call(a_TrafficLightGo)))).
agent(a_TrafficLightGo ,
   prefix([],yellow,prefix([],red,agent_call(a_TrafficLight)))).
agent(a_Counter(s(X)) ,
   prefix([],down,agent_call(a_Counter(keep(X))))).
agent(a_Counter(X) ,
   prefix([],up,agent_call(a_Counter(keep(s(X)))))).
agent(a_Counter(0) ,
   prefix([],zero,agent_call(a_Counter(keep(0))))).
agent(a_Counter(_X) ,
   prefix([],reset,agent_call(a_Counter(keep(0))))).
agent(a_MAIN ,
   prefix([],traffic,agent_call(a_TrafficLight))).
agent(a_MAIN ,
   prefix([],counter,agent_call(a_Counter(keep(0))))).


/* -------------------------------------------------------------------- */

/* a simple verifier */

unsafe(Expr,deadlock) :- \+live(Expr).

verify(Expr,[],Expr,ErrorDesc) :- unsafe(Expr,ErrorDesc).
verify(Expr,[A|Tr],Res,ErrorDesc) :-
        trans(Expr,A,NewExpr),
        verify(NewExpr,Tr,Res,ErrorDesc).

/* ------------------------------------- */
/* a simple iterative deepening verifier */
/* ------------------------------------- */


iv :- iv(a_MAIN).
iv(X) :- id_verify(agent_call(X)).

id_verify(Expr) :- 
 print('Intial expression: '),print_expr(Expr),nl,
 print('Starting an iterative deepening search '),
 list(Tr),print('.'),
 verify(Expr,Tr,Res,ErrorDesc),
 nl,print('*** error found: '),
 print(ErrorDesc), print(' ! ***'),nl,
 print('Trace: '),print_value_list(Tr),nl,
 print('Final expression: '),print_expr(Res),nl,
 print('Continue y or n ==>'),
 read(Y), Y='n'.
 
 list([]).
 list([_H|T]) :- list(T). 

 
/* ------------------------------------- */
 
trace(T) :- trace(agent_call(a_MAIN),T).
 
trace(_Expr,[]).
trace(Expr,[A|Tr]) :-
        trans(Expr,A,NewExpr),
	simplify(NewExpr,SE),
        trace(SE,Tr).


simplify(stop,stop).
simplify(skip,skip).
simplify(omega,omega).
simplify(choice(X,Y),Res) :-
    simplify(X,SX),simplify(Y,SY),
    ((X=stop) -> (Res=SY) ;
      ((Y=stop) -> (Res=SX) ; (Res = choice(SX,SY)))).
simplify(int_choice(X,Y),int_choice(SX,SY)) :-
    simplify(X,SX),simplify(Y,SY).
simplify(prefix(V,Ch,X),prefix(V,Ch,SX)) :-
    simplify(X,SX).
simplify(prefix(V,Ch,C,X),prefix(V,Ch,C,SX)) :-
    simplify(X,SX).
simplify(interleave(X,Y),par(SX,[],SY)) :-
    simplify(X,SX),simplify(Y,SY).
simplify(par(X,C,Y),par(SX,C,SY)) :-
    simplify(X,SX),simplify(Y,SY).
simplify(timeout(X,Y),timeout(SX,SY)) :-
    simplify(X,SX),simplify(Y,SY).
simplify(interrupt(X,Y),interrupt(SX,SY)) :-
    simplify(X,SX),simplify(Y,SY).
simplify(agent_call(X),agent_call(X)).
simplify(hide(X,C),Res) :-
    simplify(X,SX),
    ((SX = hide(SY,C)) -> (Res = hide(SY,C))
      ; (Res = hide(SX,C))
    ).
simplify(restrict(X,C),restrict(SX,C)) :-
    simplify(X,SX),
    ((SX = restrict(SY,C)) -> (Res = restrict(SY,C))
      ; (Res = hide(SX,C))
    ).
simplify(rename(X,R),rename(SX,R)) :-
    simplify(X,SX).
simplify(let(V,VE,X),let(V,VE,SX)) :-
    simplify(X,SX).

/* add more simplification stuff + better normalisation
  to improve termination */

/* Model checking algorithm:
    Csp -> Parser/Compiler -> Prolog
    Prolog (+ CTL) -> Logen -> Simplified Prolog  (only works for some Csp)
    Simplified Prolog -> Ecce ->  Yes/No
*/
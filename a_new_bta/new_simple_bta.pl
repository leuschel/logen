
 
/* --------------------------------- */
/* The dynamic calls_answers table  */
/* --------------------------------- */

:- dynamic change/0.

pp :- (change -> print(change) ; nl),nl,
      calls_answers(A,B,user,P,Ans),
        print(A/B), print(' '), print(P),
	print(' -> '), print(Ans),
	nl,
      fail.
pp :- nl.

%calls(app,3,[s,s,s]).
%answers(app,3,[s,s,s]).

:- dynamic calls_answers/5.

%calls_answers(dapp,4,user,[s,s,s,d],[s,s,s,s]).
calls_answers(test,3,user,[s,d,d],[s,s,s]).
calls_answers(test,1,user,[d],[s]).
calls_answers(test_memo,2,user,[d,s],[s,s]).

calls_answers(is,2,built_in,[d,s],[s,s]).
calls_answers(is,2,built_in,[s,s],[s,s]).
calls_answers(=,2,built_in,[s,s],[s,s]).
calls_answers(=,2,built_in,[d,s],[s,s]).
calls_answers(=,2,built_in,[s,d],[s,s]).
calls_answers(=,2,built_in,[d,d],[d,d]).
calls_answers(length,2,built_in,[s,d],[s,s]).
calls_answers(length,2,built_in,[d,d],[d,s]).
calls_answers(length,2,built_in,[d,s],[d,s]).
calls_answers(length,2,built_in,[s,s],[s,s]).
calls_answers(print,1,built_in,[s],[s]).
calls_answers(print,1,built_in,[d],[d]).
calls_answers(nl,0,built_in,[],[]).
calls_answers(\=,2,built_in,X,X) :- sd_list(X,2).

sd_list([],0).
sd_list([H|T],X) :- X>0, X1 is X-1, (H=s ; H=d), sd_list(T,X1).

is_built_in(Fun,Arity) :- calls_answers(Fun,Arity,built_in,_,_).

/* --------------------------------- */
/*    The program to be analyzed     */
/* --------------------------------- */
:- dynamic app/3.

app([],X,X).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

:- dynamic dapp/4.
dapp(X,Y,Z,R) :- app(X,Y,XY), app(XY,Z,R).

:- dynamic test/3.
test(X,R,Len) :- dapp(X,X,X,R1), app(R,R,R1),length(R1,Len),print(Len),nl.


:- dynamic int/4.
int(cst(X),_,_,X).
int(var(X),Vars,Vals,R) :- lookup(X,Vars,Vals,R).
int(plus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX+RY.
int(minus(X,Y),Vars,Vals,Res) :- int(X,Vars,Vals,RX), int(Y,Vars,Vals,RY), Res is RX-RY.
int(fun(X),Vars,Vals,Res) :- def0(X,Def), int(Def,Vars,Vals,Res).
%int(fun(X,Arg),Vars,Vals,Res) :- 
%   def1(X,Var,Def), int(Arg,Vars,Vals,ResArg),
%   int(Def,[Var|Vars],[ResArg|Vals],Res).
:- dynamic def0/2.
def0(one,cst(1)).
def0(rec,fun(rec)).

%def1(inc,xx,plus(var(xx),cst(1))).
%def1(rec,xx,fun(rec,var(xx))).
:- dynamic lookup/4.
lookup(X,[X|_],[Val|_],Val).
lookup(X,[Y|T],[_|ValT],Res) :-
   X \= Y, lookup(X,T,ValT,Res).

:- dynamic test/1.
test(R) :- int(minus(plus(var(xx),cst(4)),var(zz)),[aa,bb,cc,dd,ee,ff,gg,zz,yy,xx],[0,0,0,0,0,0,0,1,2,3],R).
%test2(R) :- int(minus(plus(var(xx),cst(4)),fun(inc,var(zz))),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).
%test_rec(R) :- int(fun(rec),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).

:- dynamic test_memo/2.
test_memo(X,RR) :- app(X,X,R),app(R,R,RR).
test_memo(X,_RR) :- (Y=[a] ; Y=[b]), dapp(Y,Y,Y,X).
/*
Output for above:
Analysis finished

test/3 [s,d,d] -> [s,s,s]
test/1 [d] -> [s]
dapp/4 [s,s,s,d] -> [s,s,s,s]
app/3 [d,d,s] -> [s,s,s]
int/4 [s,s,s,d] -> [s,s,s,s]
app/3 [s,s,d] -> [s,s,s]
lookup/4 [s,s,s,d] -> [s,s,s,s]
def0/2 [s,d] -> [s,s]

*/
/* --------------------------------- */

/* THE INTERFACE TO THE SIZE CHANGE ANALYSIS */
/* here mimicked by some facts */

is_not_terminating(app,3,[d,d,d]).
is_not_terminating(dapp,4,[d,d,d,d]).
is_not_terminating(lookup,4,[_,d,d,_]).
is_not_terminating(int,4,[d,_,_,_]).

generalize(_,_,X,X).

/* --------------------------------- */

/* The main loop of the analysis */
analyze :- pp, retractall(change),
   print(analyze),nl,
   calls_answers(Fun,Arity,user,Pattern,AnsPat),
   functor(Call,Fun,Arity),
   clause(Call,Body),
   Call =.. [Fun|Args],
   insert_static_vars(Pattern,Args,[],InStaticVars),
   print(in_static(Call,InStaticVars)),nl,
   check_body(Body,InStaticVars,OutSV),
   print(out_static(Call,OutSV)),nl,
   /* check that answer is ok */
   print(ans(AnsPat)),nl,
   get_pattern(Call,NewAnsPattern,OutSV),
   lub_pat(AnsPat,NewAnsPattern,Lub),
   print(ans_lub(AnsPat,NewAnsPattern,Lub)),nl,
   (Lub=AnsPat
    -> true ; (assert_change, 
	       retract(calls_answers(Fun,Arity,user,Pattern,AnsPat)), 
	       assert(calls_answers(Fun,Arity,user,Pattern,Lub)))),
   fail.
analyze :- (change -> analyze ; (nl,print('Analysis finished'),nl,nl,pp)).

assert_change :- change -> true ; assert(change).

/* check_body(BodyOfClause,ListofStaticVars,ListofStaticVarsAfterExecutingBody) */
check_body(true,In,In) :- !.
check_body((A,B),StaticVars,OutSV2) :- !,
	check_body(A,StaticVars,OutSV),
	check_body(B,OutSV,OutSV2).
check_body((A;B),StaticVars,OutSV) :- !,
	check_body(A,StaticVars,OutSV1),
	check_body(B,StaticVars,OutSV2),
	exact_inter(OutSV1,OutSV2,OutSV).
/* to do: add more cases:  if-then-else, ... */
check_body(T,InSV,OutSV) :- transparent(T,InnerT,INST),!,check_body(InnerT,InSV,II),
	 (INST=instantiate -> OutSV = II ; OutSV = InSV).
check_body(Call,InSV,OutSV) :-
	functor(Call,Fun,Arity),
        get_pattern(Call,CallPattern,InSV),
	(is_not_terminating(Fun,Arity,CallPattern)  /* TO DO: a call is memoed if any possible pattern is unsafe */
	 -> (generalize(Fun,Arity,CallPattern,GenCallPattern),
	     print(memo(Call,GenCallPattern)),nl,
	     check_call_pattern(Fun,Arity,GenCallPattern,_AnswPat),
	     OutSV=InSV
	    )
	 ;   check_unfold_call(Call,CallPattern,Fun,Arity,InSV,OutSV)
	).

check_unfold_call(Call,CallPattern,Fun,Arity,InSV,OutSV) :-
	check_call_pattern(Fun,Arity,CallPattern,AnswPat),
	Call =.. [Fun|Args],
	insert_static_vars(AnswPat,Args,InSV,OutSV),
	print(out_sv(OutSV)),nl.

check_call_pattern(Fun,Arity,CallPattern,AnswPat) :-
	(calls_answers(Fun,Arity,_Module,CallPattern,AnswPat)  
	 -> true /* we already have an entry for this call pattern */
	 ;  (is_built_in(Fun,Arity)
	      -> (nl,print(illegal_call_to_builtin(Fun,Arity,CallPattern)),nl,
		  print('******'),nl  /* to do: make rescall */
		 )
	      ;  (print(no_calls_answers(Fun,Arity)),nl, assert_change,
	          set_static(CallPattern,AnswPat),
	          assert(calls_answers(Fun,Arity,user,CallPattern,AnswPat))
		 )
	    )
	).

transparent(when(_,X),X,instantiate).
transparent(findall(_,X,_),X,no).
transparent(\+(X),X,no).
transparent('->'(X,Y),(X,Y),instantiate).

lub_pat([],[],[]).
lub_pat([X|T1],[Y|T2],[Z|T3]) :- lub(X,Y,Z), lub_pat(T1,T2,T3).
lub(s,X,X).
lub(d,_,d).
lub(nv,s,nv).
lub(nv,nv,nv).
lub(nv,d,d).
/* note : treatment of nonvar not yet optimal: nonvar information is not propagated yet
 (we would need to keep track of the list of nonvar Variables) */

get_pattern(Call,Pattern,StaticVars) :- Call =.. [_Fun|Args], get_arg_sd_vals(Args,Pattern,StaticVars).
get_arg_sd_vals([],[],_).
get_arg_sd_vals([H|T],[SD|TSD],SV) :-
	term_variables(H,Vars),
	(exact_subset(Vars,SV) -> SD = s
	   ; (nonvar(H) -> SD = nv ; SD = d)),
	get_arg_sd_vals(T,TSD,SV).
		   
:- use_module(library(terms)).

insert_static_vars([],[],R,R).
insert_static_vars([s|T],[S|TT],In,Out) :- !,
   term_variables(S,Vars),
   l_insert(Vars,In,In2),
   insert_static_vars(T,TT,In2,Out).
insert_static_vars([_|T],[_|TT],In,Out) :-
   insert_static_vars(T,TT,In,Out).

set_static([],[]).
set_static([_|T],[s|ST]) :- set_static(T,ST).

l_insert([],R,R).
l_insert([V|T],In,Out) :- insert(In,V,In2),
  l_insert(T,In2,Out).

insert([],X,[X]).
insert([H|T],X,R) :- (H==X -> R=[H|T] ; (R=[H|R2],insert(T,X,R2))).

exact_subset([],_).
exact_subset([H|T],L) :- exact_del(H,L,L2), exact_subset(T,L2).

exact_del(X,[Y|T],R) :-
   ((X==Y) -> R=T ; (R=[Y|R2],exact_del(X,T,R2))).


exact_inter([],_R,[]).
exact_inter([H1|T1],L2,Res) :-
	(exact_del(H1,L2,L2D)
	 -> Res = [H1|RR]
	 ;  (L2D=L2, Res=RR)
	),exact_inter(T1,L2D,RR).
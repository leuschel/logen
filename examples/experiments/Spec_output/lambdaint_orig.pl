test :- statistics(runtime,_), bench(5,24), statistics(runtime,[_,T2]),
   nl, print(T2), print(' ms'),nl.
   
   
%:- mode l_eval(i,i,o).

l_eval([],_E,[]).
l_eval([H|T],E,[EH|ET]) :-
	eval(H,E,EH),
	l_eval(T,E,ET).

%:- mode eval(i,i,o).

/* eval(X,E,R) :- print(eval(X,E)),nl,fail. */
 eval(cst(C),_Env,constr(C,[])).
eval(constr(C,Args),Env,constr(C,EArgs)) :-
	l_eval(Args,Env,EArgs).
eval(var(VKey),Env,Val) :- lookup(VKey,Env,Val).
eval(plus(X,Y),Env,constr(XY,[])) :-
	eval(X,Env,constr(VX,[])),
	eval(Y,Env,constr(VY,[])),
	XY is VX + VY.
eval(minus(X,Y),Env,constr(XY,[])) :-
	eval(X,Env,constr(VX,[])),
	eval(Y,Env,constr(VY,[])),
	XY is VX - VY.
eval(times(X,Y),Env,constr(XY,[])) :-
	eval(X,Env,constr(VX,[])),
	eval(Y,Env,constr(VY,[])),
	XY is VX * VY.
eval(eq(X,Y),Env,constr(BoolRes,[])) :-
	eval(X,Env,VX),
	eval(Y,Env,VY),
	(VX=VY -> (BoolRes = true) ; (BoolRes = false)).
eval(let(VKey,VExpr,InExpr),Env,Result) :-
        eval(VExpr,Env,VVal),
        store(Env,VKey,VVal,InEnv),
        eval(InExpr,InEnv,Result).
eval(if(Test,Then,Else), Env, Res) :-
    eval_if(Test,Then,Else,Env,Res).

%eval(if(Test,Then,Else),Env,Res) :-
%	eval(Test,Env,TestRes),
%	((TestRes = constr(true,[]))
%	  -> eval(Then,Env,Res)
%      ;  eval(Else,Env,Res)
%     ).
eval(lambda(X,Expr),_Env,lambda(X,Expr)).
eval(apply(Arg,F),Env,Res) :-
	eval(F,Env,FVal),
	rename(FVal,Env,lambda(X,Expr)),
	eval(Arg,Env,ArgVal),
        store(Env,X,ArgVal,NewEnv),
	eval(Expr,NewEnv,Res).
eval(fun(F),_,FunDef) :- function(F,FunDef).
eval(print(X),_,constr(true,[])) :-
   print(X),nl.

rename(Expr,_Env,RenExpr) :- RenExpr = Expr.

eval_if(Test, Then, _Else, Env, Res) :-
    test(Test,Env),
    !,
    eval(Then,Env,Res).

eval_if(_Test,_Then,Else, Env, Res) :-
    eval(Else,Env,Res).

test(eq(X,Y), Env) :-
    eval(X,Env,VX),
    eval(Y,Env,VX).



function(fib, lambda(x,
                     if(eq(var(x),cst(0)),
                         cst(1),
                         if(eq(var(x),cst(1)),
                            cst(1),
                            plus(apply(minus(var(x),cst(1)),fun(fib)),
                                 apply(minus(var(x),cst(2)),fun(fib)))
                            )
                        )
                    )).
     
%:- mode store(i,i,i,o).

store([],Key,Value,[Key/Value]).
store([Key/_Value2|T],Key,Value,[Key/Value|T]).
store([Key2/Value2|T],Key,Value,[Key2/Value2|BT]) :-
   Key \== Key2,
   store(T,Key,Value,BT).

%:- mode lookup(i,i,o).

lookup(Key,[Key/Value|_T],Value).
lookup(Key,[Key2/_Value2|T],Value) :-
   Key \== Key2,
   lookup(Key,T,Value).
   
   

fib(X,FibX) :-
                store([],x,X,Env),
                eval(apply(cst(X),fun(fib)),Env,constr(FibX,_)).

bench(Low,Up) :- Low>Up, print('Done'),nl.

bench(Low,Up) :-
    Low =< Up,
    fib(Low,Res),!,
    print(fib(Low)), print(' == '), print(Res),nl,
    L1 is Low+1,
%    print('debug'),
    bench(L1,Up).



%:- bench(20,27).


co:-reconsult('tlambda.pl').

%strict evaluation as default
%eval(app(F,Exp),Env,Res):-eval(app_strict(F,Exp),Env,Res).


%default : call by by need
eval(app(F,Exp),Env,Res):-eval(app_lazy(F,Exp),Env,Res).


eval(var(VKey),Env,Val) :- lookup(VKey,Env,Val).

eval(cst(X),_Env,num(X)).

eval(plus(X,Y),Env,num(XY)) :-
	eval(X,Env,num(VX)),
	eval(Y,Env,num(VY)),
	XY is VX + VY.

eval(minus(X,Y),Env,num(XY)) :-
	eval(X,Env,num(VX)),
	eval(Y,Env,num(VY)),
	XY is VX - VY.

eval(mul(X,Y),Env,num(XY)) :-
	eval(X,Env,num(VX)),
	eval(Y,Env,num(VY)),
	XY is VX * VY.

eval(if(Test,Then,Else), Env, Res) :-
    eval_if(Test,Then,Else,Env,Res).

eval(lam(Var,Expr), Env, fun(Var,Expr,Env)).

eval(app_strict(F,Expr),Env,Res):-
    eval(F,Env,Fres),
    eval(Expr,Env,Eres),
    app_strict(Fres,Eres,Res).


eval(app_lazy(F,Expr),Env,Res):-
%    print(beg_laz(F,Expr,Env)),nl,
    when( nonvar(Res),
           (
 %            print(cal_laz(F,Expr,Env)),nl,
             eval(F,Env,Fkt),
 %            print(eval(F,Env,Fkt)),nl,
             eval(Expr,Env,Eres),
 %            print(eval(Expr,Env,Eres)),nl,
             app_strict(Fkt,Eres,Res)
%            ,print(end_laz(Res)),nl
           )).


eval(print(X),_,true) :-
   print(X),nl.


eval_if(Test, Then, _Else, Env, Res) :-
    test_eq(Test,Env),
    !,
    eval(Then,Env,Res).

eval_if(_Test,_Then,Else, Env, Res) :-
    eval(Else,Env,Res).

test_eq(eq(X,Y), Env) :-
    eval(X,Env,VX),
    eval(Y,Env,VX).



% this here is for strict evaluation
app_strict(fun(Var,Expr,Env),Val,Res):-!,
              eval(Expr,[(Var/Val)|Env],Res).

%app non function to a Value
app_strict(_,_,typeerror). 


lookup(_,[],badvar) :-print('unbound var'),nl,!,fail.

lookup(Key,[Key/Value|_T],Value).

lookup(Key,[Key2/_Value2|T],Value) :-
   Key \== Key2,
   lookup(Key,T,Value).
   

%here start the examples:
%term(name,Term) is used to build complex terms.
%test for sum-expression

term(tst_sum, plus( cst(1), cst(3) )).

%square funktion
term(fkt_square, lam( x, mul( var(x), var(x) ))).

term(test_square, app(Fkt,cst(3))):-term(fkt_square,Fkt).

% fn f=>fn y=> f (f y)
term(fkt_twice,lam( f,
                           lam( x,
                                     app( var(f) , app( var(f) , var(x) ))
                                 )
                         )).


%buid a power of 4 fkt by apping fkt_twice to fkt_square
term(fkt_power4,app(Fkt_twice,Fkt_square)):-term(fkt_twice,Fkt_twice),term(fkt_square,Fkt_square).

%test the power4 fkt with const 7
term(test_power4,app(Fkt_power4,cst(7))):-term(fkt_power4,Fkt_power4).


term(id,lam(i,var(i))).

%Y = \f.(\x.f (x x)) (\x.f (x x))
%ycomb
term(ycomb,lam(f,app(E2,E2))):-term(help_exp,E2).
%part of fixpoint-operator
%f is a free var in help_exp
term(help_exp,lam(x,app(var(f),app(var(x),var(x))))).


term(fkt_u,lam(f,app(var(f),var(f)))).

term(fib_r2, lam(r,lam(x,
                     if(eq(var(x),cst(0)),
                         cst(1),
                         if(eq(var(x),cst(1)),
                            cst(1),
                            plus(app(var(r),minus(var(x),cst(1))),
                                 app(var(r),minus(var(x),cst(2))))
                            )
                        )
                    ))).


term(fib_r, lam(r,lam(x,
                     if(eq(var(x),cst(0)),
                         cst(1),
                         if(eq(var(x),cst(1)),
                            cst(1),
                            plus(app(app(var(r),var(r)),minus(var(x),cst(1))),
                                 app(app(var(r),var(r)),minus(var(x),cst(2))))
                            )
                        )
                    ))).


term(fib,app(Ycom,F)):-term(ycomb,Ycom),term( fib_r2,F).

%term(fib,app(Fkt_u,Fib_r)):-term( fkt_u, Fkt_u),term( fib_r,Fib_r).

term(fib_x(X),app(Fib,cst(X))):-term(fib,Fib).

term(fak_r, lam(r,lam(x,
                     if(eq(var(x),cst(1)),
                         cst(1),
                         mul(var(x),
                               app(var(r),minus(var(x),cst(1))))
                            )
                        )
                    )).


term(fak,app(Y,Fak_r)):-term( ycomb, Y),term( fak_r,Fak_r).
term(fak_x(X),app(F,cst(X))):-term(fak,F).


%this may return lazy(X)
test(Term_name,X):-term( Term_name, Tterm ), eval( Tterm, [] , X ).

%test for eager
test_e(Term,X):-test(Term,num(X)),!.

%basic tests
t1(X):-test(test_square,X).
te1(X):-test_e(test_square,X).
t2(X):-test(test_power4,X).
te2(X):-test_e(test_power4,X).
t3(X):-test(fib_x(4),X).
te3(X):-test_e(fib_x(4),X).


bench(Low,Up) :- Low>Up, print('Done'),nl.

bench(Low,Up) :-
    Low =< Up,
    test_e(fib_x(Low),Res),!,
    print(fib(Low)), print(' == '), print(Res),nl,
    L1 is Low+1,
%    print('debug'),
    bench(L1,Up).

%:- bench(12,22).

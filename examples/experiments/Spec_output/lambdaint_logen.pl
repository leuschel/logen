test :- statistics(runtime,_), bench__0(5,24), statistics(runtime,[_,T2]),
   nl, print(T2), print(' ms'),nl. % was 5,28
   
/* bench(A,B) :- bench__0(A,B). */
bench__0(A,B) :-
        A>B,
        print('Done'),
        nl.
bench__0(A,B) :-
        A=<B,
        eval__1(constr(A,[]),constr(C,_)), !,
        print(fib(A)),
        print(' == '),
        print(C),
        nl,
        D is A+1,
        bench__0(D,B).

/* eval(if(eq(var(x),cst(0)),cst(1),if(eq(var(x),cst(1)),cst(1),plus(apply(minus(var(x),cst(1)),fun(fib)),apply(minus(var(x),cst(2)),fun(fib))))),[x/B],A) :- eval__1(B,A). */
eval__1(constr(0,[]),constr(1,[])) :- !.
eval__1(constr(1,[]),constr(1,[])) :- !.
eval__1(constr(A,[]),constr(B,[])) :-
        C is A-1,
        eval__1(constr(C,[]),constr(D,[])),
        E is A-2,
        eval__1(constr(E,[]),constr(F,[])),
        B is D+F.
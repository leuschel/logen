test :- statistics(runtime,_), bench__0(5,24), statistics(runtime,[_,T2]),
   nl, print(T2), print(' ms'),nl. % was 5,28

/* bench(A,B) :- bench__0(A,B). */
bench__0(A,B) :-
        A>B,
        print('Done'),
        nl.
bench__0(A,B) :-
        A=<B,
        eval__1(A,C,_), !,
        print(fib(A)),
        print(' == '),
        print(C),
        nl,
        D is A+1,
        bench__0(D,B).

/* eval(if(eq(var(x),cst(0)),cst(1),if(eq(var(x),cst(1)),cst(1),plus(apply(minus(var(x),cst(1)),fun(fib)),apply(minus(var(x),cst(2)),fun(fib))))),[x/constr(A,[])],constr(B,C)) :- eval__1(A,B,C). */
eval__1(0,1,[]) :- !.
eval__1(1,1,[]) :- !.
eval__1(A,B,[]) :-
        C is A-1,
        eval__1(C,D,[]),
        E is A-2,
        eval__1(E,F,[]),
        B is D+F.
/* Specialisation time 3.22299999969916 ms (runtime) */
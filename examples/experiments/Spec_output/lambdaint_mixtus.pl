test :- statistics(runtime,_), bench(5,24), statistics(runtime,[_,T2]),
   nl, print(T2), print(' ms'),nl. % was 5,28

bench(A, B) :-
        bench1(A, B).

% bench1(A,B):-bench(A,B)
bench1(A, B) :-
        B<A,
        print('Done'),
        nl.
bench1(A, B) :-
        A=<B,
        (   A=0, !,
            print(fib(0)),
            print(' == '),
            print(1),
            nl,
            bench1(1, B)
        ;   A=1, !,
            print(fib(1)),
            print(' == '),
            print(1),
            nl,
            bench1(2, B)
        ;   evalapplyminusvarx1(A, C),
            D is A-2,
            evalifeqvarx3(D, E),
            F is C+E, !,
            print(fib(A)),
            print(' == '),
            print(F),
            nl,
            G is A+1,
            bench1(G, B)
        ).

% evalapplyminusvarx1(A,B):-eval(apply(minus(var(x),cst(1)),fun(fib)),[x/constr(A,[])],constr(B,[]))
evalapplyminusvarx1(A, B) :-
        C is A-1,
        evalifeqvarx3(C, B).

% evalifeqvarx3(A,B):-eval(if(eq(var(x),cst(0)),cst(1),if(eq(var(x),cst(1)),cst(1),plus(apply(minus(var(x),cst(1)),fun(fib)),apply(minus(var(x),cst(2)),fun(fib))))),[x/constr(A,[])],constr(B,[]))
evalifeqvarx3(0, A) :- !,
        A=1.
evalifeqvarx3(1, A) :- !,
        A=1.
evalifeqvarx3(A, B) :-
        evalapplyminusvarx1(A, C),
        D is A-2,
        evalifeqvarx3(D, E),
        B is C+E.
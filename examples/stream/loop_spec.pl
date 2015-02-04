:-use_module(prolog_reader).

/* run_loop(loop(A,B)) :- run_loop__0(B,A). */
main(_) :- run_loop__0(A,25), print(res(A)),nl.
run_loop__0(B,A) :-
        run_file_all__1(B,A).

/* run_file_all(loop.pl,loop(A,B)) :- run_file_all__1(B,A). */
run_file_all__1(B,A) :-
        solve_atom__2(_,_,B,A,invalid,invalid).
run_file_all__1(B,A) :-
        solve_atom__2(_,_,B,A,invalid,invalid).

/* solve_atom(loop(A,B),[table(func,2,[D],C)],[table(func,2,[F],E)]) :- solve_atom__2(E,F,B,A,D,C). */
solve_atom__2(A,B,0,0,B,A).
solve_atom__2(E,F,B,A,D,C) :-
        A>0,
        G is A//10,
        (   G==D ->
            H=C,
            I=D,
            H=C
        ;   solve_atom__3(H,G),
            G=I
        ),
        J is A-1,
        solve_atom__2(E,F,K,J,I,H),
        B is H+A+K.

/* solve_atom(func(A,B),[],[]) :- solve_atom__3(B,A). */
solve_atom__3(B,A) :-
        B is A*A.

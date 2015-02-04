:-use_module(prolog_reader).

/* run_auto(A) :- run_auto__0(A). */
main(A) :-
        run_file_all__1(A).

/* run_file_all(autoplay_nocirc.pl,main(A)) :- run_file_all__1(A). */
run_file_all__1(A) :-
        solve_atom__2(_,_,_,_,A,invalid,_,_,invalid).

/* solve_atom(main(A),[table(compute_angle_index,4,[C,D,E],B)],[table(compute_angle_index,4,[G,H,I],F)]) :- solve_atom__2(F,I,H,G,A,C,D,E,B). */
solve_atom__2(E,H,G,F,_,B,C,D,A) :-
        solve_atom__3(I,J,K,L,M,B,C,D,A),
        solve_atom__4(E,H,G,F,M,L,K,J,I).

/* solve_atom(sample_file(A),[table(compute_angle_index,4,[C,D,E],B)],[table(compute_angle_index,4,[G,H,I],F)]) :- solve_atom__3(F,I,H,G,A,C,D,E,B). */
solve_atom__3(A,D,C,B,'sample.raw',B,C,D,A).

/* solve_atom(doit(A),[table(compute_angle_index,4,[C,D,E],B)],[table(compute_angle_index,4,[G,H,I],F)]) :- solve_atom__4(F,I,H,G,A,C,D,E,B). */
solve_atom__4(F,I,H,G,A,C,D,E,B) :-
        solve_atom__5(J,K,L,M,N,A,C,D,E,B),
        statistics(runtime,[_,_]),
        solve_atom__6(F,I,H,G,N,M,L,K,J),
        fail.
solve_atom__4(A,D,C,B,_,B,C,D,A) :-
        statistics(runtime,[_,E]),
        display(user_error,'Sample file processed in '),
        display(user_error,E),
        display(user_error,' ms.'),
        nl(user_error).

/* solve_atom(get_sound_samples(A,B),[table(compute_angle_index,4,[D,E,F],C)],[table(compute_angle_index,4,[H,I,J],G)]) :- solve_atom__5(G,J,I,H,B,A,D,E,F,C). */
solve_atom__5(G,J,I,H,B,A,D,E,F,C) :-
        open(A,read,K),
        solve_atom__7(L,M,N,O,P,K,D,E,F,C),
        solve_atom__8(G,J,I,H,[],B,K,P,O,N,M,L).

/* solve_atom(play_stream(A),[table(compute_angle_index,4,[C,D,E],B)],[table(compute_angle_index,4,[G,H,I],F)]) :- solve_atom__6(F,I,H,G,A,C,D,E,B). */
solve_atom__6(A,D,C,B,[],B,C,D,A) :- !.
solve_atom__6(A,D,C,B,[_],B,C,D,A) :- !.
solve_atom__6(F,I,H,G,A,C,D,E,B) :- !,
        solve_atom__9(J,K,L,M,N,A,C,D,E,B),
        solve_atom__10(O,P,Q,R,_,_,N,20,2.199999999794659,1.099999999872068,M,L,K,J),
        solve_atom__6(F,I,H,G,N,R,Q,P,O).

/* solve_atom(get_sample(A,B),[table(compute_angle_index,4,[D,E,F],C)],[table(compute_angle_index,4,[H,I,J],G)]) :- solve_atom__7(G,J,I,H,B,A,D,E,F,C). */
solve_atom__7(C,F,E,D,B,A,D,E,F,C) :-
        get_byte(A,B).

/* solve_atom(read_bytes(A,B,C,D),[table(compute_angle_index,4,[F,G,H],E)],[table(compute_angle_index,4,[J,K,L],I)]) :- solve_atom__8(I,L,K,J,D,C,B,A,F,G,H,E). */
solve_atom__8(B,E,D,C,A,A,_,-1,C,D,E,B) :- !.
solve_atom__8(H,K,J,I,C,[A|L],B,A,E,F,G,D) :-
        solve_atom__7(M,N,O,P,Q,B,E,F,G,D),
        solve_atom__8(H,K,J,I,C,L,B,Q,P,O,N,M).

/* solve_atom(shift_soundstream(A,B),[table(compute_angle_index,4,[D,E,F],C)],[table(compute_angle_index,4,[H,I,J],G)]) :- solve_atom__9(G,J,I,H,B,A,D,E,F,C). */
solve_atom__9(B,E,D,C,A,[_|A],C,D,E,B).

/* solve_atom(spatial(A,B,C,D,E,F),[table(compute_angle_index,4,[H,I,J],G)],[table(compute_angle_index,4,[L,M,N],K)]) :- solve_atom__10(K,N,M,L,F,E,D,C,B,A,H,I,J,G). */
solve_atom__10(K,N,M,L,F,E,D,C,B,A,H,I,J,G) :-
        solve_atom__11(O,P,Q,R,S,B,A,H,I,J,G),
        D=[T|_],
        (   A==R,
            B==Q,
            C==P ->
            (U,V)=O,
            W=R,
            X=Q,
            Y=P,
            (U,V)=O
        ;   solve_atom__12((U,V),C,B,A),
            A=W,
            B=X,
            C=Y
        ),
        solve_atom__13(K,N,M,L,Z,V,D,W,X,Y,(U,V)),
        (   U<0 ->
            E is T/S,
            F is Z/S
        ;   E is Z/S,
            F is Z/S
        ).

/* solve_atom(att(A,B,C),[table(compute_angle_index,4,[E,F,G],D)],[table(compute_angle_index,4,[I,J,K],H)]) :- solve_atom__11(H,K,J,I,C,B,A,E,F,G,D). */
solve_atom__11(D,G,F,E,C,B,A,E,F,G,D) :-
        H is A*A+B*B,
        (   H<1 ->
            C=1
        ;   C=H
        ).

/* solve_atom(compute_angle_index(A,B,C,D),[],[]) :- solve_atom__12(D,C,B,A). */
solve_atom__12((D,E),C,B,A) :-
        solve_atom__14(D,C,B,A),
        solve_atom__15(E,D).

/* solve_atom(peek_stream(A,B,C),[table(compute_angle_index,4,[E,F,G],D)],[table(compute_angle_index,4,[I,J,K],H)]) :- solve_atom__13(H,K,J,I,C,B,A,E,F,G,D). */
solve_atom__13(A,D,C,B,0,_,[],B,C,D,A).
solve_atom__13(C,F,E,D,B,A,[G|H],D,E,F,C) :-
        (   A=<1 ->
            B=G
        ;   I is A-1,
            solve_atom__13(C,F,E,D,B,I,H,D,E,F,C)
        ).

/* solve_atom(angle(A,B,C,D),[],[]) :- solve_atom__14(D,C,B,A). */
solve_atom__14(D,C,B,A) :-
        solve_atom__16(E,B,A),
        D is(E+C)mod 360-180.

/* solve_atom(index(A,B),[],[]) :- solve_atom__15(B,A). */
solve_atom__15(B,A) :-
        B is abs(round(A/29.99999999775268)).

/* solve_atom(theta(A,B,C),[],[]) :- solve_atom__16(C,B,A). */
solve_atom__16(C,B,A) :-
        solve_atom__17(D,A,B),
        C is round(54.99999999587992*D).

/* solve_atom(atan2(A,B,C),[],[]) :- solve_atom__17(C,B,A). */
solve_atom__17(C,B,A) :-
        (   B=0 ->
            (   A>0 ->
                C is 3.139999999706922/2
            ;   C is-3.139999999706922/2
            )
        ;   C is atan(A/B)
        ).

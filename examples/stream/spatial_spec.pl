:-use_module(prolog_reader).

/* run_stream(A) :- run_stream__0(A). */
main(A) :-
        run_file_all__1(main(A)).

/* run_file_all(spatial_stream.pl,A) :- run_file_all__1(A). */
run_file_all__1(main(A)) :-
        solve_atom__2(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(loop(A)) :-
        solve_atom__3(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(loop(A)) :-
        solve_atom__3(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(loop(A)) :-
        solve_atom__3(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(soundstream(A)) :-
        solve_atom__4(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(shift_soundstream(A,B)) :-
        solve_atom__5(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(peek_stream(A,B,C)) :-
        solve_atom__6(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(peek_stream(A,B,C)) :-
        solve_atom__6(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(spatial(A,B,C,D,E,F)) :-
        solve_atom__7(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],F,E,D,C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(table_function(A)) :-
        solve_atom__8(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(table_function(A)) :-
        solve_atom__8(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(table_function(A)) :-
        solve_atom__8(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(table_function(A)) :-
        solve_atom__8(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(index(A,B)) :-
        solve_atom__9(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(dist(A,B,C)) :-
        solve_atom__10(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(att(A,B)) :-
        solve_atom__11(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(angle(A,B,C,D)) :-
        solve_atom__12(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],D,C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(theta(A,B,C)) :-
        solve_atom__13(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).
run_file_all__1(atan2(A,B,C)) :-
        solve_atom__14(_,_,_,_,invalid,[invalid|_],invalid,[invalid|_],C,B,A,[invalid|_],invalid,[invalid|_],invalid,_,_,_,_).

/* solve_atom(main(A),[table(index,2,B,C),table(dist,3,D,E),table(angle,4,F,G),table(theta,3,H,I)],[table(index,2,J,K),table(dist,3,L,M),table(angle,4,N,O),table(theta,3,P,Q)]) :- solve_atom__2(O,N,K,J,I,H,E,D,A,B,C,F,G,L,M,P,Q). */
solve_atom__2(N,M,J,I,H,G,D,C,_,A,B,E,F,K,L,O,P) :-
        print('% START'),
        nl,
        solve_atom__4(Q,R,S,T,H,G,D,C,U,A,B,E,F,V,W,X,Y),
        solve_atom__3(N,M,J,I,Y,X,W,V,U,T,S,R,Q,K,L,O,P),
        print('% END'),
        nl.

/* solve_atom(loop(A),[table(index,2,B,C),table(dist,3,D,E),table(angle,4,F,G),table(theta,3,H,I)],[table(index,2,J,K),table(dist,3,L,M),table(angle,4,N,O),table(theta,3,P,Q)]) :- solve_atom__3(O,N,K,J,I,H,E,D,A,B,C,F,G,L,M,P,Q). */
solve_atom__3(F,E,B,A,H,G,D,C,[],A,B,E,F,C,D,G,H) :- !.
solve_atom__3(F,E,B,A,H,G,D,C,[_],A,B,E,F,C,D,G,H) :- !.
solve_atom__3(O,N,K,J,I,H,E,D,A,B,C,F,G,L,M,P,Q) :-
        solve_atom__5(R,S,T,U,I,H,E,D,V,A,B,C,F,G,W,X,Y,Z),
        solve_atom__7(A1,B1,C1,D1,Z,Y,X,W,E1,F1,V,20,2.199999999794659,1.099999999872068,U,T,S,R,G1,H1,I1,J1),
        print(out(F1,E1)),
        nl,
        solve_atom__3(O,N,K,J,J1,I1,H1,G1,V,D1,C1,B1,A1,L,M,P,Q).

/* solve_atom(soundstream(A),[table(index,2,B,C),table(dist,3,D,E),table(angle,4,F,G),table(theta,3,H,I)],[table(index,2,J,K),table(dist,3,L,M),table(angle,4,N,O),table(theta,3,P,Q)]) :- solve_atom__4(O,N,K,J,I,H,E,D,A,B,C,F,G,L,M,P,Q). */
solve_atom__4(F,E,B,A,H,G,D,C,[0,0.0999999999941802,0.1999999999767397,0.3999999999534794,0.7999999999069587,1,0.6999999999185888,0.3999999999534794,0.1999999999767397,0.2999999999651095],A,B,E,F,C,D,G,H).

/* solve_atom(shift_soundstream(A,B),[table(index,2,C,D),table(dist,3,E,F),table(angle,4,G,H),table(theta,3,I,J)],[table(index,2,K,L),table(dist,3,M,N),table(angle,4,O,P),table(theta,3,Q,R)]) :- solve_atom__5(P,O,L,K,J,I,F,E,B,A,C,D,G,H,M,N,Q,R). */
solve_atom__5(G,F,C,B,I,H,E,D,A,[_|A],B,C,F,G,D,E,H,I).

/* solve_atom(peek_stream(A,B,C),[table(index,2,D,E),table(dist,3,F,G),table(angle,4,H,I),table(theta,3,J,K)],[table(index,2,L,M),table(dist,3,N,O),table(angle,4,P,Q),table(theta,3,R,S)]) :- solve_atom__6(Q,P,M,L,K,J,G,F,C,B,A,D,E,H,I,N,O,R,S). */
solve_atom__6(F,E,B,A,H,G,D,C,0,_,[],A,B,E,F,C,D,G,H).
solve_atom__6(H,G,D,C,J,I,F,E,B,A,[K|L],C,D,G,H,E,F,I,J) :-
        (   A=<1 ->
            B=K
        ;   M is A-1,
            solve_atom__6(H,G,D,C,J,I,F,E,B,M,L,C,D,G,H,E,F,I,J)
        ).

/* solve_atom(spatial(A,B,C,D,E,F),[table(index,2,G,H),table(dist,3,I,J),table(angle,4,K,L),table(theta,3,M,N)],[table(index,2,O,P),table(dist,3,Q,R),table(angle,4,S,T),table(theta,3,U,V)]) :- solve_atom__7(T,S,P,O,N,M,J,I,F,E,D,C,B,A,G,H,K,L,Q,R,U,V). */
solve_atom__7(S,R,O,N,M,L,I,[V,W|X],F,E,D,C,B,A,G,H,J,K,P,Q,T,U) :-
        (   A==V,
            B==W ->
            print(used_cache(I)),
            nl,
            dist(A,B,Y)=I,
            Z=[V,W|X],
            A1=I
        ;   print(recomputing(dist(A,B,Y))),
            nl,
            solve_atom__15(Y,B,A),
            Z=[A,B,Y],
            A1=dist(A,B,Y)
        ),
        solve_atom__11(B1,[I1,J1,K1|L1],C1,[M1|N1],M,L,A1,Z,D1,Y,G,H,J,K,E1,F1,G1,H1),
        (   A==I1,
            B==J1,
            C==K1 ->
            print(used_cache(B1)),
            nl,
            angle(A,B,C,O1)=B1,
            P1=[I1,J1,K1|L1],
            Q1=B1
        ;   print(recomputing(angle(A,B,C,O1))),
            nl,
            solve_atom__16(O1,C,B,A),
            P1=[A,B,C,O1],
            Q1=angle(A,B,C,O1)
        ),
        D=[R1|_],
        (   O1==M1 ->
            print(used_cache(C1)),
            nl,
            index(O1,S1)=C1,
            T1=[M1|N1],
            U1=C1
        ;   print(recomputing(index(O1,S1))),
            nl,
            solve_atom__17(S1,O1),
            T1=[O1,S1],
            U1=index(O1,S1)
        ),
        solve_atom__6(S,R,O,N,H1,G1,F1,E1,V1,S1,D,T1,U1,P1,Q1,P,Q,T,U),
        (   O1<0 ->
            E is R1/D1,
            F is V1/D1
        ;   E is V1/D1,
            F is V1/D1
        ).

/* solve_atom(table_function(A),[table(index,2,B,C),table(dist,3,D,E),table(angle,4,F,G),table(theta,3,H,I)],[table(index,2,J,K),table(dist,3,L,M),table(angle,4,N,O),table(theta,3,P,Q)]) :- solve_atom__8(O,N,K,J,I,H,E,D,A,B,C,F,G,L,M,P,Q). */
solve_atom__8(F,E,B,A,H,G,D,C,index/2,A,B,E,F,C,D,G,H).
solve_atom__8(F,E,B,A,H,G,D,C,dist/3,A,B,E,F,C,D,G,H).
solve_atom__8(F,E,B,A,H,G,D,C,angle/4,A,B,E,F,C,D,G,H).
solve_atom__8(F,E,B,A,H,G,D,C,theta/3,A,B,E,F,C,D,G,H).

/* solve_atom(index(A,B),[table(index,2,C,D),table(dist,3,E,F),table(angle,4,G,H),table(theta,3,I,J)],[table(index,2,K,L),table(dist,3,M,N),table(angle,4,O,P),table(theta,3,Q,R)]) :- solve_atom__9(P,O,L,K,J,I,F,E,B,A,C,D,G,H,M,N,Q,R). */
solve_atom__9(H,G,D,C,J,I,F,E,B,A,C,D,G,H,E,F,I,J) :-
        B is abs(round(A/29.99999999775268)).

/* solve_atom(dist(A,B,C),[table(index,2,D,E),table(dist,3,F,G),table(angle,4,H,I),table(theta,3,J,K)],[table(index,2,L,M),table(dist,3,N,O),table(angle,4,P,Q),table(theta,3,R,S)]) :- solve_atom__10(Q,P,M,L,K,J,G,F,C,B,A,D,E,H,I,N,O,R,S). */
solve_atom__10(I,H,E,D,K,J,G,F,C,B,A,D,E,H,I,F,G,J,K) :-
        C is A*A+B*B.

/* solve_atom(att(A,B),[table(index,2,C,D),table(dist,3,E,F),table(angle,4,G,H),table(theta,3,I,J)],[table(index,2,K,L),table(dist,3,M,N),table(angle,4,O,P),table(theta,3,Q,R)]) :- solve_atom__11(P,O,L,K,J,I,F,E,B,A,C,D,G,H,M,N,Q,R). */
solve_atom__11(H,G,D,C,J,I,F,E,B,A,C,D,G,H,E,F,I,J) :-
        (   A<1 ->
            B=1
        ;   B=A
        ).

/* solve_atom(angle(A,B,C,D),[table(index,2,E,F),table(dist,3,G,H),table(angle,4,I,J),table(theta,3,K,L)],[table(index,2,M,N),table(dist,3,O,P),table(angle,4,Q,R),table(theta,3,S,T)]) :- solve_atom__12(R,Q,N,M,L,K,H,G,D,C,B,A,E,F,I,J,O,P,S,T). */
solve_atom__12(J,I,F,E,K,[N,O|P],H,G,D,C,B,A,E,F,I,J,G,H,L,M) :-
        (   A==N,
            B==O ->
            print(used_cache(K)),
            nl,
            theta(A,B,Q)=K,
            L=[N,O|P],
            M=K
        ;   print(recomputing(theta(A,B,Q))),
            nl,
            solve_atom__18(Q,B,A),
            L=[A,B,Q],
            M=theta(A,B,Q)
        ),
        D is(Q+C)mod 360-180.

/* solve_atom(theta(A,B,C),[table(index,2,D,E),table(dist,3,F,G),table(angle,4,H,I),table(theta,3,J,K)],[table(index,2,L,M),table(dist,3,N,O),table(angle,4,P,Q),table(theta,3,R,S)]) :- solve_atom__13(Q,P,M,L,K,J,G,F,C,B,A,D,E,H,I,N,O,R,S). */
solve_atom__13(Q,P,M,L,K,J,G,F,C,B,A,D,E,H,I,N,O,R,S) :-
        solve_atom__14(Q,P,M,L,K,J,G,F,T,A,B,D,E,H,I,N,O,R,S),
        C is round(54.99999999587992*T).

/* solve_atom(atan2(A,B,C),[table(index,2,D,E),table(dist,3,F,G),table(angle,4,H,I),table(theta,3,J,K)],[table(index,2,L,M),table(dist,3,N,O),table(angle,4,P,Q),table(theta,3,R,S)]) :- solve_atom__14(Q,P,M,L,K,J,G,F,C,B,A,D,E,H,I,N,O,R,S). */
solve_atom__14(I,H,E,D,K,J,G,F,C,B,A,D,E,H,I,F,G,J,K) :-
        (   B=0 ->
            (   A>0 ->
                C is pi/2
            ;   C is-pi/2
            )
        ;   C is atan(A/B)
        ).

/* solve_atom(dist(A,B,C),[],[]) :- solve_atom__15(C,B,A). */
solve_atom__15(C,B,A) :-
        C is A*A+B*B.

/* solve_atom(angle(A,B,C,D),[],[]) :- solve_atom__16(D,C,B,A). */
solve_atom__16(D,C,B,A) :-
        solve_atom__18(E,B,A),
        D is(E+C)mod 360-180.

/* solve_atom(index(A,B),[],[]) :- solve_atom__17(B,A). */
solve_atom__17(B,A) :-
        B is abs(round(A/29.99999999775268)).

/* solve_atom(theta(A,B,C),[],[]) :- solve_atom__18(C,B,A). */
solve_atom__18(C,B,A) :-
        solve_atom__19(D,A,B),
        C is round(54.99999999587992*D).

/* solve_atom(atan2(A,B,C),[],[]) :- solve_atom__19(C,B,A). */
solve_atom__19(C,B,A) :-
        (   B=0 ->
            (   A>0 ->
                C is pi/2
            ;   C is-pi/2
            )
        ;   C is atan(A/B)
        ).

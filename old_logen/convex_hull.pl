% (c) Florence Benoy, Andy King and Fred Mesnard
%
% See the TPLP paper "Calculating Convex Hulls with
% a Linear Solver" for explanation and discussion
%

/* example call:
  | ?- hull:convex_hull([X1,Y1],[X1=0,Y1=1],[X2,Y2],[X2>=0,Y2=X2],V,S).
S = [_A>=0,_A-_B>= -1,_A-_B=<0],
V = [_A,_B] ? 

*/


:- module(convex_hull, [project/3, convex_hull/6, tell_cs/1]).
:- use_module(library(clpq)).

project(Xs, Cxs, ProjectCxs) :-
    call_residue(copy_term(Xs-Cxs, CpyXs-CpyCxs), _),
    tell_cs(CpyCxs),
    prepare_dump(CpyXs, Xs, Zs, DumpCxs, ProjectCxs),
    dump(Zs, Vs, DumpCxs), Xs = Vs.

tell_cs([]).
tell_cs([C|Cs]) :-  {C}, tell_cs(Cs).

prepare_dump([], [], [], Cs, Cs).
prepare_dump([X|Xs], YsIn, ZsOut, CsIn, CsOut) :-
    (ground(X) ->
        YsIn  = [Y|Ys],
        ZsOut = [_|Zs],
        CsOut = [Y=X|Cs]
    ;
        YsIn  = [_|Ys],
        ZsOut = [X|Zs],
        CsOut = Cs
    ),
    prepare_dump(Xs, Ys, Zs, CsIn, Cs).

convex_hull(Xs, Cxs, Ys, Cys, Zs, Czs) :-
    scale(Cxs, Sig1, [], C1s),
    scale(Cys, Sig2, C1s, C2s),
    add_vect(Xs, Ys, Zs, C2s, C3s),
    project(Zs, [Sig1 >= 0, Sig2 >= 0, Sig1+Sig2 = 1|C3s], Czs).

scale([], _, Cs, Cs).
scale([C1|C1s], Sig, C2s, C3s) :-
    C1 =.. [RelOp, A1, B1],
    C2 =.. [RelOp, A2, B2],
    mul_exp(A1, Sig, A2),
    mul_exp(B1, Sig, B2),
    scale(C1s, Sig, [C2|C2s], C3s).

mul_exp(E1, Sigma, E2) :- once(mulexp(E1, Sigma, E2)).

mulexp(  X,   _,     X) :- var(X).
mulexp(N*X,   _,   N*X) :- ground(N), var(X).
mulexp( -X, Sig,    -Y) :- mulexp(X, Sig, Y).
mulexp(A+B, Sig,   C+D) :- mulexp(A, Sig, C), mulexp(B, Sig, D).
mulexp(A-B, Sig,   C-D) :- mulexp(A, Sig, C), mulexp(B, Sig, D).
mulexp(  N, Sig, N*Sig) :- ground(N).

add_vect([], [], [], Cs, Cs).
add_vect([U|Us], [V|Vs], [W|Ws], C1s, C2s) :-
    add_vect(Us, Vs, Ws, [W = U+V|C1s], C2s).
    
    
    
    
    
    
 


member( X, [X|Xs]).
member( X, [_|Xs]):- member(X,Xs).

iota(N,List):-
   iota1(0,N,List).  
iota1(K,K,[]).
iota1(K,N,[K|List]):- K1 is K+1, iota1(K1,N,List).

dif( [],_,_,[],[]).
dif( [S|Ss],Val,Mod,[D|Ds],[D2|D2s]):-
     D is Val - S, D2 is Mod - D,
     dif( Ss,Val,Mod,Ds,D2s).
    

rev( [],L,L).
rev( [X|Xs], Y,L):-
   rev( Xs,[X|Y],L).


mergedelete([],L,L).
mergedelete([D|Ds], [D|R], L2):-
        mergedelete( Ds, R, L2).
mergedelete( [D|Ds], [X|R], [X|L2]):-
        D > X,
        mergedelete( [D|Ds],R,L2).
       
check( [],_,L,L,_).
check( S, Choice, Old, L3,Modulus):-
      S = [_|_],
      dif( S, Choice,Modulus, Ds,Dds),
      mergedelete( Ds, Old,L2),
      rev( Dds,[],Rds),
      mergedelete(Rds, L2,L3).


pds1( [],_,[],_).
pds1( Unused, List, [Choice|Rest],Mod):-
     member(Choice,Unused),
     check( List, Choice, Unused,U3,Mod),
     pds1( U3, [Choice| List], Rest, Mod).
    
pds( Order, [0|Ans]):-
    N is Order * (Order + 1) + 1,
    iota( N, [0|List]),
    pds1( List, [0], Ans, N).

pdsbm(N,X):- pds(N, [0,1|X]).


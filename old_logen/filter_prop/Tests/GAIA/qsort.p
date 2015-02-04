qsort(S,Sorted) :-
   qsort(S,Sorted,[]).

partition([],F,[],[]).
partition([F|T],P,[F|S],B) :-
   F =< P,
   partition(T,P,S,B).
partition([F|T],P,S,[F|B]) :-
   F > P,
   partition(T,P,S,B).

qsort([],X,X).
qsort([F|T],Res,Tail) :-
   partition(T,F,S,B),
   qsort(S,Res,[F | Rest]),
   qsort(B,Rest,Tail).  

%split(T,F,T,T).

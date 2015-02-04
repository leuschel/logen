
main(V1,V2) :-
   init(100,10,4,
            V1,
      Symbols),


   randomize(Symbols,RSymbols,21),
   investigate(RSymbols,V2).


init(N,M,Npats,Ipats,Result) :- init(N,M,M,Npats,Ipats,Result).

init(0,_,_,_,_,_).
init(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
   fill(I,[],L),
   get_pats(Npats,Ipats,Ppats),
   J is M - I,
   fill(J,[pattern(Ppats)|L],Symb),
   N1 is N - 1,
        test(I,I1,M),
   init(N1,I1,M,Npats,Ipats,Rest).

test(I,I1,M) :-
  I = 0,
  I1 is M.
test(I,I1,M) :-
   I1 is I - 1.

fill(0,L,L).
fill(N,L,[dummy([])|Rest]) :- N1 is N - 1, fill(N1,L,Rest).


randomize([],[],_).
randomize(In,[X|Out],Rand) :-
   g_length(In,Lin),
   Rand1 is Rand * 17,
   N is Rand1,
   split(N,In,X,In1),
   randomize(In1,Out,Rand1).

split(0,[X|Xs],X,Xs).
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
   N1 is N - 1,
   split(N1,Xs,RemovedElt,Ys).


investigate([],_).
investigate([U|Units],Patterns) :-
   property(U,pattern,Data),
   p_investigate(Data,Patterns),
   investigate(Units,Patterns).


get_pats(Npats,Ipats,Result) :- get_pats(Npats,Ipats,Result,Ipats).

get_pats(0,_,[],_).
get_pats(N,[X|Xs],[X|Ys],Ipats) :-
   N1 is N - 1,
   get_pats(N1,Xs,Ys,Ipats).
get_pats(N,[],Ys,Ipats) :-
   get_pats(N,Ipats,Ys,Ipats).

property([Prop|RProps],P,Val) :-
   my_functor(Prop,P,_),
   my_arg(1,Prop,Val).
property([_|RProps],P,Val) :-
   property(RProps,P,Val).

my_functor(f(a,b),f,[a,b]).
my_functor([a|X],f,[a,X]).
my_functor(start(X),star,[X]).
my_functor(pattern(X),pattern,[X]).

my_arg(1,f(X,Y),X).
my_arg(1,[X|Y],X).
my_arg(1,g(X,Y,Z),X).
my_arg(1,pattern(X),X).

p_investigate([],_).
p_investigate([D|Data],Patterns) :-
   p_match(Patterns,D),
   p_investigate(Data,Patterns).

p_match([],_).
p_match([P|Patterns],D) :-
   match(D,P), 
        a = b.
p_match([P|Patterns],D) :-
   p_match(Patterns, D).

match([],[]).
match([X|PRest],[Y|SRest]) :-
   X = Y,
   match(PRest,SRest).
match(List,[Y|Rest]) :- 
   Y = star(X),
   g_concat(X,SRest,List),
   match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
        my_atom(X),
   X = Y,
   match(PRest,SRest).
match([X|PRest],[Y|SRest]) :-
        match(X,Y),
   match(PRest,SRest).

my_atom(a).
my_atom(b).

g_concat([],L,L).
g_concat([X|L1],L2,[X|L3]) :- g_concat(L1,L2,L3).

g_length(List,L) :- g_length(List,0,L).
g_length([],L,L).
g_length([X|Xs],N,L) :- N1 is N + 1, g_length(Xs,N1,L).

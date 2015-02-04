play(Game,Result) :-
   initialize(Game,Position,Player),
   displaygame(Position,Player),
   play(Position,Player,Result).

play(Position,Player,Result) :-
  gameover(Position,Player,Result),
  announce(Result).
play(Position,Player,Result) :-
  choosemove(Position,Player,Move),
  move(Move,Position,Position1),
  displaygame(Position1,Player),
  nextplayer(Player,Player1),
  play(Position1,Player1,Result).

choosemove(Position,computer,Move) :-
   lookahead(Depth),
   alphabeta(Depth,Position,40,40,Move,Value).
choosemove(Position,opponent,Move) :-
   genlegal(Move).

alphabeta(0,Position,Alpha,Beta,Move,Value) :-
   value(Position,Value).
alphabeta(D,Position,Alpha,Beta,Move,Value) :-
  D > 0,
  allmoves(Position,Moves),
  Alpha1 is 0 - Beta,
  Beta1 is 0 - Alpha,
  D1 is D - 1,
  evaluateandchoose(Moves,Position,D1,Alpha1,Beta1,nil,p(Move,Value)).

allmoves(Position,[X]) :-
  move(Position,X).
allmoves(Position,[X|Xs]) :-
  move(Position,X),
  allmoves(Position,Xs).



evaluateandchoose([Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
  move(Move,Position,Position1),
  alphabeta(D,Position1,Alpha,Beta,MoveX,Value),
  Value1 is 0 - Value,
  cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluateandchoose([],Position,D,Alpha,Beta,Move,p(Move,Alpha)).

cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,p(Move,Value)) :-
  Value >= Beta.
cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-   
  Alpha < Value,
  Value < Beta,
  evaluateandchoose(Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-   
  Value =< Alpha, 
  evaluateandchoose(Moves,Position,D,Alpha,Beta,Move1,BestMove).

move(Board,[M|Ms]) :-
   member(M,[1,2,3,4,5,6]),
   stonesinhole(M,Board,N),
   extendmove(N,M,Board,Ms).
move(board([0,0,0,0,0,0],K,Ys,L),[]).

member(X,[X|Y]).
member(X,[F|T]) :-
   member(X,T).

stonesinhole(M,board(Hs,K,Ys,L),Stones) :-
  nthmember(M,Hs,Stones),
  Stones > 0.

extendmove(Stones,M,Board,[]) :-
  Stones \== 7 - M.
extendmove(Stones,M,Board,Ms) :-
  Stones =:= 7 - M,
  distributestones(Stones,M,Board,Board1),
  move(Board1,Ms).

move([N|Ns],Board,FinalBoard) :-
  stonesinhole(N,Board,Stones),
  distributestones(Stones,N,Board,Board1),
  move(Ns,Board1,FinalBoard).
move([],Board1,Board2) :-
  swap(Board1,Board2).

distributestones(Stones,Hole,Board,FinalBoard) :-
  distributemyholes(Stones,Hole,Board,Board1,Stones1),
  distributeyourholes(Stones1,Board1,FinalBoard).

distributemyholes(Stones,N,board(Hs,K,Ys,L),board(Hs1,K1,Ys,L),Stones1) :-
  Stones > 7 - N,
  pickupanddistribute(N,Stones,Hs,Hs1),
  K1 is K + 1,
  Stones1 is Stones + N - 7.
distributemyholes(Stones,N,board(Hs,K,Ys,L),Board,0) :-
  pickupanddistribute(N,Stones,Hs,Hs1),
  checkcapture(N,Stones,Hs1,Hs2,Ys,Ys1,Pieces),
  updatekalah(Pieces,N,Stones,K,K1),
  checkiffinished(board(Hs2,K1,Ys1,L),Board).

checkcapture(N,Stones,Hs,Hs1,Ys,Ys1,Pieces) :-
  FinishingHole is N + Stones,
  OppositeHole is 7 - FinishingHole,
  nthmember(OppositeHole,Ys,Y),
  Y > 0,
  nsubstitute(OppositeHole,Hs,0,Hs1),
  nsubstitute(FinishingHole,Ys,0,Ys1),
  Pieces is Y + 1.
checkcapture(N,Stones,Hs,Hs,Ys,Ys,0).

checkiffinished(board(Hs,K,Ys,L),board(Hs,K,Hs,L1))   :-
 zero(Hs),
 sumlist(Ys,YsSum),
 L1 is L + YsSum.
checkiffinished(board(Hs,K,Ys,L),board(Ys,K1,Ys,L))   :-
 zero(Ys),
 sumlist(Hs,HsSum),
 K1 is K + HsSum.
checkiffinished(Board,Board).

updatekalah(0,Stones,N,K,K) :-
  Stones < 7 - N.
updatekalah(0,Stones,N,K,K1) :-
  Stones =:= 7 - N,
  K1 is K + 1.
updatekalah(Pieces,Stones,N,K,K1) :-
  Pieces > 0,
  K1 is K + Pieces.

distributeyourholes(0,Board,Board).
distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
   1 =< Stones,
   Stones =< 6,
   nonzero(Hs),
   distribute(Stones,Ys,Ys1).
distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
   Stones > 6,
   distribute(6,Ys,Ys1),
   Stones1 is Stones - 6,
   distributestones(Stones1,1,board(Hs,K,Ys1,L),Board).
distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Hs,L1)) :-
   zero(Hs),
   sumlist(Ys,YsSum),
   L1 is Stones + YsSum + L.

pickupanddistribute(1,N,[H|Hs],[0|Hs1]) :-
   distribute(N,Hs,Hs1).
pickupanddistribute(K,N,[H|Hs],[0|Hs1]) :-
   K > 1,
   K1 is K - 1,
   pickupanddistribute(K1,N,Hs,Hs1).

distribute(0,Hs,Hs).
distribute(N,[H|Hs],[H1|Hs1]) :-
   N > 0,
   N1 is N - 1,
   H1 is H + 1,
   distribute(N1,Hs,Hs1).
distribute(N,[],[]).

value(board(H,K,Y,L),Value) :-
  Value is K - L.

gameover(board(0,N,0,N),Player,draw) :-
  pieces(K),
  N =:= 6 * K.
gameover(board(H,K,Y,L),Player,Player) :-
  pieces(N),
  K > 6 * N.
gameover(board(H,K,Y,L),Player,Opponent) :-
  pieces(N),
  L > 6 * N,
  nextplayer(Player,Opponent).

announce(opponent).
announce(computer).
announce(draw).

nthmember(N,[H|Hs],K) :-
  N > 1,
  N1 is N - 1,
  nthmember(N1,Hs,K).
nthmember(1,[H|Hs],H).

nsubstitute(1,[X|Xs],Y,[Y|Xs]).
nsubstitute(N,[X|Xs],Y,[X|Xs1]) :-
   N > 1,
   N1 is N -1,
   nsubstitute(N1,Xs,Y,Xs1).

nextplayer(computer,opponent).
nextplayer(opponent,computer).

legal([N|Ns]) :-
  0 < N,
  N < 7,
  legal(Ns).
legal([]).

genlegal([N|Ns]) :-
  member(N,[1,2,3,4,5,6]),
  genlegal(Ns).
genlegal([]).

swap(board(Hs,K,Ys,L),board(Ys,L,Hs,K)).

displaygame(Position,computer) :-
  show(Position).
displaygame(Position,opponent) :-
  swap(Position,Position1),
  show(Position1).

show(board(H,K,Y,L)) :-
   reverse(H,Hr),
   writestones(Hr),
   writekalahs(K,L),
   writestones(Y).

writestones(H) :-
  displayholes(H).

displayholes([H|Hs]) :-
   writepile(H),
   displayholes(Hs).
displayholes([]).

writepile(N) :-
  N < 10,
  k_write(N).
writepile(N) :-
  N >= 10,
  k_write(N).

k_write(X).

writekalahs(K,L) :-
  k_write(K),
  k_write(L).

zero([0,0,0,0,0,0]).
nonzero(Hs) :-
  Hs \== [0,0,0,0,0,0].

reverse(L,K):-
   rev(L,[],K).

rev([],L,L).
rev([H|T],L,K):-
   rev(T,[H|L],K).

sumlist(Is,Sum) :-
   sumlist(Is,0,Sum).
sumlist([],Sum,Sum).
sumlist([I|Is],Temp,Sum) :-
  Temp1 is Temp + 1,
  sumlist(Is,Temp1,Sum).

lookahead(2).
lookahead(5).


initialize(kalah,board(a,0,a,0),opponent).
initialize(kalah,toto(b,1,b,1),computer).
initialize(kalah,board(c,2,c,2),computer).
initialize(kalah,board(a,0,a,0),computer).
initialize(kalah,board(c,2,c,2),opponent).

pieces(6).
pieces(1).




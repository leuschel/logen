
main(_) :- print('% START'),nl,soundstream(S), loop(S), print('% END'),nl.

play_stream([]) :- !.
play_stream([_]) :- !.
play_stream(SoundStream) :- 
  shift_soundstream(SoundStream,NS),
  spatial(1.1,2.2, 20, NS, Left,Right),
  print(out(Left,Right)),nl,
  play_stream(NS).
  
soundstream([0, 0.1, 0.2, 0.4, 0.8, 1, 0.7, 0.4, 0.2, 0.3]). 
shift_soundstream([_|T],T).

peek_stream([],_N,0).
peek_stream([H|T],N,R) :-
  (N=<1 -> R = H ; (N1 is N-1, peek_stream(T,N1,R))).
  
spatial(X,Y,C,SoundStream,  LeftOut,RightOut) :-
  dist(X,Y,Dist),
  att(Dist,Att),
  angle(X,Y,C,Angle),
  SoundStream = [One|_],
  index(Angle,Index),
  peek_stream(SoundStream,Index,Other),
  (Angle<0
    -> (LeftOut is One/Att, RightOut is Other/Att)
    ;  (LeftOut is Other/Att, RightOut is Other/Att)
  ).
  


table_function(index/2).
table_function(dist/3).
table_function(angle/4).
table_function(theta/3).

index(Angle,R) :- R is abs(round(Angle/30.0)).
   
dist(X,Y,R) :- R is X*X+Y*Y.

att(Dist,A) :- (Dist<1 -> A=1 ; A = Dist).

angle(X,Y,C,R) :- theta(X,Y,Theta),
  R is ((Theta+C) mod 360) - 180.
  
theta(X,Y,R) :- 
  atan2(Y,X,A2),
  R is round(55.0 * A2).
  
atan2(X,Y,R) :- 
   (Y=0 -> (X>0 -> R is pi/2  ; R is -pi/2)
        ;  R is atan(X/Y)).
        
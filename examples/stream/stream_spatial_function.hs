spatial :: [[(Int,Int)]] -> (Int,Int)
spatial [as,((x,y):gs),((c,_):cs)] = (left/att,right/att)
 where theta = round (55.0 * (atan2 (fromIntegral y) (fromIntegral x)))
       dist = x*x + y*y
       att = if dist<1 then 1 else dist  -- attenuation
       angle = ((theta + c) `mod` 360 ) - 180
       peek i xs = head (drop i xs)  -- definition of peek function
       (one,_)   =  head as   -- current sound amplitude
       (other,_) = peek (abs (round ((fromIntegral angle) / 6.0))) as   -- look at future
       left  = if angle < 0 then one else other
       right = if angle < 0 then other else one 
       
       
  INPUTS:
  
  as = sound stream -> changes 8 - 44 KHz
  (x,y) = location   -> changes 1 Hz
  c = compass angle in degrees reading  -> 10 Hz
  
  gs: remainder of stream for locations
  cs: remainder of stream for compass angles
  
  angle is in between -180 and +180
  
  divide by 6.0 ->  for 44.1 KHz; we need to store 30 values
  divide by 30.0 -> for UPM 8 KHz system
  
  
  fromIntegral: convert int into floating point number
  atan2 x y = if y!=0 then atan(x/y) else if x>0 then pi/2 else -pi/2
  

spatial(X,Y,C,SoundStream,  LeftOut,RightOut) :-
  dist(X,Y,Dist),
  theta(X,Y,Theta),
  att(Dist,Att),
  angle(X,Y,C,Angle),
  SoundStream = [One|_],
  index(Angle,Index),
  peek(SoundStream,Index,Other),
  (Angle<0
    -> (LeftOut is One/Att, RightOut is Other/Att)
    ;  (LeftOut is Other/Att, RightOut is Other/Att)
  ).
  
peek([H|T],N,R) :-
  (N=<1 -> R = H ; (N1 is N-1, peek(T,N1,R))).

index(Angle,R) :- R is abs(round(angle/6.0)).
   
dist(X,Y,R) :- R is X*X+Y*Y.

att(Dist,A) :- (Dist<1 -> A=1 ; A = Dist).

angle(X,Y,C,R) :- theta(X,Y,T),
  R is ((Theta+C) mod 360) - 180.
  
theta(X,Y,R) :- 
  atan2(Y,X,A2),
  R is round(55.0 * A2).
  
atan2(X,Y,R) :- 
   (Y=0 -> (X>0 -> R is pi/2  ; R is -pi/2)
        ;  R is atan(X/Y)).
        
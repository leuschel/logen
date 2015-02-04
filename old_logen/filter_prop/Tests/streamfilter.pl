:-module(streamfilter,_).

%:- use_module(library(lists)).

%import Int

%data Rat = R Int Int
%represent rational as r(X,Y)

%mul :: Int -> Rat -> Int
%mul a (R c d) = (a*d)

mul(A,rat(_C,D),E) :-
	E is A*D // 1.		% ??? mul(3,r(4,5),15) ??? /

%divide :: Int -> Rat -> Int
%divide b (R c d) = (b*c) `div` d

divide(B,rat(C,D),E) :-
	E is (B*C) // D.  	% /

%stream :: ([[a]] -> b) -> [[a]] -> [Rat] -> Rat -> [b]
%stream f ss fs outF = step zeros f ss steps
% where 
%   max = mul (foldl mul 1 fs) outF
%   steps = map (divide max) ([outF]++fs)
%   zeros = take ((length ss)+1) (repeat 0)

take(0,_,[]).
take(N,[X|Xs],[X|Ys]) :-
	N > 0,
	N1 is N-1,
	take(N1,Xs,Ys).
	
repeat(_,[]).
repeat(X,[X|Xs]) :-
	repeat(X,Xs).
	
mapdivideMax(_,[],[]).
mapdivideMax(Max,[X|Xs],[Y|Ys]) :-
	divide(Max,X,Y),
	mapdivideMax(Max,Xs,Ys).
	
foldlMul([],Z,Z).
foldlMul([X|Xs],M,M2) :-
	mul(M,X,M1),
	foldlMul(Xs,M1,M2).
	
stream(F,Ss,Fs,OutF,Z) :-
	length(Ss,N),
	N1 is N+1,
	take(N1,Zs,Zeros),
	repeat(0,Zs),
	foldlMul(Fs,1,FsMul),
	mul(FsMul,OutF,Max),
	mapdivideMax(Max,[OutF|Fs],Steps),
	step(Zeros,F,Ss,Steps,Z).


%step :: [Int] -> ([[a]]->b) -> [[a]] -> [Int] -> [b]
%step cnts f st steps = if outcnt == 0 then (f st) : moresteps else moresteps
% where
%   moresteps =  (step ncnts f nst steps)
%   (outcnt:incnts) = cnts
%   ncnts = inc cnts steps
%   inc [] [] = []
%   inc (c:cs) (m:ms) = ((c+1) `mod` m) : (inc cs ms)
%   nst = newlists incnts st
%   newlists [] [] = []
%   newlists (c:cs) (l:ls) = nl : (newlists cs ls)
%       where
%          nl = if c==0 then tail l else l  

	
inc([],[],[]).
inc([C|Cs],[M|Ms],[M1|Ms1]) :-
	 M1 is (C+1) mod M,
	 inc(Cs,Ms,Ms1).
	 
newlists([],[],[]).
newlists([C|Cs],[L|Ls],[NL|NLs]) :-
	newlistElement(C,L,NL),
	newlists(Cs,Ls,NLs).
	
newlistElement(0,[_|NL],NL).
newlistElement(N,L,L) :-
	N =\= 0.
	
step(_,_,St,_,[]) :-
	member([],St).
step(Cnts,F,St,Steps,MoreSteps) :-
	Cnts = [X|IncCnts],
	inc(Cnts,Steps,NCnts),
	newlists(IncCnts,St,NSt),
	chooseStep(X,F,St,NSt,NCnts,Steps,MoreSteps).
	
chooseStep(0,F,St,NSt,NCnts,Steps,[Y|Z]) :-
	apply(F,St,Y),
	write(user_output,Y),
	nl(user_output),
	step(NCnts,F,NSt,Steps,Z).
chooseStep(N,F,_St,NSt,NCnts,Steps,Z) :-
	N =\= 0,
	step(NCnts,F,NSt,Steps,Z).

% spatial :: [[(Int,Int)]] -> (Int,Int)
% spatial [as,((x,y):gs),((c,_):cs)] = (left,right)
% where
%   theta = round (55.0 * (atan2 (fromIntegral y) (fromIntegral x)))
%   angle = ((theta + c) `mod` 360 ) - 180
%   peek i xs = head (drop i xs)
%   (one,_)   =  head as
%   (other,_) = peek (abs (round ((fromIntegral angle) / 6.0))) as 
%   left  = if angle < 0 then one else other
%   right = if angle < 0 then other else one 

%  Note: atan2 y x computes the angle (from the positive x-axis) of the 
%  vector from the origin to the point (x,y)
%  atan2 y x returns  a value in the range [-pi, pi] 

atan2(X,Y,Z) :-
	Z is atan(X/Y).

spatial([As,[(X,Y)|_],[(C,_)|_]],(Left,Right)) :-
	atan2(Y,X,Atan),
	Theta is round(55.0 * Atan),
	Angle is ((Theta+C) mod 360)-180,
	As = [(One,_)|_],
	Angle1 is abs(round(Angle / 6.0))//1,  %/
	peek(Angle1,As,(Other,_)),
	pick(Angle,One,Other,Left),
	pick(Angle,Other,One,Right).
	
peek(X,Xs,Y) :-
	drop(X,Xs,[Y|_]).
	
drop(0,Xs,Xs).
drop(N,[],[]) :-
	N>0.
drop(N,[_|Xs],Ys) :-
	N>0,
	N1 is N-1,
	drop(N1,Xs,Ys).
	
pick(X,Y,_,Y) :-
	X < 0.
pick(X,_,Z,Z) :-
	X >= 0.

%tspatial :: [[Int]] -> (Int,Int)
%tspatial [(a:as),(g:gs),(c:cs)] = (left,right)
% where
%   left = a+c+g
%   right = a*c*g

tspatial([[A|_],[G|_],[C|_]],(Left,Right)) :-
	Left is A+C+G,
	Right is A*C*G.
	
apply(spatial,X,Y) :-
	spatial(X,Y).
	
	
%--main :: IO ()
%--main =  do putStrLn . (take 100) . show . (stream spatial [(repeat 0),(repeat 1),(repeat 2)] [(R 1 44100), (R 1 1), (R 1 10)])

	
%pl n = 1 : (map (n*) (pl n))

%dblLst = map (\x->(x,x))

dblLst([],[]).
dblLst([X|Xs],[(X,X)|XXs]) :-
	dblLst(Xs,XXs).
	
coords([]).
coords([(50,50)|Cs]) :-
	coords(Cs).
	
mapMod360([],[]).
mapMod360([X|Xs],[(Y,Y)|Ys]) :-
	Y is X mod 360,
	mapMod360(Xs,Ys).
	
natStream([0]).
natStream([N,N1|Ns]) :-
	natStream([N1|Ns]),
	N is N1+1.

rev([],X,X).
rev([X|Xs],Ys,Zs) :-
	rev(Xs,Ys,[X|Zs]).

applyStream(Ws) :-
	length(Is,50),
	natStream(Is),
	rev(Is,Is1,[]),
	dblLst(Is1,As),
	length(Gs,50),
	coords(Gs),
	mapMod360(Is1,Cs),
	stream(spatial, [As,Gs,Cs],[rat(1,3),rat(1,2),rat(1,7)],rat(1,5),Ws).

%applyStream :: [(Int,Int)]
%--applyStream = stream spatial [map (*2) [1..],(map (*3) [1..]),(map (*5) [1..])] [(R 1 44100), (R 1 1), (R 1 10)]
%--applyStreamT = stream spatial [pl 2,pl 3,pl 5] [(R 1 1), (R 1 2), (R 1 3)] (R 1 3)
%applyStream = stream spatial [dblLst [1..],coords,dblLst ((map (`mod` 360)) [0..]) ] [(R 1 44100), (R 1 1), (R 1 10)] (R 1 44100)
%coords = zip (repeat 50) (repeat 50)
%--coords = zip [-50..] (repeat 50)

%main = putStrLn ( show (take 50 applyStream ) )


%length([],0).
%length([_|X],N1) :-
%	length(X,N),
%	N1 is N+1.
	
%member(X,[X|_]).
%member(X,[_|Y]) :-
%	member(X,Y).
	
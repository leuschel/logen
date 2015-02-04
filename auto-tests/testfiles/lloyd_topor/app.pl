:- dynamic app/3.

app([],X,X).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

napp(X,Y,Z) :- ~app(X,Y,Z).

double(X) :- exists(Z,app(Z,Z,X)).
ndouble(X) :- ~double(X).

app_hnf(X,Y,Z) :- (X=[] & Y=Z) or 
  (exists(H,exists(TX,X=[H|TX] & exists(TZ,Z=[H|TZ] & app_hnf(TX,Y,TZ))))).
  
  
bench :- print(start),nl, statistics(runtime,_),
   bench([a],70),statistics(runtime,[_,X]),
   print(finished(X)),nl.
   
bench2 :- print(start),nl, statistics(runtime,_),
   bench([a],5),statistics(runtime,[_,X]),
   print(finished(X)),nl.

bench(_,0).
bench(L,N) :- N>0, napp(L,L,L), N1 is N-1, bench([a|L],N1).
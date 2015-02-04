

transform(State1,State2,Plan) :-
   transform(State1,State2,[State1],Plan).

transform(State,State,Visited,[]).
transform(State1,State2,Visited,[Action|Actions]) :-
   chooseaction(Action,State1,State2),
   update(Action,State1,State),
   notmember(State,Visited),
   transform(State,State2,[State|Visited],Actions).

chooseaction(Action,State1,State2):-
  suggest(Action,State2), 
  legalaction(Action,State1).
chooseaction(Action,State1,State2):-
  legalaction(Action,State1).

suggest(toplace(X,Y,Z),State) :-
   member(on(X,Z),State), 
   place(Z).
suggest(toblock(X,Y,Z),State) :-
   member(on(X,Z),State),
   my_block(Z).

legalaction(toplace(Block,Y,Place),State) :-
   on(Block,Y,State),
   clear(Block,State),
   place(Place),
   clear(Place,State).

legalaction(toblock(Block1,Y,Block2),State) :-
   on(Block1,Y,State),
   clear(Block1,State),
   my_block(Block2),
   Block1 \== Block2,
   clear(Block2,State).

clear(X,State) :-
   my_block(A),
   notmember(on(A,X),State).
on(X,Y,State) :-
   member(on(X,Y),State).

update(toblock(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).
update(toplace(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).

member(X,[X|Y]).
member(X,[F|T]) :-
   member(X,T).

notmember(X,[]).
notmember(X,[F|T]) :-
   X \== F,
   notmember(X,T).

substitute(X,Y,[],[]).
substitute(X,Y,[X|T],[Y|Ts]) :-
  substitute(X,Y,T,Ts).
substitute(X,Y,[F|T],[F|Ts]) :-
  X \== F,
  substitute(X,Y,T,Ts).

testplan(Name,Plan) :-
  initialstate(Name,I),
  finalstate(Name,F),
  transform(I,F,Plan).

initialstate(test,[on(a,b),on(b,p),on(c,r)]).
finalstate(test,[on(a,b),on(b,c),on(c,r)]).

my_block(a).
my_block(b).
my_block(c).

place(p).
place(q).
place(r).


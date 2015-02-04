pgenconfig(Config) :-
   configuration(Config).

configuration(Config) :-
   verticalcut(Vertical),
   horizontalcut(Horizontal),
   combination(Vertical,Horizontal,Config).

verticalcut(Cut) :-
   cutv(Data),
   wastev(Waste),
   horizontal(H),
   vertical(V),
   cut(H,V,Data,Waste,Cut).

horizontalcut(Horizontal) :-
   cuth(Data),
   horizontal(H),
   wasteh(Waste),
   gencut(Data,H,Waste,Horizontal).

gencut([],H,Waste,[]).
gencut([[Larg,Ldata]|Tail],H,Waste,[[Larg,Cut]|Rtail]) :-
   cut(Larg,H,Ldata,Waste,Cut),
   gencut(Tail,H,Waste,Rtail).

cut(Sizecut,Lengthcut,Listecut,Waste,[Cut,Costcut]) :-
   horizontal(H),
   vertical(V),
   Surface is (Lengthcut * Sizecut) - (Waste * H * V / 100),
   generatecut(Sizecut,Surface,Lengthcut,Listecut,Cut,Cost),
   Costcut is (((Lengthcut * Sizecut) - Cost) * 100)/((H * V)/100).


generatecut(Larg,Min,Max,Current,Curs,[F|T],Sol,Cost2,Nb) :-
   Nb < 4,
   Current1 is Current + F,
   Current2 is Current1 * Larg,
   Current1 =< Max,
   generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],Sol,Cost2,Nb).

generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],[F],Current2,Nb) :-
   X is Max - Current1,
   Current2 >= Min,
   nobeter(X,[F|T],Nb).

generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],[F|Sol],Cost2,Nb) :-
   Nb1 is Nb + 1,
   generatecut(Larg,Min,Max,Current1,Current2,[F|T],Sol,Cost2,Nb1).

generatecut(Larg,Min,Max,Current,Current2,[F|T],Sol,Cost2,Nb) :-
   generatecut(Larg,Min,Max,Current,Current2,T,Sol,Cost2,1).

generatecut(Larg,Min,Max,Liste,Sol,Cost2) :-
   generatecut(Larg,Min,Max,0,0,Liste,Sol,Cost2,1).


nobeter(X,[F|T],3) :- 
   nobeter(X,T).
nobeter(X,[F|T],N) :-
   N \== 3,
   nobeter(X,[F|T]).
nobeter(X,[],N).

nobeter(X,[]).
nobeter(X,[Y|U]) :-
   X < Y,
   nobeter(X,U).



combination(Vertical,Horizontal,Config) :-
   emptyconfig(Empty),
   combine(Vertical,Horizontal,Empty,Config).

combine([F,C],Horizontal,Init,[Sol,Cost,[F,[P1,Fv],[P2,Sv]]]) :-
   P1 = [X1|X1s],
   P2 = [X2|X2s],
   split(F,Lres),
   member([P1,P2],Lres),
   combcut(Init,P1,Horizontal,New,C1,Fv),
   combcut(New,P2,Horizontal,Sol,C2,Sv),
   Cost is C1 + C2 + C.

combcut(Init,[Nb|R],Horizontal,Sol,Cost,Cut) :-
   find(Nb,Horizontal,Lcutnb),
   member([Cut,C1],Lcutnb),
   construct(Cut,Nb,Lcut),
   addliste(Lcut,Init,Init1),
   combcutaux(Init1,Cut,R,Horizontal,Sol,C2),
   Cost is C1 + C2.

combcutaux(Init,Cut,[],Horizontal,Init,0).
combcutaux(Init,Cut,[Nb|R],Horizontal,Sol,Cost) :-
   find(Nb,Horizontal,Lcutnb),
   member([Cut,C1],Lcutnb),
   construct(Cut,Nb,Lcut),
   addliste(Lcut,Init,Init1),
   combcutaux(Init1,Cut,R,Horizontal,Sol,C2),
   Cost is C1 + C2.

emptyconfig(C) :-
   empty(L),
   genempty(L,C).

genempty([],[]).
genempty([[X,Y]|Tail],[[X,Y,0]|Rtail]) :-
   genempty(Tail,Rtail).

split([],[],[]).
split([X|Y],[X|Res1],Res2) :-
   split(Y,Res1,Res2).
split([X|Y],Res1,[X|Res2]) :-
   split(Y,Res1,Res2).

allsplit(F,[]).
allsplit(F,[R|T]) :-
   split(F,R),
   allsplit(F,T).

split([F|T],[[F|P1],P2]) :-
   split(T,P1,P2),
   intersection([F|P1],P2).

intersection(P1,P2) :-
   member(X,P1),
   member(X,P2).


construct([],Nb,[]).
construct([X|Y],Nb,[[X,Nb]|Res]) :-
   construct(Y,Nb,Res).

addelement([X,Y],[[X,Y,Nb]|Tail],[[X,Y,Nb1]|Tail]) :-
   Nb1 is Nb + 1.
addelement([X,Y],[[Y,X,Nb]|Tail],[[Y,X,Nb1]|Tail]) :-
   [X,Y] \== [],
   Nb1 is Nb + 1.
addelement(F,[[X,Y,Nb]|Ts],[[X,Y,Nb]|Tres]) :-
   F \== [X,Y],
   F \== [Y,X],
   addelement(F,Ts,Tres).

addliste([],L,L).
addliste([X|Y],L,Lres) :-
   addelement(X,L,L1),
   addliste(Y,L1,Lres).


find(Nb,[[Nb,Reste]|Tail],Reste).
find(Nb,[[N,Reste]|Tail],Res) :-
   Nb \== N,
   find(Nb,Tail,Res).


append([],X,X).
append([X|Y],Z,[X|R]) :-
   append(Y,Z,R).

member(X,[X|R]).
member(X,[Y|R]):-
   member(X,R).

horizontal(1).
pgenconfig(Config) :-
   configuration(Config).

configuration(Config) :-
   verticalcut(Vertical),
   horizontalcut(Horizontal),
   combination(Vertical,Horizontal,Config).

verticalcut(Cut) :-
   cutv(Data),
   wastev(Waste),
   horizontal(H),
   vertical(V),
   cut(H,V,Data,Waste,Cut).

horizontalcut(Horizontal) :-
   cuth(Data),
   horizontal(H),
   wasteh(Waste),
   gencut(Data,H,Waste,Horizontal).

gencut([],H,Waste,[]).
gencut([[Larg,Ldata]|Tail],H,Waste,[[Larg,Cut]|Rtail]) :-
   cut(Larg,H,Ldata,Waste,Cut),
   gencut(Tail,H,Waste,Rtail).

cut(Sizecut,Lengthcut,Listecut,Waste,[Cut,Costcut]) :-
   horizontal(H),
   vertical(V),
   Surface is (Lengthcut * Sizecut) - (Waste * H * V / 100),
   generatecut(Sizecut,Surface,Lengthcut,Listecut,Cut,Cost),
   Costcut is (((Lengthcut * Sizecut) - Cost) * 100)/((H * V)/100).


generatecut(Larg,Min,Max,Current,Curs,[F|T],Sol,Cost2,Nb) :-
   Nb < 4,
   Current1 is Current + F,
   Current2 is Current1 * Larg,
   Current1 =< Max,
   generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],Sol,Cost2,Nb).

generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],[F],Current2,Nb) :-
   X is Max - Current1,
   Current2 >= Min,
   nobeter(X,[F|T],Nb).

generatecutaux(Larg,Min,Max,Current1,Current2,[F|T],[F|Sol],Cost2,Nb) :-
   Nb1 is Nb + 1,
   generatecut(Larg,Min,Max,Current1,Current2,[F|T],Sol,Cost2,Nb1).

generatecut(Larg,Min,Max,Current,Current2,[F|T],Sol,Cost2,Nb) :-
   generatecut(Larg,Min,Max,Current,Current2,T,Sol,Cost2,1).

generatecut(Larg,Min,Max,Liste,Sol,Cost2) :-
   generatecut(Larg,Min,Max,0,0,Liste,Sol,Cost2,1).


nobeter(X,[F|T],3) :- 
   nobeter(X,T).
nobeter(X,[F|T],N) :-
   N \== 3,
   nobeter(X,[F|T]).
nobeter(X,[],N).

nobeter(X,[]).
nobeter(X,[Y|U]) :-
   X < Y,
   nobeter(X,U).



combination(Vertical,Horizontal,Config) :-
   emptyconfig(Empty),
   combine(Vertical,Horizontal,Empty,Config).

combine([F,C],Horizontal,Init,[Sol,Cost,[F,[P1,Fv],[P2,Sv]]]) :-
   P1 = [X1|X1s],
   P2 = [X2|X2s],
   split(F,Lres),
   member([P1,P2],Lres),
   combcut(Init,P1,Horizontal,New,C1,Fv),
   combcut(New,P2,Horizontal,Sol,C2,Sv),
   Cost is C1 + C2 + C.

combcut(Init,[Nb|R],Horizontal,Sol,Cost,Cut) :-
   find(Nb,Horizontal,Lcutnb),
   member([Cut,C1],Lcutnb),
   construct(Cut,Nb,Lcut),
   addliste(Lcut,Init,Init1),
   combcutaux(Init1,Cut,R,Horizontal,Sol,C2),
   Cost is C1 + C2.

combcutaux(Init,Cut,[],Horizontal,Init,0).
combcutaux(Init,Cut,[Nb|R],Horizontal,Sol,Cost) :-
   find(Nb,Horizontal,Lcutnb),
   member([Cut,C1],Lcutnb),
   construct(Cut,Nb,Lcut),
   addliste(Lcut,Init,Init1),
   combcutaux(Init1,Cut,R,Horizontal,Sol,C2),
   Cost is C1 + C2.

emptyconfig(C) :-
   empty(L),
   genempty(L,C).

genempty([],[]).
genempty([[X,Y]|Tail],[[X,Y,0]|Rtail]) :-
   genempty(Tail,Rtail).

split([],[],[]).
split([X|Y],[X|Res1],Res2) :-
   split(Y,Res1,Res2).
split([X|Y],Res1,[X|Res2]) :-
   split(Y,Res1,Res2).

allsplit(F,[]).
allsplit(F,[R|T]) :-
   split(F,R),
   allsplit(F,T).

split([F|T],[[F|P1],P2]) :-
   split(T,P1,P2),
   intersection([F|P1],P2).

intersection(P1,P2) :-
   member(X,P1),
   member(X,P2).


construct([],Nb,[]).
construct([X|Y],Nb,[[X,Nb]|Res]) :-
   construct(Y,Nb,Res).

addelement([X,Y],[[X,Y,Nb]|Tail],[[X,Y,Nb1]|Tail]) :-
   Nb1 is Nb + 1.
addelement([X,Y],[[Y,X,Nb]|Tail],[[Y,X,Nb1]|Tail]) :-
   [X,Y] \== [],
   Nb1 is Nb + 1.
addelement(F,[[X,Y,Nb]|Ts],[[X,Y,Nb]|Tres]) :-
   F \== [X,Y],
   F \== [Y,X],
   addelement(F,Ts,Tres).

addliste([],L,L).
addliste([X|Y],L,Lres) :-
   addelement(X,L,L1),
   addliste(Y,L1,Lres).


find(Nb,[[Nb,Reste]|Tail],Reste).
find(Nb,[[N,Reste]|Tail],Res) :-
   Nb \== N,
   find(Nb,Tail,Res).


%append([],X,X).
%append([X|Y],Z,[X|R]) :-
%   append(Y,Z,R).
%
%member(X,[X|R]).
%member(X,[Y|R]):-
%   member(X,R).


ground_list([]).
ground_list([F|T]) :-
   my_ground(F),
   ground_list(T).

my_ground(10).
my_ground(20).
my_ground(40).
my_ground(X) :-
   ground_list(X).

horizontal(2500).
horizontal(250).
vertical(1220).
vertical(122).
wastev(10).
wastev(1).
wasteh(4).
wasteh(5).

cutv(X) :- ground_list(X).
cuth(X) :- ground_list(X).
empty(X) :- ground_list(X).
quantity(X) :- ground_list(X).



test(C,D,E,F) :- 
	append([a,a,c|C], [x,y], D),
	append1([a,a,c|E], [x,y], F).

append([],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).

append1([],A,A).
append1([A|B],C,[A|D]) :- append1(B,C,D).

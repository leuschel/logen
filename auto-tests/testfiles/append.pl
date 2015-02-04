


test(C,D) :- app([a,a,c|C], [x,y], D).
app([],B,B).
app([X|Xs],Y, [X|Zs]) :- app(Xs,Y,Zs).

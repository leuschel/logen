/* Expected results [app(A,B,C)-[B-C=<rat(0,1),B>=rat(0,1),A= -(B)+C]]. */
app([],A,A).
app([X|Xs],Y,[X|Zs]) :- app(Xs,Y,Zs).

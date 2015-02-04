append([],Y,Y).
append([X|Xs],Y,[X|Zs]) :- append(Xs,Y,Zs).


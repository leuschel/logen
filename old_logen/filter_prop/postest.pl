p(A,A,A,A,A,A,A,A).
p(H,G,F,E,D,C,B,c):-p(H,G,F,E,D,C,B,A).
p(G,F,E,D,C,B,c,A):-p(G,F,E,D,C,B,A,c).
p(F,E,D,C,B,c,A,A):-p(F,E,D,C,B,A,c,c).
p(E,D,C,B,c,A,A,A):-p(E,D,C,B,A,c,c,c).
p(D,C,B,c,A,A,A,A):-p(D,C,B,A,c,c,c,c).
p(C,B,c,A,A,A,A,A):-p(C,B,A,c,c,c,c,c).
p(B,c,A,A,A,A,A,A):-p(B,A,c,c,c,c,c,c).
p(c,A,A,A,A,A,A,A):-p(A,c,c,c,c,c,c,c).

:-use_module(library(clpr)).


mortgage(P,T,_I,_R,B) :- {T=0, B=P}.
mortgage(P,T,I,R,B) :- {P>= 0, T >= 1, NT = T -1, NP = P + P * I -R}, mortgage(NP,NT,I,R,B).


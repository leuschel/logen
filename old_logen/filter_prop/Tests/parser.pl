nont(X,T,R) :- 
	t(a,T,V), 
	nont(X,V,R).
nont(X,T,R) :- 
	t(X,T,R).

t(X,[X|Es],Es).

:-filter(nont(static,dynamic,dynamic)).
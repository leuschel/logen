
lookup(X,[(Key/Val)|_],Res) :-
    X==Key, Res=Val.
lookup(X,[(Key/_)|T],Res) :-
    X\==Key, lookup(X,T,Res).
    

lookup2(X,[(Key/Val)|T],Res) :-
    (X==Key -> Res=Val ; lookup2(X,T,Res)).
    
    
test(X) :- lookup(xx,[(yy/2),(zz/3),(xx/5),(xx/2),(vv/11)],X).
test(X) :- lookup2(vv,[(yy/2),(zz/3),(xx/5),(xx/2),(vv/11)],X).
    
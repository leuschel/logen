

:- use_module(library(lists)).

:- op(750,fy,'~').


test(X,L) :- append([~a,~b,~c],X,X2), append(X2,[~c],X3), length(X3,L).

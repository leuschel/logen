:- include('../logen_source/logen').

:- use_module(library(write)).

:- residual p/1.
:- filter p(dynamic) : p_call.
logen(p_call, p(X)) :- logen(rescall,write(X)).




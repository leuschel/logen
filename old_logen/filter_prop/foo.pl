:- module(foo, [main/0],[pure]).

:- use_module(library('engine/arithmetic')).
:- use_module(library(lists)).
:- use_module(library('engine/atomic_basic')).
:- use_module(library(write)).

main :- 
	write(user_output, 'hello world'),
	1 is 1,
	pqr(X),
	append([a],[b,c],X),
	atom_concat(a,b,_).
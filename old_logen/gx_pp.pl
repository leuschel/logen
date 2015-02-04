

:- module('gx_pp.pl', [gx_clause/2, assert_gx_clauses/1, print_gx_clauses/0, reset_gxpp/0]).
:- dynamic gx_clause/2.


reset_gxpp :-
	retractall(gx_clause(_,_)).
	


assert_gx_clauses([]).
assert_gx_clauses([Clause|Tail]) :- assert_gx_clause(Clause), assert_gx_clauses(Tail).

assert_gx_clause(clause(H,B)) :-
	!,
	assert(gx_clause(H,B)).

assert_gx_clause(':-'(Head,Body)) :-
         !,
         assert(gx_clause(Head,Body)).

assert_gx_clause(Fact) :-
	assert(gx_clause(Fact, true)).

print_gx_clauses :-
	print_gx_decl,
	print_gx_clauses1.


print_gx_clauses1 :-
	gx_clause(H,B),
	portray_clause(':-'(H,B)),
	fail.
print_gx_clauses1.



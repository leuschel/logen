:- module('memo_pp.pl',
	  [
	   reset_memopp/0,
	   memo_clause/2,
	   memo_decl/1,
	  assert_memo_clause/1,
	  assert_failing_predicate_declarations/0
	  
	  ]).

:- dynamic memo_clause/2.
:- dynamic memo_decl/1.


:- use_module('gx_pp.pl').

reset_memopp :-
	retractall(memo_clause(_,_)),
	retractall(memo_decl(_)).

assert_memo_clause(':-'(Head,Body)) :-
        !,
        assert(memo_clause(Head,Body)).



assert_memo_clause(':-'(Declaration)) :-
        !,
	assert(memo_decl(Declaration)).
assert_memo_clause(Head) :-
	assert(memo_clause(Head,true)).

        

assert_failing_predicate_declarations :-
    memo_clause(memo_table_entry(_Id,_Call,ResidualCall,_,_), true),
    \+gx_clause(clause(ResidualCall,_)),
    functor(ResidualCall, Func,Arity),
    functor(NewCall, Func,Arity),
    assert_gx_clause(clause(NewCall, fail)),
    fail.
assert_failing_predicate_declarations.




print_memo_clauses :-
	print_memo_decl,
	print_memo_clauses1.


print_memo_decl :-
	memo_decl(D),
	portray_clause(':-'(D)),
	fail.
print_memo_decl.

print_memo_clauses1 :-
	memo_clause(H,B),
	portray_clause(':-'(H,B)),
	fail.
print_memo_clauses1.
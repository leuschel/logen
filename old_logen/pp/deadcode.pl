:- module(deadcode, [deadcode/0]).


:- use_module('../logen_post').

:- use_module(library(lists)).

:- dynamic generate/1.
:- dynamic processing/1.

mark_generate(Call) :-
	%portray_clause(user_error, mark_generate(Call)),
	(generate(Call) ->
	    true
	;
	    assertz(generate(Call))
	).

deadcode :-
	retractall(generate(_)),
	retractall(processing(_)),
	
	find_external_calls,	
	propagate,
	clone_decl,
	generate_code.
	
	%clone_spec.


propagate :-
	generate(Call),
	propagate(Call),
	fail;true.


propagate(Call) :-
	processing(Call),
	!. % stop

propagate(F/A) :-
	assert(processing(F/A)),
	functor(Call, F,A),
	%portray_clause(prop(Call)),
	findall((Body), spec_clause((Call:-Body)), Calls),
	%portray_clause(all_calls(Call,Calls)),
	propagateL(Calls).

	
propagateL([]).
propagateL([B|Bs]) :-
	%portray_clause(body(B)),
	propagateBody(B),
	propagateL(Bs).

%propagateBody(B) :-
	%portray_clause(body(B)).

propagateBody((A,B)) :-
	!,
	propagateBody(A),
	propagateBody(B).

propagateBody((A->B;C)) :-
	!,
	propagateBody(A),
	propagateBody(B),
	propagateBody(C).

	
propagateBody(\+(Call)) :-
	!,
	propagateBody(Call).
propagateBody(call(Call)) :-
	nonvar(Call),
	!,
	propagateBody(Call).

	
propagateBody(Call) :-
	functor(Call, F,A),
	mark_generate(F/A),
	%portray_clause(user_error, prop_over(Call)),
	propagate(F/A).


	
	


find_external_calls :-
	memo_clause(table(_,Call, Properties)),
	memberchk(crossmodule, Properties),
	functor(Call, F, A),
	mark_generate(F/A),
	fail ; true.



generate_body((A,B)) :-
	!,
	generate_body(A),
	generate_body(B).

generate_body(C) :-
	functor(C,F,A),
	mark_generate(F/A).	
	


%%% For each generate code predicate make output the corresponding code
generate_code :-
	generate(F/A),
	functor(Call, F,A),
	generate_code(Call),
	fail; true.

generate_code(Call) :-
	memo_clause(table(Res,Call, Properties)),
	save_memo_clause(table(Res,Call,Properties)).
	
	
generate_code(Call) :-
	%% get all facts
	findall(Call, spec_clause(Call), Facts),
	save_list(Facts).

generate_code(Call) :-
	%% get all clauses
	findall((Call:-Body), spec_clause((Call:-Body)), Facts),	
	save_list(Facts).

save_list([]).
save_list([T|Ts]) :-
	save_spec_clause(T),
	save_list(Ts).

	

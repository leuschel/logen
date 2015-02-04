
match_request(A, B, Requestor, ResidualCall) :-
        (   memoizer:find_pattern(match, match(A,B), ResidualCall, Requestor) ->
            true
        ;   memoizer:insert_pattern_with_filter_types(match, match(A,C), [[static,dynamic]], Requestor, ResidualCall),
            match(A,C)=match(A,B)
        ).
match_u(A, B, C) :-
        match1_u(A, B, A, B, C).
match1_u([], _, _, _, true).
match1_u([A|_], [B|_], C, [_|D], (A\==B,E)) :-
        match1_request(C, D, C, D, internal, E).
match1_u([A|B], [A|C], D, E, F) :-
        match1_u(B, C, D, E, F).


query(Call) :-
	% do we have a filter?
	get_filter(Call,Filter),
	memo_call(Call, Filter).

:- dynamic memo_table/3.
:- dynamic clauses/1.

clear_all :-
	retractall(memo_table(_,_,_)),
	retractall(clauses(_)),
	retractall(sym(_)).

run_test :-
	clear_all,
	Call = match([a,a,b],A),	
	memo_call(Call,RX),
	print_clauses,
	portray_clause(user,(Call:-RX)).
	

	
	

%% if you want to memo a call..... you should look up in memo table first then...
memo_call(Call, ResCall) :-
	(memo_table(Call, ResCall, _) ->
	    true
	;
	    (
	      %% generalise and filter
	      generalise_and_filter(Call, GenCall, FCall),
	      assert(memo_table(GenCall, FCall,[internal])),
	      findall((FCall:-Body), unfold(GenCall, Body), XClauses),
	      assert(clauses(XClauses)),
	      memo_table(Call, ResCall, _)
	    )
	).
	      

unfold(Call, Code) :-
	portray_clause(unfold(Call)),
	rule(_ID,Call,Body),
	portray_clause(unfold(Call,Body)),
	body(Body,Code).

body(true,true).
body((A,B),(NA,NB)) :-
	body(A,NA),
	body(B,NB).

body(logen(id(ID),Call), Code) :-
	get_ann(ID,Call, Ann),
	body(logen(Ann, Call), Code).

body(logen(rescall, Call), Call).


	
	
generalise_and_filter(Call, GCall, FCall) :-
	get_filter(Call, ArgTypes),
	Call =.. [P|Args],
	gen_filter(ArgTypes, Args, GenArgs, FiltArgs),
	GCall =.. [P|GenArgs],
	new_head(P,NP),
	FCall =.. [NP|FiltArgs].

gen_filter([],[],[],[]).

gen_filter([static|A], [B|C], [B|D], E) :-
        gen_filter(A, C, D, E).
gen_filter([static_nf|A], [B|C], [B|D], [B|E]) :-
        gen_filter(A, C, D, E).
gen_filter([dynamic|A], [_|B], [C|D], [C|E]) :-
        gen_filter(A, B, D, E).

:- dynamic sym/1.

get_sym(X1) :-
	sym(X),!,
	X1 is X +1,
	retract(sym(X)),
	assert(sym(X1)).

get_sym(0) :-
	assert(sym(0)).
new_head(Func,NFunc) :-
	get_sym(X),
	name(X, XS),
	name(Ext,[95,95|XS]),
	atom_concat(Func,Ext,NFunc).


get_ann(Id,Call, Ann) :-
	ann(Id, Ann).


	
get_filter(Call, Filter) :-
	filter(Call, Filter),
	!.

get_filter(Call, Filter) :-
	ask_question(get_filter, [Call], Filter).

ask_question(get_filter, [Call], Filter) :-
	format(user, "[filter] What is the filter for ~w?", [Call]),
	read(user, Filter).

:- dynamic filter/2.
:- dynamic ann/2.

filter(match(A,B), [static,dynamic]).

rule(0,match(A,B), logen(id(1), match(A,B,A,B))).



ann(1,rescall).



print_clauses :-
	clauses(X),
	print_clauses(X),
	fail.
print_clauses.

print_clauses([]).
print_clauses([X|Xs]) :-
	portray_clause(user,X),
	print_clauses(Xs).


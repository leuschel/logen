:- module(convex_analyser, [iterate/1,
			    go/1,
			    showfacts/0,showfacts/1,
			    convex_fact/1,
			    reset_convex/0,
			    reset_convex_after_facts_only/0,
			    remember_clause/1,
			    set_convex_norm/1]).



:- use_module(convex_hull).
:- use_module(convex_norm).
:- use_module(builtin_norms).

:- use_module(library(terms)).
:- use_module(library(clpq)).
:- use_module(library(lists)).

:- use_module(library(system),[datime/1]).



:- dynamic current_sol/2.
:- dynamic abs_clause/3.

:- dynamic clause_changed/2.

:- dynamic debug_log/2.
:- dynamic debug_on/0.

:- dynamic sol_cache/2.
save_cache :-
	retractall(sol_cache(_,_)),
	save_cache1.

save_cache1 :-	
	current_sol(H,Constraints),
	portray_clause(user_error, cache(H,Constraints)),
	assert(sol_cache(H,Constraints)),
	fail; true.




	




%debug_on.

open_log(Filename) :-	
	open(Filename, write, Stream),
	assert(debug_log(Filename, Stream)),
	datime(D),
	write_log(Filename, portray_clause(D)).

close_log(Filename) :-
	debug_log(Filename,Stream),
	close(Stream),
	retractall(debug_log(Filename,Stream)).

write_log(Filename, Call) :-
	debug_on,
	!,
	(debug_log(Filename, Stream) ->
	    true
	;
	    open_log(Filename),
	    debug_log(Filename, Stream)
	
	),
	format(Stream,'~@', [Call]),
	flush_output(Stream).
write_log(_,_).


	
	
	
	


convex_fact((H:-C)) :- current_sol(H,C).






remember_clause(Clause) :-	
	convert_to_clause(Clause,C),
	(C = true ->
	    true
	;
	    C = my_clause(Head,ListBody),
	    abstract_clause(Head,ListBody,AbsHead,AbsBody,AbsConstraints),
	    assert(abs_clause(AbsHead,AbsBody,AbsConstraints))
	).

			  
	

load_file(File) :-	
	see(File),
	load_clauses,
	seen.
	


load_clauses :-
	read_term(T,[]),
	(T = end_of_file ->
	    true
	;
	    remember_clause(T),
	    load_clauses
	).

convert_to_clause(':-'(Head,Body), my_clause(Head,ListBody)) :-
          !,
          convert_to_list(Body,ListBody).
convert_to_clause(':-'(_Dec), true).
convert_to_clause(Fact, my_clause(Fact, [])).


convert_to_list((A,B), [A|Rest]) :-
	!,
	convert_to_list(B,Rest).
convert_to_list(A, [A]).


   
   

l_convex_hull([sol(Ls,C)],Ls,C).
l_convex_hull([sol(L1,C1),sol(L2,C2)|T],ResL,ResC) :-
  convex_hull(L1,C1,L2,C2,L12,C12),
  %portray_clause(convex_hull(L1,C1,L2,C2,L12,C12)),
  l_convex_hull([sol(L12,C12)|T],ResL,ResC).

depends_on_changed(_,1) :- !.

depends_on_changed([B|_],I) :-
	clause_changed(B,I),!.
depends_on_changed([_|Bs],I) :-	
	depends_on_changed(Bs,I).


propagate(Call,Const,I) :-	
	abs_clause(Call,Body,ClauseConstraints),
	copy_term(Body,CBody),
	(depends_on_changed(CBody,I) ->
	    write_log('log_convex.txt',portray_clause(processing(Call, Body)))
	    
	;
	    write_log('log_convex.txt',portray_clause(prop_NOT_processing(Call, Body))),
	    fail
	),
	find_sols(Body,BodyCs),
	append(ClauseConstraints,BodyCs, Const).






find_sols([],[]).
find_sols([Call|T], Cs) :-
	builtin_convex(Call, CallConstraints),
	write_log('log_convex.txt', portray_clause(is_builtin(Call,CallConstraints))),	
	!,
	find_sols(T,RestC),
	append(CallConstraints,RestC,Cs).


find_sols([Call|T],Cs) :-
	write_log('log_convex.txt', portray_clause(calling(Call))),
	current_sol(Call,CallConstraints),
	write_log('log_convex.txt', portray_clause(constraints(CallConstraints))),
	find_sols(T,RestC),
	append(CallConstraints,RestC,Cs).

ensure_greater_zero([],[]).
ensure_greater_zero([A|As], [A>=0|Bs]) :- var(A), ensure_greater_zero(As,Bs).
ensure_greater_zero([A|As], Bs) :- ground(A), ensure_greater_zero(As,Bs).



step(I) :-
	%% only really want to redo things which depend on things we have changed	
	propagate(Call,Const,I),
	%% Logging information
	write_log('log_convex.txt', (nl,portray_clause((step(I):-propagate(Call,Const))))),	
	functor(Call,F,A),
	functor(DenCall,F,A),

	(current_sol(DenCall,OldConstraints) ->
	    % there is previous fact...
	    ( %% we should choose to widen or just make convex hull
		(I mod 3 =:= 0 ->
		    write_log('log_convex.txt', portray_clause(widening(step(I)))),
		    
		    
		    term_variables(DenCall,Vars),
		    term_variables(Call,Vars1),		    
		    l_convex_hull([sol(Vars,OldConstraints),sol(Vars1,Const)],Vars,ConvexHull),
		    %portray_clause(l_convex_hull([sol(Vars,OldConstraints),sol(Vars1,Const)],Vars,Hull)),
		    
		    widen(DenCall,Vars,OldConstraints,ConvexHull,Vars,Hull),
		    write_log('log_convex.txt',(portray_clause(call(DenCall)-widen(Vars,OldConstraints,ConvexHull,Vars,Hull)),nl))
		    
		    
		    
		
		
		    
		;
		    
		    term_variables(DenCall,Vars),
		    term_variables(Call,Vars1),
		    %portray_clause(old(DenCall,OldConstraints)),
		    l_convex_hull([sol(Vars,OldConstraints),sol(Vars1,Const)],Vars,Hull)
		    %portray_clause(l_convex_hull([sol(Vars,OldConstraints),sol(Vars1,Const)],Vars,Hull))
		   
		)		
	    ),

	    Assert = current_sol(DenCall,Hull)	
	;
	    % this is the first solution so just assert it?
	    

	    term_variables(Call,Vars),
	    l_convex_hull([sol(Vars,Const),sol(Vars,Const)],Vars,ConvexHull),
	    %l_convex_hull([sol(Vars,Const),sol(Vars,[1=1])],Vars,ConvexHull),
	    
	    %ConvexHull = Const,
	    %simplify_constraints(Const,ConvexHull),
	    Assert = current_sol(Call,ConvexHull)
	    %Assert = current_sol(Call, Const) 

	),
	%portray_clause((Call-Const->Assert)),
	my_assert(Assert,I).


%% these are not used, made algorithm longer strangely....
simplify_constraints(C,Simple) :-
	term_variables(C,Vars),
	copy_term(C-Vars, C1-CVars),
	tell_cs(C1),
	remove_ground(CVars,NGCVars, Vars,NGVars,GndC),
	dump(NGCVars,NGVars,Simple1),
	append(GndC, Simple1, Simple).

remove_ground([],[],[],[],[]).
remove_ground([A|As],NGAs,[B|Bs],NGBs,[B=A|GndC]) :-
	ground(A),
	!,
	remove_ground(As,NGAs,Bs,NGBs,GndC).
remove_ground([A|As],[A|NGAs],[B|Bs],[B|NGBs],GndC) :-
	remove_ground(As,NGAs,Bs,NGBs,GndC).


	

my_assert(current_sol(Call,Const),I) :-
	functor(Call,F,A),
	functor(Copy,F,A),
	
	
	(current_sol(Copy,OldConstraints) ->
	    %% Old constraints exist, check entailment and replace if needed
	    
	    term_variables(Copy,CopyArgs),
	    term_variables(Call,CallArgs),
	    (entailed_check(CallArgs,Const,CopyArgs,OldConstraints) ->
		true
	    ;
		%% assert it
		make_assertion(current_sol(Call,Const),I)
	    )
	    
	    
	
	;
	    %% No previous so just assert..
	    make_assertion(current_sol(Call,Const),I)

	).

%% A solution has changed so we must set the flag and save it
make_assertion(current_sol(Call,Const),I) :-
	copy_term(Call, ChangedCopy),
	I1 is I + 1,
	assert(clause_changed(ChangedCopy,I1)),
	copy_term(Call,Copy),	
	retractall(current_sol(Copy,_)),
	assert(current_sol(Call,Const)),
	%portray_clause(assert(current_sol(Call,Const))),
	true.


entailed_check(CallArgs,Constraints, CopyArgs,OldConstraints) :-
	%portray_clause(entailed_check(CallArgs,Constraints, CopyArgs,OldConstraints)),
	copy_term(CallArgs-CopyArgs-Constraints-OldConstraints,
		  Args-Args-CConstr-COld),
	tell_cs(CConstr),
	l_entailed_check(COld).

l_entailed_check([]).
l_entailed_check([C|Cs]) :-
	entailed(C),
	l_entailed_check(Cs).

showfacts(Stream) :-
    current_sol(H,C),
    
    portray_clause(Stream,(H:-C)),    fail ; true.

showfacts :-
	current_output(S),	
	showfacts(S).



test_convex_time :- missing_stuff. /* <----------------------- */
	
	

go(File) :-
	reset_convex,
	load_file(File),
	iterate(1).

reset_convex :-
	write_log('log_convex.txt', portray_clause(reset_convex)),
	retractall(abs_clause(_,_,_)),
	retractall(current_sol(_,_)),
	retractall(clause_changed(_,_)).

reset_convex_after_facts_only :-
	write_log('log_convex.txt', portray_clause(reset_convex_ready_for_facts)),
	retractall(clause_changed(_,_)).

	





iterate(I) :- step(I), fail.
iterate(I) :-
	I1 is I +1,
	clause_changed(_,I1), %% has a clause been updated, if so loop
	!,
	retractall(clause_changed(_,I)),
	
	iterate(I1).

iterate(_) :-
	write_log('log_convex.txt', (nl,portray_clause(finished))),
	(debug_on -> (debug_log('log_convex.txt', S),showfacts(S)); true).
	







%% Widen two sets of constraints...
%% only keep constraints entailed by previous
widen(Head,Vars, Const,Const1, WVar,WConstraints) :-
	
	copy_term(Vars-Const-Const1, V-C-C1),
	copy_term(C-V, CopyC-WVar),
	tell_cs(C1),
	entailed_only(C,CopyC,EntailedOnly),

	must_not_fail(add_strict(Head,Const1,Implied)),
	append(Implied,EntailedOnly,WConstraints).

	%% do we still need greater than zero????
	
	%ensure_greater_zero(Vars,GreaterZero),
	%GreaterZero = [],

	%add_strict(Head,Const1,Strict),
	%append(Strict,GreaterZero,Implied),
	

must_not_fail(Call) :- call(Call), !.
must_nof_fail(Call) :-
	format(user_error, "~w CALL FAILED!!!!",[Call]),
	halt.

add_strict(Head, C2, Strict) :-

	term_variables(Head,Vars),
	copy_term(Vars-C2, CVars-CC2),
	tell_cs(CC2),
	add_strictL(CVars,CVars,Vars,Vars,Strict).
	%add_strict(CVars,CVars,Vars,Vars,Strict).

add_strictL([],_CVars,[],_Vars,[]).
add_strictL([CA|CAs],CVars,[A|As],Vars,Strict) :-
	add_strict(CA,CVars,A,Vars,S1),
	add_strictL(CAs,CVars,As,Vars,S2),
	append(S1,S2,Strict).

	
	

add_strict(_,[],_,[],[]).
add_strict(Var,[B|Bs], CVar,[CB|CBs], [CVar>CB|Strict]) :-
	entailed(Var>B),
	!,
	add_strict(Var,Bs,CVar,CBs,Strict).

add_strict(Var,[_B|Bs], CVar,[_CB|CBs], Strict) :-
	add_strict(Var,Bs,CVar,CBs,Strict).

	
	

entailed_only([],[],[]).
entailed_only([CopyC|CopyT],[OrigC|OrigT],[OrigC|New]) :-
	entailed(CopyC),!,entailed_only(CopyT,OrigT,New).
entailed_only([_C|CopyT],[_|OrigT],Es) :-	
	entailed_only(CopyT,OrigT,Es).



%%% Perhaps we should use this instead of add strict??? it
%%% is more precise yet alot slower! 100 iterations = 690ms vs 90ms
project_vars(Head,C2,Strict) :-
	term_variables(Head,Vars),
	copy_term(Vars-C2, CVars-CC2),
	tell_cs(CC2),
	project_varsL(CVars,Vars,Strict).

project_varsL([], [],[]).
project_varsL([CVar|CVars], [Var|Vars],Strict) :-
	project_vars1(CVar,CVars,Var,Vars,Strict1),
	project_varsL(CVars,Vars,Strict2),
	append(Strict1,Strict2,Strict).

project_vars1(_CVar,[], _Var,[],[]).
project_vars1(CVar,[CNVar|CNVars], Var,[NVar|NVars],Strict) :-	
	dump([CVar,CNVar],[Var,NVar],Cst),
	project_vars1(CVar,CNVars,Var,NVars,Strict1),
	append(Cst,Strict1,Strict).


	
	


test_strict(Strict) :-
	Cs = [B -  X  >0 , B + 2 < A, A > C, Y > X,C >Y],
	Head = foo(A,B,C),
	%add_strict(Head,Cs,Strict).
	project_vars(Head,Cs,Strict).


test_strict10(Strict) :-	
	test_strict(Strict),
	test_strict(Strict1),
	test_strict(Strict2),
	test_strict(Strict3),
	test_strict(Strict4),
	test_strict(Strict5),
	test_strict(Strict6),     
	test_strict(Strict7),
	test_strict(Strict8),
	test_strict(Strict9).


test_strict100(Time,S) :-
	statistics(runtime, [T,_]),
	test_strict10(S),
	test_strict10(S1),
	test_strict10(S2),
	test_strict10(S3),
	test_strict10(S4),
	test_strict10(S5),
	test_strict10(S6),
	test_strict10(S7),
	test_strict10(S8),
	test_strict10(S9),		
	statistics(runtime, [T1,_]),
	Time is T1 - T.	

%:- module('bta_driver', [run_convex/1]).

%:- use_module('convex/input.pl',[load_file/1, my_clause/2, remember_clause/1]).
%:- use_module('convex/db.pl',[fact/1 ]).
%:- use_module('convex/analyse.pl', [iterate/1]).
:- use_module('../examples/bin_loader.pl',[rebuild_ann/2]).
%:- include('convex/input.pl').
%:- include('convex/db.pl').
:- include('convex/analyse.pl').
%:- include('../examples/bin_loader.pl').

:- ensure_loaded('sicstus.pl').
:- include('logen_main.pl').


:- use_module(library(clpq)).


global_check(AnnFile, Unsafe) :-
	run_binmemo(AnnFile),
	run_convex('../examples/match.pl.ann'),
	findall(PP,unsafe_pp(PP), Unsafe),
	portray_clause(unsafe_arguments(Unsafe)).


:- dynamic memo/3.
:- dynamic bin_solve_fact/1, bin_solve_clause/1.

%:- dynamic flat_clause/2.
%:- dynamic decrease/2, unsafe/2.


:- use_module(annfile,[load_annfile/1,filter/2]).

compile_global_bta :-
	save_program('global_bta.sav').

%% Comment this line for debug mode


set_convex_norm(Norm) :-	
	retractall(convex_norm(_)),
	assert(convex_norm(Norm)).


%debug_format(_,_) :- !.
debug_format(S,F) :-
	format(user_error,S,F).
	

get_pred_info(ResNoPP, Res,Orig) :-
	nonvar(ResNoPP),
	!,
	ResNoPP =.. [F|Args],
	Res =.. [F,PP|Args],
	memo(memo_solve_atom(PP,Orig,_),Res,_).
get_pred_info(ResNoPP,Res,Orig) :-
	nonvar(Res),
	!,
	Res =.. [F,PP|Args],
	ResNoPP =.. [F|Args],
	memo(memo_solve_atom(PP,Orig,_),Res,_).



run_convex(Ann, Output) :-
	tell(Output),
	run_convex(Ann),
	told.

run_convex(Ann) :-
	run_convex('../examples/bin_memo.spec', '../examples/bin_memo.memo',Ann).


load_only_facts :-
	bin_solve_fact(F),
    %portray_clause(user_error,F),
	remember_clause(F),
	fail.
load_only_facts :-
	format(user_error, "Finished loading facts~n",[]).

load_only_clauses :-
	bin_solve_clause(C),
    %portray_clause(user_error,C),	
	remember_clause(C),
	fail.
load_only_clauses.




%% Run the convex hull on a Spec and Memo bin_solve program
run_convex(Spec,Memo,Ann) :-
	convex_norm(Norm),
	format(user_error,"Using norm ~w\n",[Norm]),
	reset_bin_db,
	reset_convex,	
	see(Memo),load_memo,seen,
	see(Spec),load_spec,seen,	
	load_filters(Ann),
	load_only_facts,
	load_only_clauses,
	convex,
	findall(PP,unsafe_pp(PP), Unsafe),
	portray_clause(not_safe(Unsafe)).

	%load_only_facts,
	%convex,
	%(unsafe_pp(_) ->	    
	%    true, % early termination on fact only checking....
	%    format(user_error,"~nEarly termination...~n",[])
%	
	%;
	%    (
	%      reset_convex,	      
	%      load_only_facts,
	%      load_only_clauses,
	%      convex,
	%      format(user_error,"~nFull Check performed...~n",[])
	%    
	%    )
	%),
	%findall(PP,unsafe_pp(PP), Unsafe),				
	%rebuild_ann(Ann,Unsafe).

	   

load_filters(Ann) :-
	annfile:load_annfile(Ann).
	

print_convex_clause_db :-
	my_clause(A,B),
	format(user_error,"~@",[portray_clause((A:-B))]),
	fail ; true.

convex :-
	iterate(1),
	
	showfacts, %% prints output to stdout
	%print_convex_clause_db,
	check_for_decrease.
%%%%%	test_facts,
	%remove_decrease.
	%true.

%%
%% Load the memo table from the .memo file
%%
load_memo :-
	read_term(Term,[]),
	(Term == end_of_file ->
	    true
	;
	    (
	      assert_memo(Term),
	      load_memo
	    )
	).

assert_memo(table(CP,RC,Pref)) :-
	!,
	assert(memo(CP,RC,Pref)).
assert_memo(_).

%% 
%% Load the bin_solve clauses from the .spec file
%%
load_spec :-
	read_term(Term,[]),
	(Term == end_of_file ->
	    true
	;
	    ( 
	      assert_spec(Term),
	      load_spec
	    )
	).


%%
%% Not using via at the moment
%% 
remove_via(A,A).

%remove_via((via(_),B),B).
%#remove_via(via(_),true).


%% We remove the pp information as it gets in the way of the abstraction....
remove_pp_info((H:-B),(NH:-NB)) :-
        get_pred_info(NH,H,_),
	remove_pp_body(B,NB).
remove_pp_info(H,NH) :-
	get_pred_info(NH,H,_).

remove_pp_body((A,B),(NA,NB)) :-
	!,
	remove_pp_body(A,NA),
	remove_pp_body(B,NB).
remove_pp_body(A,NA) :-
	get_pred_info(NA,A,_),
	!.
%% Allow non bin_clause_atom predicates to remain untouched
remove_pp_body(A,A).


	

%%% Remember the clause *only* if they are bin_clause_atoms from memo table
%%% oops or if they are used in bin_solve clauses
assert_spec(':-'(H,B)) :-
        memo(memo_solve_atom(_,_,_),H,_),	
	!,
	copy_term((H,B),(CH,CB)),
	%remove_via(CB,NB),
	%get_pred_info(H_NO_PP,CH, Orig),
	remove_pp_info((CH:-CB), (H_NO_PP:-NB)),
	%portray_clause(	get_pred_info(H_NO_PP,CH, Orig)),
	(NB == true ->
	    assert(bin_solve_fact(H_NO_PP))
	    %remember_clause(H_NO_PP)
	;
	    assert(bin_solve_clause(':-'(H_NO_PP,NB)))
	    %remember_clause(':-'(H_NO_PP,NB))
	).

assert_spec(':-'(_)) :- !.

%% this will only catch bin_solve as remove_pp will fail else
assert_spec(Head) :-
       %portray_clause(Term),
	
       remove_pp_info(Head,NH),
       !,
       assert(bin_solve_fact(NH)).
       %remember_clause(NH).
%% This will catch all non bin_solve clauses??
assert_spec(Term) :-
	assert(bin_solve_clause(Term)).
	%remember_clause(Term).
        
%assert_spec(':-'(H,B)) :-
%        !,
%	assert(flat_clause(H,B)).
%assert_spec(H) :-
%	!,
%	assert(flat_clause(H,true)).



%% Load up ann file and print out with safe annotations


	

%%% Reset all dynamic clauses
reset_bin_db :-
	retractall(memo(_,_,_)),
	retractall(flat_clause(_,_)),
	retractall(bin_solve_fact(_)),
	retractall(bin_solve_clause(_)).

	
reset_convex :-	
	%retractall(unsafe(_,_)),
	%retractall(decrease(_,_)),
	retractall(my_clause(_,_)),
	retractall(fact((_:-_))),
	retractall(is_unsafe(_)).
	

check_for_decrease :-
	findall((FPP:-C),(fact((F:-C)),get_pred_info(F,FPP,_)),Facts),
        %portray_clause(facts(Facts)),	
	check_for_decrease(Facts).

check_for_decrease([]).
check_for_decrease([(H:-CLPList)|Fs]) :-
      convert_to_clp(CLPList,CLP),
      %findall((H,Check),tests(H,Check),Checks),
      tests(H,Checks),
      debug_format("%% Valid checks for ~@",[portray_clause(H:Checks)]),
      (Checks == [] ->
	  debug_format("WARNING NO VALID CHECKS FOR ~w~n",[H])
      ;
	  true
      ),
      
      check_conditions(Checks,CLP,H),
      check_for_decrease(Fs).


:- dynamic is_unsafe/1.

decrease(H,_CLP,[]) :-
	debug_format("%%~@ %%  marked unsafe~n",[portray_clause(H)]),
	assert(is_unsafe(H)).

decrease(H,CLP,[(H,Check)|Cs]) :-
	debug_format("%%~@",[(get_pred_info(_,H,Orig),portray_clause(decrease(Orig:H,CLP,Check)))]),
	((call_residue((call(CLP),entailed(Check)),_R)) ->
	    %true %% We have valid decrease so stop checking this fact
	    debug_format("%%   --> SAFE~n",[])	    
	;
	    debug_format("%%   --> *NOT* SAFE~n",[]),
	    decrease(H,CLP,Cs)	    
	).
	
	
/* Condition checking logic... */
% any and all have references to identify failure...
check_conditions(or(A,_), CLP, H) :- %portray_clause(or(A,B)),
	check_conditions(A, CLP, H),
	!.

check_conditions(or(_,B), CLP, H) :- %portray_clause(or(A,B)),
	!,
	check_conditions(B, CLP, H).

check_conditions(any(nop,[]), _,_) :- !, fail.
check_conditions(any(Ref,[]), _,_) :- !, portray_clause(failed(Ref)),assert(is_unsafe(Ref)).%,fail.
check_conditions(any(Ref,[C|Cs]), CLP,H) :-
	!,
	((check_conditions(C, CLP,H),!);check_conditions(any(Ref,Cs),CLP,H)).


check_conditions(all(_Ref,[]), _,_) :- !.
check_conditions(all(Ref,[C|Cs]), CLP,H) :-
	!,
	check_conditions(C, CLP,H),
	check_conditions(all(Ref,Cs), CLP,H).

check_conditions(fail, _, _) :- !,fail.
check_conditions(C, CLP,H) :-
	portray_clause(checking(C,H)),
	(call_residue((call(CLP),entailed(C)),_R) ->
	    portray_clause(ok)
	;
	    portray_clause(fail),
	    fail
	).



	
	
	

%%% test_facts :-
%%% 	fact((Head:-CLPList)),
%%% 	convert_to_clp(CLPList,CLP),
%%% %	portray_clause((Head:-CLP)),
%%% 	decrease_test(Head,CLP),
%%% 	fail.
%%% test_facts.


convert_to_clp([],true) :- !.
convert_to_clp(H, {C}) :- convert_to_clp1(H,C).
convert_to_clp1([CLP],CCLP) :- clp_clean(CLP,CCLP).
convert_to_clp1([C|Cs],(CC,Rest)) :- clp_clean(C,CC),convert_to_clp1(Cs,Rest).

%%% silly clp engine outputs dump/3 in format it cannot parse!
clp_clean(C,CC) :-
	
	C =.. [OP,LHS,ZERO],
	ground(ZERO),
	ZERO = -0.0,
	CC =.. [OP,LHS,0],!.
clp_clean(C,C).




test_db(Call,A) :-
	%findall((F,F1), annfile:filter(F,F1), All),portray_clause(All),
	copy_term(Call,CC),
	functor(CC,_F,Arity),
	count(1,Arity,A).
	%portray_clause(annfile:filter(CC,Fil)),
	%annfile:filter(CC,Fil),
	%get_rigid_args(Fil,1,A).


count(X,M,R) :-
	X =< M,
	((X = R)
	;
	(X1 is X + 1,
	 count(X1,M,R))).




get_rigid_args([static|_], A,A).
get_rigid_args([type(list(dynamic))|_], A,A) :- convex_norm(list).
get_rigid_args([type(list(static))|_], A,A).

get_rigid_args([_|Fs], A,R) :-
	A1 is A +1,
	get_rigid_args(Fs,A1,R).


/* So we need to test:
   strict decrease Ai -> Bi i:1-> n
   eq-or-decrease Ai -> Bj  i:1-> n, j: 1-> n
   ....which is n^2+n...
   */

test_strict(H,any(nop,C)) :-
	findall((H,C), test_strict1(H,C),ChecksH),
	unify_tests(ChecksH,H,C).

test_strict1(Head,Check) :-
	functor(Head,F,A),
	functor(Copy,F,A),	
	memo(memo_solve_atom(_PP,Call,Loop), Copy,_Prop),	
	count(1,A,ArgTest1),	
	%test_db(Call, ArgTest),	
	arg(ArgTest1,Call, Lhs),
	arg(ArgTest1,Loop, Rhs),
	Check = (Lhs>Rhs),
	Copy =.. [Func, _|Args],
	Head =.. [Func,_|Args].


/* want ForAll j:1>=j>=n @ (Exists i: 1>=i>=n @ Ai >= Bj) */
/* ideally wants sets of tests, one for each Bj, then it must pass
   at least one from each set*/
test_eqless(Head,all(eqless,Checks)) :-
	findall((Head,Check), test_eqless1(Head,Check),ChecksH),
	unify_tests(ChecksH,Head,Checks).

test_eqless1(Head,any(Func/Arity:J,Checks)) :-
	functor(Head,F,A),
	functor(Copy,F,A),
	memo(memo_solve_atom(_PP,Call,Loop), Copy,_Prop),
	functor(Call,Func,Arity),	

	count(1,Arity,J),
	% do findall here to get set?
	findall((Head,Check),test_eqless2(Head,Check,J), ChecksH),
	unify_tests(ChecksH,Head,Checks).

unify_tests([],_,[]).
unify_tests([(Head,Check)|T],Head,[Check|Cs]) :- unify_tests(T,Head,Cs).


test_eqless2(Head,Check, ArgTestJ) :-		
	functor(Head,F,A),
	functor(Copy,F,A),	
	memo(memo_solve_atom(_PP,Call,Loop), Copy,_Prop),	
	count(1,A,ArgTestI),
	%test_db(Call, ArgTest),	
	arg(ArgTestI,Call, Lhs),
	arg(ArgTestJ,Loop, Rhs),
	Check = (Lhs>=Rhs),
	Copy =.. [Func, _|Args],
	Head =.. [Func,_|Args].

   
tests(Head, or(CheckStrict, CheckEqLess)) :-
	      test_strict(Head,CheckStrict),test_eqless(Head,CheckEqLess).




%%% decrease_test(Head,CLP) :-
%%% 	tests(Head, Check),
%%% 	((call_residue((call(CLP),entailed(Check)),_R)) ->
	    
%%% 	  assert(decrease(Head,Check))
	
%%% 	;
%%% 	    (unsafe(Head,_) ->
%%% 		true
%%% 	    ;
%%% 		assert(unsafe(Head,Check))
%%% 	    )
%%% ),
%%% 	fail.

%%% decrease_test(_Head,_CLP).

%%% remove_decrease :-
%%% 	decrease(H,_),
%%% 	retractall(unsafe(H,_)),
%%% 	fail.
%%% remove_decrease.


unsafe_pp(PP) :- is_unsafe(PP).


%unsafe_pp(PP) :-	
	 %unsafe(H,_),
%	 is_unsafe(H),
%	 copy_term(H,NH),
%	 memo(Call,NH,_),
%	 arg(1,Call,PP).

%%% print_unsafe :-
%%% 	unsafe_pp(PP),
%%% 	portray_clause(unsafe(PP)),
%%% 	fail.
%%% print_unsafe.





	
	
	


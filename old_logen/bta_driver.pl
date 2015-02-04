:- module(bta_driver,[run_convex/2,unsafe_pp/1,set_convex_norm/1,test_bta_driver/1,showfacts/1]).



%:- op(1150, fx, type).

%:- use_module('convex/input.pl',[load_file/1, my_clause/2, remember_clause/1]).
%:- use_module('convex/db.pl',[fact/1 ]).
%:- use_module('convex/analyse.pl', [iterate/1]).


%:- include('../examples/bin_loader.pl').
%:- use_module('../examples/bin_loader.pl',[rebuild_ann/2]).


:- use_module('./bta_files/bin_loader.pl',[rebuild_ann/3]).
:- use_module(library(clpq)).




:- dynamic memo/3.
:- dynamic bin_solve_fact/1, bin_solve_clause/1.




:- ensure_loaded('sicstus.pl').
:- use_module(annfile,[load_annfile/1,filter/2]).
:- use_module('cogen-tools', [application_path/1]).


%% New Convex Hull Code:
:- use_module(convex_analyser,[iterate/1, convex_fact/1, showfacts/1,
			       remember_clause/1,reset_convex/0,reset_convex_after_facts_only/0]).
:- use_module(convex_norm,[set_convex_norm/1,convex_norm/1]).

debug_format(_,_) :- !.
debug_format(S,F) :-
	format(user_error,S,F).

represents_loop(ResNoPP, Res) :-
	get_pred_info(ResNoPP, Res, Orig, Loop),
	functor(Loop,F,A),
	functor(Orig,F,A).

		
get_pred_info(ResNoPP, Res,Orig) :-
	get_pred_info(ResNoPP, Res,Orig,_).
	
get_pred_info(ResNoPP, Res,Orig,Loop) :-
	nonvar(ResNoPP),
	!,
	ResNoPP =.. [F|Args],
	Res =.. [F,PP|Args],
	
	memo(bin_solve_atom(PP,Orig,Loop),Res,_).


get_pred_info(ResNoPP,Res,Orig,Loop) :-
	nonvar(Res),
	!,
	Res =.. [F,PP|Args],
	ResNoPP =.. [F|Args],
	memo(bin_solve_atom(PP,Orig,Loop),Res,_).



run_convex(Ann, Output) :-	
	open(Output,write,OS),
	%portray_clause(user_error, opened_convex_stream(Output)),
	run_convex1(OS,Ann),
	close(OS).



run_convex1(OS,Ann) :-	
	application_path(Dir),
	atom_concat(Dir, '/bta_files/bin_via.spec', BINSPEC),
	atom_concat(Dir, '/bta_files/bin_via.memo', BINMEMO),	
	run_convex1(OS,BINSPEC, BINMEMO,Ann).
	

load_only_facts :-
	bin_solve_fact(F),
        debug_format("~@",[portray_clause(user_error,facts(F))]),
	remember_clause(F),
	fail.
load_only_facts :-
	debug_format( "Finished loading facts~n",[]).

load_only_clauses :-
	bin_solve_clause(C),
    %portray_clause(user_error,C),	
	remember_clause(C),
	fail.
load_only_clauses.




run_convex_for_norm(Norm,Spec,Memo,Ann,Unsafe) :-
	set_convex_norm(Norm),
	debug_format("Using norm ~w\n",[Norm]),

	%% RESET STUFF
	reset_bin_db,
	reset_bta,

	%% Read in memo
	open(Memo,read,MemoS),
	load_memo(MemoS),
	close(MemoS),

	%% Read in Spec 
	open(Spec,read,SpecS),	
	load_spec(SpecS),
	close(SpecS),	
	load_filters(Ann),			
	convex,

	debug_format("~nChecking for Early Termination...~n",[]),
	(unsafe_pp(_) ->	    
	    true, % early termination on fact only checking....
	    debug_format("~nEarly termination~n",[])
	
	;
	    (
	      debug_format("~nDoing Full Check (No Early)~n",[]),
	      reset_convex_after_facts_only,
	      load_only_clauses,
	      convex,
	      debug_format("~nFull Check performed...~n",[])
	    
	    )
	),
	findall(PP,unsafe_pp(PP), Unsafe).


:- use_module(library(lists)).

intersect_list([],_,[]).
intersect_list([A|As], Bs, Intersect) :-
	(member(A,Bs) ->
	    Intersect = [A|Is]
	;
	    Intersect = Is
	),
	intersect_list(As,Bs,Is).



	
assert_new_unsafe([]).
assert_new_unsafe([PP|Us]) :-
	memo(Call,U,_),
	arg(1,Call,PP),		
	assert(is_unsafe(U)),
	assert_new_unsafe(Us).



run_convex1(OS,Spec,Memo,Ann) :-
	convex_norm(Norm),
	%portray_clause(i_have_norm(Norm)),
	!,
	(Norm == both ->
	    
	    run_convex_for_norm(list,Spec,Memo,Ann,UnsafeList),
	    run_convex_for_norm(term,Spec,Memo,Ann,UnsafeTerm),
	    intersect_list(UnsafeTerm,UnsafeList,Unsafe),
	    %Unsafe = UnsafeTerm,
	    retractall(is_unsafe(_)),
	    assert_new_unsafe(Unsafe),
	    %portray_clause(unsafe(intersect_list(UnsafeTerm,UnsafeList,Unsafe))),
	    set_convex_norm(both)
	
	;	    	  	    
	    run_convex_for_norm(Norm,Spec,Memo,Ann,Unsafe)
	),
	%portray_clause(unsafe_points(Unsafe)),
	rebuild_ann(OS,Ann,Unsafe).

	

%% Run the convex hull on a Spec and Memo bin_solve program
/*
run_convex1(OS,Spec,Memo,Ann) :-
	%build_depends_table(Ann),
	convex_norm(Norm),
	debug_format("Using norm ~w\n",[Norm]),
	reset_bin_db,
	reset_bta,
	

	open(Memo,read,MemoS),
	load_memo(MemoS),
	close(MemoS),


	open(Spec,read,SpecS),	
	load_spec(SpecS),
	close(SpecS),
	
	load_filters(Ann),
			
	%% STEVE NOW STORE FACTS IN CONVEX IMEDIATELY SEE LOAD SPEC
	%%load_only_facts,		
	convex,

	%%% DEBUG
	debug_format("~nChecking for Early Termination...~n",[]),
	%showfacts(user_error),
	%%% 
	(unsafe_pp(_) ->	    
	    true, % early termination on fact only checking....
	    debug_format("~nEarly termination~n",[])
	
	;
	    (
	      debug_format("~nDoing Full Check (No Early)~n",[]),
	      %reset_bta, 	      
	      %load_only_facts,
	      reset_convex_after_facts_only,
	      load_only_clauses,
	      convex,
	      debug_format("~nFull Check performed...~n",[])
	    
	    )
	),
	findall(PP,unsafe_pp(PP), Unsafe),

	%CACHE WORK IN PROGRESS
	%save_convex_cache(Unsafe),
	%portray_clause(user_error, calling_rebuild(OS,Ann,Unsafe)),
	rebuild_ann(OS,Ann,Unsafe).

*/

load_filters(Ann) :-
	%annfile:load_annfile(Ann).
	load_annfile(Ann).
	
	

print_convex_clause_db :-
	my_clause(A,B),
	debug_format("~@",[portray_clause((A:-B))]),
	fail ; true.

convex :- convex(1).

convex(I) :-
	iterate(I),	
	debug_format("~@",showfacts(user_output)), %% prints output to stdout
	%print_convex_clause_db,
	check_for_decrease.

%%
%% Load the memo table from the .memo file
%%

load_memo(S) :-
	read_term(S,Term,[]),
	(Term == end_of_file ->
	    true
	;
	    
	    (
	      assert_memo(Term),
	      load_memo(S)
	    )
	).


assert_memo(table(CP,RC,Pref)) :-
	!,
	assert(memo(CP,RC,Pref)).

%assert_memo(table(bin_solve_atom(CPP,CPC1,CPC2),RC,_Pref)) :-
%	!,	
%	assert(memo(bin_solve_atom(CPP,CPC1,CPC2),RC,[])).
assert_memo(_).


%% 
%% Load the bin_solve clauses from the .spec file
%%
load_spec(S) :-
	read_term(S,Term,[]),
	(Term == end_of_file ->
	    true
	;
	    ( 
	      assert_spec(Term),
	      load_spec(S)
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
%assert_spec(':-'(H,fail)) :-
        %portray_clause(user_error, discarding(H)),
%        !.
	
assert_spec(':-'(_)) :- !.
assert_spec(':-'(H,B)) :-
	% need to test if we are a valid bin_solve_atom: which is best?
	%functor(H,FUNCTOR,_),atom_concat(bin_solve_atom,_,FUNCTOR), %or
	memo(bin_solve_atom(_,_,_),H,_),	
	!,

	  
	  
	%%% DO WE REALLY NEED THIS COPY???
	%copy_term((H,B),(CH,CB)),
	%remove_pp_info((CH:-CB), (H_NO_PP:-NB)),
	  
	remove_pp_info((H:-B), (H_NO_PP:-NB)),
	(is_not_recursive(NB) ->		    
	    (NB == true ->
		
		% STEVE STORE IMEDIATELY
		%assert(bin_solve_fact(H_NO_PP))
		%debug_format("~nRemembering Fact:~w~n", H_NO_PP),
		remember_clause(H_NO_PP)
	    ;
		(
		  % STEVE STORE IMEDIATELY
		  %assert(bin_solve_fact(':-'(H_NO_PP,NB)))
		  %debug_format("~nRemembering Fact:~w~n", (H_NO_PP:-NB)),		  
		  remember_clause(':-'(H_NO_PP,NB))
		)
	    )	    
	  
	;
	   %% Recursive bin_solve program, we should not use
	    % for early termination testing
	    assert(bin_solve_clause(':-'(H_NO_PP,NB)))	  
	)
	.



%% this will only catch bin_solve as remove_pp will fail else
assert_spec(Head) :-
       %portray_clause(Term),
	
       remove_pp_info(Head,NH),
       !,
       
       %debug_format("~nRemembering Fact:~w~n", (NH)),		  
       remember_clause(NH).



       %remember_clause(NH).
%% This will catch all non bin_solve clauses??

/* Steve refactor update:
this seems to catch lots of solve_file --- not useful
and solve_atom can only call solve atom???
*/

%% ignore all solve_files
assert_spec((solve_file__0:-_)) :-
        !.
assert_spec((solve_file(_):-_)) :-
        !.

assert_spec(Term) :-		
        remember_clause(Term).


is_not_recursive((A,B)) :-
	!,
	is_not_recursive(A),
	is_not_recursive(B).
is_not_recursive(true).


is_not_recursive(A) :-
	functor(A,F,_),
	\+(atom_concat(bin_solve_atom_, _,F)).
	





%% Load up ann file and print out with safe annotations


	

%%% Reset all dynamic clauses
reset_bin_db :-
	retractall(memo(_,_,_)),
	retractall(flat_clause(_,_)),
	retractall(bin_solve_fact(_)),
	retractall(bin_solve_clause(_)).


reset_bta :-
	reset_convex,
	retractall(is_unsafe(_)).

%%% reset_convex :-	
%%% 	%retractall(unsafe(_,_)),
%%% 	%retractall(decrease(_,_)),
%%% 	retractall(my_clause(_,_)),
%%% 	retractall(fact((_:-_))).
	
	

check_for_decrease :-
	findall((FPP:-C),(convex_fact((F:-C)),get_pred_info(F,FPP,_),represents_loop(F,_)),Facts),
        %portray_clause(facts(Facts)),
	% we should only check for a decrease on loops!
	
	check_for_decrease(Facts).

check_for_decrease([]).
/*
check_for_decrease([(H:-CLPList)|Fs]) :-
            debug_format("Is it a loop? ~w~n", [H]),
	    debug_format("%%~@",[(get_pred_info(F,H,Orig,Loop),portray_clause(known_info(F,H,Orig,Loop)))]),
	    (represents_loop(H,_) ->
		debug_format("Its a loop:: ~w~n", [H])
	    ;
		debug_format("Not a loop:: ~w~n", [H])
	    ),
		
	    
	    fail.
*/

check_for_decrease([(H:-CLPList)|Fs]) :-
      convert_to_clp(CLPList,CLP),
      findall((H,Check),tests(H,Check),Checks),
      
      debug_format("%% Valid checks for ~w: ~@",[H,portray_clause(Checks)]),
      (Checks == [] ->
	  debug_format("WARNING NO VALID CHECKS FOR ~w~n",[H])
      ;
	  true
      ),
      decrease(H,CLP,Checks),
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
	
	
	
	
	

%%% test_facts :-
%%% 	fact((Head:-CLPList)),
%%% 	convert_to_clp(CLPList,CLP),
%%% %	portray_clause((Head:-CLP)),
%%% 	decrease_test(Head,CLP),
%%% 	fail.
%%% test_facts.


convert_to_clp([],true) :- !.
convert_to_clp(H, {C}) :- convert_to_clp1(H,C).

convert_to_clp1([CLP], CLP).
convert_to_clp1([C|Cs],(C,Ts)) :- convert_to_clp1(Cs,Ts).


%% STEVE NOTE Think this bug is fixed with new convex hull, will comment out conversion
/*
%%% silly clp engine outputs dump/3 in format it cannot parse!

convert_to_clp1([CLP],CCLP) :- clp_clean(CLP,CCLP).
convert_to_clp1([C|Cs],(CC,Rest)) :- clp_clean(C,CC),convert_to_clp1(Cs,Rest).
clp_clean(C,CC) :-
	
	C =.. [OP,LHS,ZERO],
	ground(ZERO),
	ZERO = -0.0,
	CC =.. [OP,LHS,0],!.
clp_clean(C,C).
*/



test_db(Call,A) :-	
	%copy_term(Call,CC),	
	%annfile:filter(CC,Fil),
	annfile:filter(Call,Fil),
	get_rigid_args(Fil,1,A).


get_rigid_args([static|_], A,A).
%get_rigid_args([type(list(dynamic))|_], A,A) :- convex_norm(list).
get_rigid_args([type(list(_))|_], A,A) :- convex_norm(list).
get_rigid_args([list(_)|_], A,A) :- convex_norm(list).
get_rigid_args([list(dynamic)|_], A,A) :- convex_norm(list).
get_rigid_args([type(list(static))|_], A,A).
get_rigid_args([list(static)|_], A,A).

get_rigid_args([_|Fs], A,R) :-
	A1 is A +1,
	get_rigid_args(Fs,A1,R).

/* Steve refactor, dont think we needed the copy here, it looks safe */
tests(Head, Check) :-
	%functor(Head,F,A),
	%functor(Copy,F,A),
	memo(bin_solve_atom(_PP,Call,Loop), Head,_Prop),
	test_db(Call, ArgTest),
	arg(ArgTest,Call, Lhs),
	arg(ArgTest,Loop, Rhs),
	Check = (Lhs>Rhs).
	%Copy =.. [Func, _|Args],
	%Head =.. [Func,_|Args].
/*
tests(Head, Check) :-
	functor(Head,F,A),
	functor(Copy,F,A),
	memo(bin_solve_atom(_PP,Call,Loop), Copy,_Prop),
	test_db(Call, ArgTest),
	arg(ArgTest,Call, Lhs),
	arg(ArgTest,Loop, Rhs),
	Check = (Lhs>Rhs),
	Copy =.. [Func, _|Args],
	Head =.. [Func,_|Args].
*/


unsafe_pp(PP) :-	
	 %unsafe(H,_),
	 is_unsafe(H),
	 %copy_term(H,NH),
	 %memo(Call,NH,_),
	 memo(Call,H,_),
	 arg(1,Call,PP).


pretty_print_convex(S) :-
	'convex_analyser':current_sol(H,C),
	pretty_print_convex_call(S,H,C),
	fail ; true.


pretty_print_convex_call(S,H,C) :-
	functor(H,F,Arity),
	(atom_concat('bin_solve_atom', _, F) ->
	    NewArity is Arity+1,
	    functor(H1,F,NewArity),
	    H1 =.. [F,_PP | Args],
	    H =.. [F|Args]		
	;
	    functor(H1,F,Arity),
	    H1 =.. [F|Args],
	    H =.. [F|Args]
	
	),	    
	(memo(NewHead,H1,_) ->
	    prettify_constraints(C,NC),
	    portray_clause(S,(NewHead:-NC))
	;
	    prettify_constraints(C,NC),
	    portray_clause(S,unable_to_convert(H1)),
	    portray_clause(S,(H:-NC))
	),!.	
	
prettify_constraints([], []).
prettify_constraints([C|Cs], [NC|NCS]) :-
	prettify_constraints_1(C,NC),
	prettify_constraints(Cs,NCS).


prettify_constraints_1(N,N) :-
	(atom(N);var(N)),
	!.
	
prettify_constraints_1(rat(X,Y),N) :-
	!,
	N is X/Y.


prettify_constraints_1(C,NC) :-
	nonvar(C),
	C =.. [F|Args],
	prettify_constraints(Args,NArgs),
	NC =.. [F|NArgs].


	
	
	
	

	
	    




test_bta_driver(_) :-
	Memo = './bta_files/bin_via.memo',
	open(Memo,read,S),
	load_memo(S),
	close(S).




	
	
	



%%%% chaching stuff=== scrapped
/*
save_convex_cache(Unsafe) :-
	get_unsafe_names(Unsafe, Names),
	portray_clause(user_error,cache_unsafe(Names)),
	remove_changed_cache(Names),
	propagate_changes,
	print_depends,
	'convex_analyser':save_cache.

get_unsafe_names([Unsafe|Us], [F/Arity|Rest]) :-
	memo(bin_solve_atom(Unsafe, Loop, Loop1), _,_),
	functor(Loop, F,Arity),
	functor(Loop1, F,Arity),
	get_unsafe_names(Us,Rest).

get_unsafe_names([],[]).

:- use_module(library(lists)).

:- dynamic changed_flag/0.
has_changed([]).
	
has_changed([Id|_]) :-
	depends_on(Id,_),
	!,fail.
has_changed([_|T]) :-
	has_changed(T).


propagate_changes :-
	depends_on(Id,Depends),
	(has_changed(Depends) ->
	    retract(Id,Depends),
	    portray_clause(user_error,retract1(Id,Depends)),
	    assert(changed_flag)
	
	;
	    true
	),
	fail;
	(changed_flag ->
	    retract_all(changed_flag),
	    propagate_changes
	
	;
	    true
	).
	

remove_changed_cache([]).
remove_changed_cache([Changed|Cs]) :-
	depends_on(Id, Depends),
	(member(Changed, Depends) ->
	    portray_clause(user_error,retract(depends(Id, Depends))),
	    retract(depends_on(Id, Depends))
	;
	    true
	),
	fail;remove_changed_cache(Cs).

	   
	
	

print_depends :-
	depends_on(H,D),
	portray_clause(user_error, depends(H,D)),
	fail ; true.

build_depends_table(File) :-
	open(File, read, In),
	build_depends_table1(In),
	print_depends,
	close(In).
build_depends_table1(In) :-
	read_term(In,Term,[]),
	(Term = end_of_file ->
	    true
	;
	    update_depends(Term),
	    build_depends_table1(In)
	).

:- dynamic depends_on/2.
update_depends((logen(_,Head):-Body)) :-
        !,
	functor(Head,Func,Arity),
	ID = Func/Arity,
        get_depends_list(Body, Depends),
	list_to_ord_set(Depends,OrdSet),
	(depends_on(ID,DependsSet) ->
	    retract(depends_on(ID,DependsSet)),
	    ord_union(DependsSet, OrdSet, NewSet),
	    assert(depends_on(ID,NewSet))
	;
	    assert(depends_on(ID,OrdSet))
	),	   	
	true.
	
update_depends(_).

:- use_module(library(ordsets)).

get_depends_list((logen(_,Call),B), [F/A|Rest]) :-
	!,
	functor(Call,F,A),
	get_depends_list(B,Rest).


get_depends_list(logen(_,A), [F/Arity]) :-
	functor(A,F,Arity).

	*/
	


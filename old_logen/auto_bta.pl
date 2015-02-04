
:- set_prolog_flag( single_var_warnings,off).



:- include('logen_main.pl').

:- use_module(run_binsolve, [run_binsolve/3]).

:- use_module('bta_files/bta_pp', [bta_if_in_pp/2, bta_if_out_pp/2]).

%:- op(1150, fx, type).

:- op(1150, fx, residual).
:- op(1150, fx, filter).
:- op(1150, fx, is_safe).
:- op(1150, fx, table).
:- op(1150, fx, type).
:- op(1150, fx, residual_pred).
:- op(500,yfx,--->).

:- use_module(bta_driver,[run_convex/2,set_convex_norm/1,unsafe_pp/1]).

:- use_module('filter_prop/auto_bta.pl').
:- use_module(library(system)).
:- use_module('filter_prop/makelogenbta.pl',[makelogenbta/3]).
%:- use_module('bta_files/logen_create_types.pl',[get_types_from_file/2]).
:- use_module('filter_prop/logen/logen_create_types.pl',[get_types_from_file/2]).
:- use_module('cogen-tools', [application_path/1]).


%:- use_module(library(gauge)).


:- dynamic print_clause/1.
run_bta_loop_int(File, Ann,Out, Silent) :-
	statistics(runtime,[TT1,_]),
	Int = 'logenbta.tmp',

	build_logenbta(Ann,File, Int),

	run_bta_loop(Ann,Int,Out,Loops),
	statistics(runtime,[TT2,_]),
	TT is TT2 - TT1,
	(Silent = yes -> true ;
        format("~nTotal Time: ~wms ~w iterations~n",[TT,Loops])),
	%show_convex_profile,
	true.


show_convex_profile :-
	prolog_flag(compiling, profiledcode,profiledcode),
	!,
	view([convex_analyser:_,convex_hull:_,convex_norm:_,user:_,library(lists):_]).

show_convex_profile :-
	format(user_error, "PROFILE: Please recompile with profiling turned on to view profile stats~n",[]).


	
	
run_bta_loop(Ann,Int,Out,Loop) :-
	run_bta_once(Ann,Int,Out),
	(unsafe_pp(_) ->
	    run_bta_loop1(Out,Int,Out,Loop1),
	    Loop is Loop1 +1
	
	;
	    Loop=1
	),
	print(user_error,'Finished auto_bta\n').

	
run_bta_loop1(In,Int,Out,Loop) :-
	run_bta_once(In,Int,Out),
	(unsafe_pp(_) ->	    
	    run_bta_loop1(In, Int,Out,Loop1),
	    Loop is Loop1 +1
	;
	    Loop = 1
	).

make_basic_types(AnnFile, TypeFile):-
	atom_concat(AnnFile,'.types', TypeFile),
	open(TypeFile, write, Out),
	get_types_from_file(AnnFile, Out),
	close(Out).
	

build_logenbta(Ann,File,OutFile) :-
	application_path(Dir),
	%atom_concat(Dir, '/bta_files/listtype.pl', LISTTYPE),
	make_basic_types(Ann,TypeFile),
	
	%portray_clause(is_the_same(user_error,'./bta_files/listtype.pl',LISTTYPE)),
	makelogenbta(File,OutFile,TypeFile).
	%makelogenbta(File,OutFile,'./filter_prop/listtype.pl').


run_bta_once_int(File, Ann,Out):-
	Int = 'logenbta.tmp',
	build_logenbta(Ann,File, Int),
	run_bta_once(Ann,Int,Out).

run_bta_once(Ann,Int, Out) :-
	%% Pre processing here?
	PP_TMP_FILE = '__bta_pp.pl',	

	%bta_if_in_pp(Ann, Ann),
	%run_filter_prop(Ann,Int,Out),
	pre_process(Ann, PP_TMP_FILE),	
	run_filter_prop(PP_TMP_FILE,Int,Out),
	
	TmpFile = '__bta.tmp',
	run_bin(Out,TmpFile),
	(file_exists(Out) -> delete_file(Out); true),	
	rename_file(TmpFile, Out),
	%% Post processing here
	post_process(Out, Out),
	true.


pre_process(In,Out) :-
	bta_if_in_pp(In, Out).


post_process(In,Out) :-
	bta_if_out_pp(In, Out).
	

	

run_filter_prop_int(File,Ann,Out) :-
	Int = 'logenbta.tmp',
	build_logenbta(Ann,File, Int),
	run_filter_prop(Ann,Int,Out).


run_filter_prop(Ann,Int,Out) :-
	statistics(runtime,[Time1,_]),
	TmpFile = '_filter.tmp',
	auto_bta:filter_prop(Ann,Int,TmpFile),

	
	load_new_ann(Ann,TmpFile),	
	
	open(Out, write,OutStream),
	print_all_clauses(OutStream),
	close(OutStream),
	statistics(runtime, [Time2,_]),
	Time is Time2 - Time1,
	format(user_error,"Filter Propagation Time ~wms~n", [Time]).



%%% Run the binsolve+convex only, so we should run pre/post process
run_bin_convex_only(In,Out) :-
	TmpFile = '_bta_pp.pl.ann',
	pre_process(In,TmpFile),
	run_bin(TmpFile,Out),
	post_process(Out,Out).

run_bin(In,Out) :-
	statistics(runtime,[Time1,_]),
	%portray_clause(user_error, run_bin_call(run_binsolve(In))),
	run_binsolve(In),
	statistics(runtime,[Time2,_]),
	%portray_clause(user_error, run_bin_call(run_convex(In,Out))),
	run_convex(In,Out),
	%portray_clause(user_error, run_bin_exit(run_convex(In,Out))),
	statistics(runtime,[Time3,_]),
	TimeBin is Time2-Time1,
	TimeCon is Time3-Time2,
	Time is Time3-Time1,
	format(user_error,"Termination Analysis Time ~wms (Bin ~wms, Convex ~wms)~n", [Time,TimeBin,TimeCon]).

	

compile_bta :-
	save_program('bta.sav').

print_all_clauses(S) :-
	print_clause(C),
	%portray_clause(S,C),
	write_term(S,C,[quoted(true),numbervars(true),indented(true),ignore_ops(true)]),write(S,'.'), nl(S),
	fail.
print_all_clauses(_).


:- dynamic unsafe_builtin/3.

load_new_ann(Ann,Filters) :-
	format(user_error,"~nLoad new Ann~n", []),
	%portray_clause(loading_new_ann),
	retractall(unsafe_builtin(_,_,_)),
	retractall(print_clause(_)),


		
	
	open(Filters,read,FilterStream),
	%portray_clause(started_reading_filter),
	load_filters(FilterStream),
	%portray_clause(finished_reading_filter),
	close(FilterStream),
	open(Ann,read,AnnStream),
	%load_ann_no_filter(AnnStream),
	load_ann_no_filter(AnnStream,0),
	close(AnnStream).
	

	
	
	



%load_ann_no_filter(AnnStream) :-
load_ann_no_filter(AnnStream, ClauseNumber) :-
	read_term(AnnStream,C,[]),
	(C == end_of_file ->
	    true
	;
	    load_ann(C, ClauseNumber),
	    NextClause is ClauseNumber +1,
	    load_ann_no_filter(AnnStream, NextClause)
	).

remove_unsafe_builtin((C:-B), (C:-NB), Unsafe) :-
          remove_unsafe_builtin_body(B,NB,Unsafe,1).

remove_unsafe_builtin_body((A,B),(NA,NB), Unsafe, Current) :-
	!,
	Next is Current +1,
	remove_unsafe_builtin_literal(A,NA,Unsafe, Current),
	remove_unsafe_builtin_body(B,NB,Unsafe, Next).

remove_unsafe_builtin_body(A, NA, Unsafe, Current) :-
	remove_unsafe_builtin_literal(A,NA,Unsafe, Current).

remove_unsafe_builtin_literal(A,NA, Unsafe, Current) :-
	member(Current, Unsafe),
	!,
	A = logen(_call, Call),
	NA = logen(rescall, Call).

remove_unsafe_builtin_literal(A,A, _,_).



		      
	
	

%%% Check if the built in is marked unsafe, if so change it
load_ann(C, No) :-
	%portray_clause(user_error, loading(C,No)),
	unsafe_builtin(_,No,Lit),
	%portray_clause(user_error,unsafe(Lit,C)),
	findall(Item, unsafe_builtin(_,No,Item), Unsafe),

	portray_clause(user_error, Unsafe),
	
	remove_unsafe_builtin(C,NC, Unsafe),
	%portray_clause(user_error, (C,NC)),
	%portray_clause(new_clause(NC)),
	%fail,
	assert(print_clause(NC)),
	!.
	
	


load_ann(':-'(filter(_)),_) :- !.
load_ann(':-'(type(_)),_) :- !.
load_ann(C,_) :- %portray_clause(C).
	assert(print_clause(C)).

load_filters(S) :-
	%portray_clause(reading_filter),
	read_term(S,C,[]),
	(C == end_of_file ->
	    true
	;
	    %% it is either a filter, or a warning about builtins
	    (C = invalidCall(F/A,Clause,Literal) ->
		(unsafe_builtin(F/A, Clause, Literal) ->
		    true
		;
		    %portray_clause(user_error, found(unsafe_builtin(F/A, Clause, Literal))),
		    assert(unsafe_builtin(F/A, Clause, Literal))
		)
	    ;
		convert_list_type(C,F),
		assert(print_clause(F))
	    ),
			      	    
	    %portray_clause(user_error,debug_f(F)),
	    %assert(print_clause(C)),

	    load_filters(S)

	).

convert_list_type((:-filter(C)),(:-filter(NC))) :-
            C =.. [F|Args],
	    convert_list_type_l(Args,NArgs),
	    NC =.. [F|NArgs].
convert_list_type(F,F).


convert_list_type_l([],[]).
convert_list_type_l([list(dynamic)|Ts],[(type(list(dynamic)))|Xs] ) :-
	!,
	convert_list_type_l(Ts,Xs).
convert_list_type_l([T|Ts],[T|Xs]) :- convert_list_type_l(Ts,Xs).



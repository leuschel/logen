:- module('logen_post',
	  
	  [clone_all/0,
	   clone_decl/0,
	   clone_memo/0,
	   clone_spec/0,
	   current_spec/1,
	   current_memo/1,
	   
	   
	   map_pp/2,
	   memo_clause/1,


	   reset_pp/0,
	   run_pp/3,
	   spec_clause/1,

	   save_file/0,
	   save_file/2,	   
	   save_spec_clause/1,
	   save_memo_clause/1,
	   save_memo_filename/1,
	   save_spec_filename/1,

	   shift_reset/1,
	   test_pp/0
	   ]).

:- use_module(library(lists)).

:- dynamic memo_clause/1.
:- dynamic spec_clause/1.

:- dynamic out_memo_clause/1.
:- dynamic out_spec_clause/1.

:- dynamic current_memo/1.
:- dynamic current_spec/1.

save_spec_filename(File) :-
	retractall(current_spec(_)),
	assert(current_spec(File)).
save_memo_filename(File) :-
	retractall(current_memo(_)),
	assert(current_memo(File)).


%% simple postprocessor makes exact copy..
clone_all :-
	clone_spec,
	clone_memo.
	
clone_spec :- 
	map_pp(spec_clause(C), save_spec_clause(C)).	
clone_memo :-
	map_pp(memo_clause(C), save_memo_clause(C)).
clone_decl :-
	map_pp(spec_clause(':-'(C)),save_spec_clause(':-'(C))).


%% Copies the input back to the output ready for next PP
shift_reset :-
	%% Remove input clauses
	retractall(spec_clause(_)),
	retractall(memo_clause(_)),
	%% Copy Out to input
	map_pp(out_spec_clause(C), assert(spec_clause(C))),
	map_pp(out_memo_clause(C), assert(memo_clause(C))),
	%% Remove old output
	retractall(out_spec_clause(_)),
	retractall(out_memo_clause(_)).	
	


%% Generic mapper function
map_pp(Pattern, Call) :-
	Pattern,
	Call,
	fail ; true.



save_memo_clause(A) :- assert(out_memo_clause(A)).
save_spec_clause(C) :- assert(out_spec_clause(C)).



reset_pp :-	
	retractall(out_spec_clause(_)),
	retractall(out_memo_clause(_)),
	retractall(spec_clause(_)),
	retractall(memo_clause(_)).



		      

:- use_module('pp/inline.pl',[inline/0]).
:- use_module('pp/deadcode.pl',[deadcode/0]).
:- use_module('pp/loader.pl',[loader_from_file/2]).
:- use_module('pp/saver.pl',[saver_to_file/2,portray_memo/1,portray_spec/1]).


%% At the moment this just runs all the postprocessing, but will be selective
%% in future
run_pp(Spec,Memo, PP) :-
	reset_pp,
	(memberchk(loader, PP) ->
	    portray_clause(user_error, runningpp(loader)),	    
	    loader_from_file(Spec, Memo),shift_reset
	;
	    true
	),
	(memberchk(inline, PP) ->
	    portray_clause(user_error, runningpp(inline)),
	    inline,	shift_reset
	;
	    true
	),
	(memberchk(deadcode, PP) ->
	    portray_clause(user_error, runningpp(deadcode)),	    
	    deadcode,shift_reset
	;
	    true
	),
	(memberchk(saver, PP) ->
	    portray_clause(user_error, runningpp(saver)),	    
	    saver_to_file(Spec,Memo),shift_reset
	;
	    true
	),

	true.
	%portray_memo(user),
	%portray_spec(user).
	


	

test_pp :-
	run_pp('../examples/interpreters/lloyd_topor_op.spec','../examples/interpreters/lloyd_topor_op.memo',
	       [loader,inline,deadcode]),
	portray_memo(user),
	portray_spec(user).
	

	%reset_pp,
	%loader_from_file('./bta_files/tests/inter_simple.spec', './bta_files/tests/inter_simple.memo'),
	%shift_reset,	
	%inline,
	%shift_reset,
	%deadcode,
	%portray_memo(user),
	%portray_spec(user).
	

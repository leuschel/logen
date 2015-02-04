
:- module(bta_pp,[
		  clone_all/0,
		  remove_in_logen_clause/1,
		  remove_out_logen_clause/1,
		  pp_op_decl/0,
		  
		  in_logen_clausedb/1,
		  out_logen_clausedb/1,
		  save_logen_clause/1,
		  test_bta_pp/0,

		  bta_if_in_pp/2,
		  bta_if_out_pp/2
		 ]).


:- dynamic out_logen_clausedb/1.
:- dynamic in_logen_clausedb/1.


pp_op_decl :-
	op(1150, fx, residual),
	op(1150, fx, filter),
	op(1150, fx, is_safe),
	op(1150, fx, table),
	op(1150, fx, type),
	op(1150, fx, residual_pred),
	op(500,yfx,--->).

:- pp_op_decl.

remove_in_logen_clause(Clause) :-
	retract(in_logen_clausedb(Clause)).
remove_out_logen_clause(Clause) :-
	retract(out_logen_clausedb(Clause)).

save_logen_clause(Clause) :-
	assert(out_logen_clausedb(Clause)).

clone_all :-
	in_logen_clausedb(C),
	save_logen_clause(C),
	fail ; true.

shift_reset :-
	retractall(in_logen_clausedb(_)),
	shift,
	retractall(out_logen_clausedb(_)).

shift :-
	out_logen_clausedb(C),
	assert(in_logen_clausedb(C)),
	fail ; true.

reset :-
	retractall(in_logen_clausedb(_)),
	retractall(out_logen_clausedb(_)).
	



:- use_module(pp_iftransform).
:- use_module(pp_annfile).
%bta_if_in_pp(In,In) :- !.	
bta_if_in_pp(InAnnFile,OutAnnFile) :-
	(bta_if_in_pp1(InAnnFile,OutAnnFile) ->
	    portray_clause(user_error, pp_if_in_ok)
	;
	    portray_clause(user_error, pp_if_in_fail),
	    halt
	).
	

bta_if_in_pp1(InAnnFile,OutAnnFile) :-
	reset,
	load_annfile(InAnnFile),
	shift_reset,
	iftransform_in,
	shift_reset,
	save_annfile(OutAnnFile).


%bta_if_out_pp(In,In) :- !.

bta_if_out_pp(In,Out) :-
	portray_clause(user_error, starting_out_pp(if)),
	(bta_if_out_pp1(In,Out) ->
	    portray_clause(user_error, pp_if_out_ok)
	;
	    portray_clause(user_error, pp_if_out_fail),
	    halt
	).	    

bta_if_out_pp1(InAnnFile,OutAnnFile) :-
	reset,
	load_annfile(InAnnFile),
	shift_reset,
	%portray_annfile(user_error),
	iftransform_out,	
	shift_reset,
	%portray_annfile(user_error),
	save_annfile(OutAnnFile).

	

test_bta_pp :-
	reset,
	load_annfile('/home/sjc02r/cvs_root/cogen2/logen_source/bta_files/if.pl.ann'),
	shift_reset,
	iftransform_in,
	shift_reset,
	portray_annfile(user),
	iftransform_out,
	shift_reset,
	portray_annfile(user).


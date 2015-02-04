
:- module(pp_annfile, [load_annfile/1,portray_annfile/1, save_annfile/1]).


:- use_module(bta_pp, [in_logen_clausedb/1, clone_all/0,save_logen_clause/1,pp_op_decl/0]).

save_annfile(AnnFile) :-
	pp_op_decl,
	open(AnnFile, write,Out),
	portray_annfile(Out),
	close(Out).
	%portray_annfile(user_error).


load_annfile(AnnFile):-
	pp_op_decl,
	open(AnnFile,read,In),
	assert_clauses(In),
	close(In).



assert_clauses(In) :-
	read_term(In,Term,[]),
	(Term = end_of_file ->
	    true
	;
	    save_logen_clause(Term),
	    assert_clauses(In)
	).




portray_annfile(Out) :-
	in_logen_clausedb(C),
	portray_clause(Out,C),
        % write_term(Out,C,[quoted(true),ignore_ops(true)]),
        % write_term(Out,'.',[]), nl(Out),
      
	fail ; true.
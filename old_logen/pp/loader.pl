:- module(loader, [loader_from_file/2]).


:- use_module('../logen_post').


load_clauses(In, Term, Call) :-
	copy_term((Term,Call), (CTerm,CCall)),
	read_term(In,CTerm, []),
	(CTerm = end_of_file ->
	    true
	;
	    CCall,
	    load_clauses(In,Term,Call)
	).


loader_from_file(Spec,Memo) :-
	%% load spec file
	save_spec_filename(Spec),
	open(Spec, read, SpecStream),
	load_clauses(SpecStream,SpecClause, save_spec_clause(SpecClause)),
	close(SpecStream),

	%% load memo file
	save_memo_filename(Memo),
	open(Memo,read, MemoStream),
	load_clauses(SpecStream,T, save_memo_clause(T)),
	close(MemoStream).



:- module(build_dispatcher, [build_dispatch/5]).

:- dynamic memo_name/1.
:- dynamic spec_name/1.
:- dynamic orig_name/1.
:- dynamic out_name/1.

:- dynamic import/1, export/1.


	

reset_all :-
	retractall(memo_name(_)),
	retractall(spec_name(_)),
	retractall(orig_name(_)),
	retractall(out_name(_)),	
	retractall(export(_)),
	retractall(import(_)).

build_dispatch(Path,Spec,Memo, File, NewFileName) :-
	reset_all,
	assert(spec_name(Spec)),
	assert(memo_name(Memo)),
	assert(orig_name(File)),
	assert(out_name(NewFileName)),

	atom_concat(Path,File, AbsFile),
	atom_concat(Path,NewFileName, AbsNewFile),
	open(AbsFile,read,In),
	get_module_info(In),
	close(In),
	open(AbsNewFile, write, Out),
	%Out = user,
	write_dispatcher(Out),
	close(Out).
	


build_module_decl((:-module(Module,ExportList))) :-
        %orig_name(Module),
        out_name(Module),
	findall(Export, export(Export),ExportList).

write_hook(Out,Call) :-
	spec_name(Spec),
	Clause = (Call :- (dispatch_helper:call_spec(Call,NewCall,Spec),!,NewCall)),
	portray_clause(Out,Clause),
	fail.

write_hook(Out,Call) :-
	orig_name(Orig),
	Clause = (Call :- (Orig:Call)),
	portray_clause(Out,Clause).
	
	
write_hooks(Out) :-
	export(F/A),
	functor(Call,F,A),
	write_hook(Out,Call),
	fail;true.

write_imports(Out) :-
	portray_clause(Out, (:- use_module(dispatch_helper, []))),
	orig_name(Orig),
	portray_clause(Out, (:- use_module(Orig, []))).

write_reloader(Out) :-
	spec_name(Spec),
	memo_name(Memo),
	Clause = (save_module_names :-(dispatch_helper:save_memo_module(Memo),dispatch_helper:save_spec_module(Spec) )),
	portray_clause(Out,Clause),
	portray_clause(Out,(:-save_module_names,dispatch_helper:reload_spec)).
	
write_dispatcher(Out) :-
	build_module_decl(M),
	portray_clause(Out,M),
	write_imports(Out),
	write_hooks(Out),
	write_reloader(Out).



get_module_info(In) :-
	read_term(In,Term,[]),
	(Term = end_of_file ->
	    true
	;
	    (Term = (:-module(_,Exports)) ->
		save_exports(Exports)
	    ;
		true
	    ),
	    (Term = (:-use_module(Import)) ->
		save_import(Import)
	    ;
		true
	    ),
	    get_module_info(In)
	).

save_import(Import) :-
	assert(import(Import)).

save_exports([]).
save_exports([E|T]) :-
	assert(export(E)),
	save_exports(T).

	
	
	
	
	
	
	
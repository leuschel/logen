%%%%%%%%%%%%%%%%%%%%%%%%%%
% Steve
% Interface to cogen system, provides simple commands to run annotation
% cogen and a gx file to specialise a program
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('cogen-interface', [
			    set_logen_flag/2,
			    annotateFile/3,
			    load_annfile/1,
			    run_cogen/1,
			    run_cogen_and_save_wo_expansion/2,
			    load_gx_wo_expansion/1,
			    atom_ok_to_specialise/1,
			    reset_all/0,
			    run_gx/2,
			    run_time/2,
			    run_gx_and_save_wo_expansion/2,
			    run_all_wo_expansion/2,
			    do_nothing/2,
			    delete_memo_table/1,
			    run_and_save/2
			   ]).

%%%% OLD EXPORTS OBSOLETE
			    %annotate_program/1,
			    %annotate_program_and_save/1,
			    %load_ann/1,
			    %run_cogen/0,			    
			    %run_cogen_and_save/1,
			    %run_cogen_and_save/2,			    
			    %load_gx/1,
			    %run_all/2,
			    %run_gx/1,			    			   			    %load_file/1,
			    %run_gx_and_save/3,
			    

:- use_module('prob/logen_preferences.pl').

ciao(( :- set_prolog_flag(multi_arity_warnings, off))).

%% Ciao-specific libs
:- use_package(.(sicsignore)).
%ciao((:-initialization(print('Loading Cogen-Interface (CIAO)\n')))).
ciao((:- use_module(library(dec10_io)))).
ciao((:- use_module(library(prolog_sys)))).
ciao((:- use_module(library(compiler),[ensure_loaded/1]))).
%ciao((:- use_package(library(clpq)))).
ciao((:- use_module(library(file_utils),[copy_stdout/1]))).

%%% Sicstus Specific libs
%sicstus((:- initialization(print('Loaded Sicstus Version of Logen\n')))).
sicstus((:- use_module(library(clpq)))).

%%% shared modules
:- use_module('annfile.pl').  %% to manage annotated files
:- use_module('bta.pl').      %% to create annotated files
:- use_module('cogen.pl').    %% to run cogen
:- use_module('cogen-tools.pl').   
%:- use_module('memo.pl').     %% to run gx file
%:- use_module('clprmemo.pl').
:- use_module('pp.pl').           %% to print output
:- use_module(library(system)).
:- use_module('flags.pl').
:- use_module('logen_messages.pl').
:- use_module('gensym.pl').

:- use_module('gx_pp.pl').
:- use_module('memo_pp.pl').


ciao(load_annfile(X)) :- annfile:load_annfile(X).
ciao(annotateFile(X,Y,Z)) :- bta:annotateFile(X,Y,Z).
ciao(set_logen_flag(X,Y)) :- flags:set_logen_flag(X,Y).


delete_memo_table(File) :-
	add_extension(File, '.memo', MemoFile),
	add_extension(File, '.spec', SpecFile),
	(file_exists(MemoFile) ->
	    delete_file(MemoFile)
	;
	    true),
	(file_exists(SpecFile) ->
	    delete_file(SpecFile)
	;
	    true
	).


	

%%
% Check the date stamps on the gx file and load memo file if
% it is still valid, if not reset the memo enteries.
%%
load_memo_table_if_needed(File) :-
	add_extension(File, '.memo', MemoFile),
	add_extension(File, '.gx', GxFile),
	((file_exists(MemoFile), file_exists(GxFile)) ->
	    (file_is_newer(GxFile, MemoFile) ->
    		load_memo_table(MemoFile, discard), %% discard internal entries
	        run_and_save_with_ext(
				      (datime(datime(Y,M,D,HH,MM,SS)), format('% File Reset on ~d/~d/~d ~d:~d:~d~n', [D,M,Y,HH,MM,SS])),
				      File, '.spec', _NewFile)
	    %% should blank the .spec file aswell....
	    ;
		load_memo_table(MemoFile, keep)	    
	    );
	    true % file doesnt exist
	).



run_all_wo_expansion(FILE, Spec_Pred) :-
	annfile:load_annfile(FILE),
	run_and_save_with_ext(
			      run_time(run_cogen(GXFILE),CogenTime)
			     ,FILE,'.gx',GXFILE),
	load_gx_wo_expansion(GXFILE),
	run_time(run_gx(Spec_Pred, FILE),SpecTime),
	
	!,nl,nl,	
	add_extension(FILE, '.memo', MemoFile),
	display_file(MemoFile),nl,
	add_extension(FILE, '.spec', SpecFile),
	display_file(SpecFile),
	print_timings('%%Run Cogen', CogenTime),
	print_timings('%%Run GX   ', SpecTime).


run_and_save(Call, Stream) :-
	current_output(OldStream),
	set_output(Stream),
	call(Call),
	flush_output,
	close(Stream),
	set_output(OldStream).	

run_and_save_with_ext(Call,File,Ext,NewFile, Mode) :-
	add_extension(File,Ext,NewFile),
	open(NewFile, Mode, Stream),	
	run_and_save(Call, Stream).

run_and_save_with_ext(Call,File,Ext,NewFile) :-
	run_and_save_with_ext(Call,File,Ext,NewFile, write).


add_extension(File, Ext, NewFile) :-
	string_concatenate(File,Ext,NewFile).


sicstus(display_file(File)) :-
 	seeing(OldInput),
 	see(File),
 	repeat,
 	     read_line(T),
 	     (T \== end_of_file ->
 		 (
 		   name(A,T),
 		   write(A),nl
 		 )
 	     ;
 		 nl),
 	     T == end_of_file,
 	!,   
 	seen,
 	see(OldInput).

ciao(display_file(File)) :- copy_stdout(File).
	
print_timings(Label, Time) :-
	write(Label) , write(' completed in '), write(Time), nl.

	


%%
% Run cogen on the currently loaded annotations
%%
run_cogen(Module) :-
	setprintmode(clpq),  %%% print rational numbers for gx files
	statistics(runtime,_),
	reset_cogen,
	delete_table, %%% clear memo table
	cogen,!,
	flush_cogen(Module),
	statistics(runtime,[_,MSTime]),
	nl,write('/* GX file Generated in '), write(MSTime), write(' ms */'),
	nl.	

run_cogen_and_save_wo_expansion(File,Save) :-
   /* same as above, but not not suffix .ann and do not prefix examples_directory to File */
    tell(Save),
    print_message('Loading annfile:'), print_message(File),
    annfile:load_annfile(File),
    print_ann_clause_status,
    print_message('Running cogen to generate:'), print_message(Save),
    run_cogen(Save),  %%% use gx file name as module name for now
    told.



sicstus(load_gx_wo_expansion(File)) :-
	prolog_flag(single_var_warnings, Old, off),
	prolog_flag(redefine_warnings,OldR,off),
	ensure_loaded(File),
	set_prolog_flag(single_var_warnings, Old),
	set_prolog_flag(redefine_warnings, OldR).

ciao(load_gx_wo_expansion(File)) :-
 	ensure_loaded(File).


atom_ok_to_specialise(Atom) :- annfile:residual(Atom).



reset_all :-
	pp:reset_pp,
	reset_gxpp,
	reset_memopp,
	reset_cogen,
	reset_gennum,
	delete_table, %% reset the memo table
%	delete_table_clp,
	flags:reset_to_defaults.	

run_gx(Atom, File) :-
	setprintmode(clpr),
	copy_term(Atom, CAtom),
	(annfile:residual(CAtom) -> true ; (print(CAtom), print(' not residual!'), nl, fail)),
	reset_all,

	(get_preference(persistent_memo_table_for_gx, true) ->
	    load_memo_table_if_needed(File)
	;
	    run_and_save_with_ext(
				  (datime(datime(Y,M,D,HH,MM,SS)),
				   format('% Clear_memo flag is true,File Reset on ~d/~d/~d ~d:~d:~d~n', [D,M,Y,HH,MM,SS])),
				      File, '.spec', _NewFile)	    
	),
	add_extra_argument("_request", CAtom, user, RC),
	add_extra_argument("", RC, _V, RequestCall),
	call(RequestCall),

%% Must tell the memo module what module we are dealing with	
	add_extension(File, '.gx', GXFile),
	add_extension(File, '.spec', CodeFile),

	memo:service_all_requests(GXFile),

	%% open spec code file
	memo:print_spec_file_header(MemoFile,CodeFile),	
	assert_failing_predicate_declarations,
	run_and_save_with_ext(
%			      flush_pp,
			      flush_pp_external('gx_pp.pl',[gx_clause(_,_)]),
			      File, '.spec', CodeFile,append
			     ),

	
	run_and_save_with_ext(flush_pp_external('memo_pp.pl', [memo_decl(_),memo_clause(_,_)]), File, '.memo', MemoFile).
				   

	%% open header file
	
	%run_and_save_with_ext( (memo:print_spec_file_header(MemoFile,CodeFile)), File, '.memo', MemoFile).	


run_gx_and_save_wo_expansion(Atom, Save) :-
    print('Starting partial evaluation for: '), print(Atom),nl,
%	tell(Save),
	statistics(runtime,[_,_]),
	run_gx(Atom, Save),
	statistics(runtime,[_,Time]),
%	told,
	print('Finished partial evaluation in: '), print(Time), print(' ms'),nl.
	
	
do_nothing(Atom, Save) :- tell(Save),
  print('done nothing'),nl,
  told,
  print('done nothing:'), print(Atom),
  print(' '), print(Save),nl,nl.


%%
% Simple benchmarking routine to time invocation
%%
run_time(Call, Time) :-
	statistics(runtime,_),
	call(Call),
	statistics(runtime, [_,Time]).


%%
% Run a gx file
%%
%run_gx(File, Atom) :-
%	load_ann(File),
%	load_gx(File),
%	run_gx(Atom).


%%
% Run a gx file and save
%%
%run_gx_and_save(File, Atom, Save) :-
%	make_abs_example_filename(File, '.pe', Save),
%	tell(Save),
%	run_gx(File, Atom),
%	told.





%%
% Helper, append the extension and add it too the examples path to make abs filename
%%
%make_abs_example_filename(File, Extension, AbsFile) :-
%	string_concatenate(File, Extension, Annfile),
%	logen_examples_directory(Path),
%	string_concatenate(Path, Annfile, AbsFile).


%% 
%  Annotate a program from the examples directory
%%
%annotate_program(File) :-
%	make_abs_example_filename(File, '.pl', AbsFile),
%	annotateFile(AbsFile).

%annotate_program_and_save(File) :-
%	make_abs_example_filename(File, '.ann', AbsANNFile),
%	tell(AbsANNFile),
%	annotate_program(File),
%	told.
	
%%
% Load the annotated file from the examples directory
%%
%load_ann(File) :-
%	make_abs_example_filename(File, '.pl', AbsFile),
%	load_annfile(AbsFile).


%%
% Load new annotated file
%%
%load_file(File) :-
%	make_abs_example_filename(File, '.pl', AbsFile),
%	load_annfile(AbsFile).


%%% MARKED FOR SCRAPPING OLD CODE, if anyone wants it please tell me SJC
%%% 29/8/03


%%% %%
%%% % Load the annotated file and run cogen
%%% %%
%%% run_cogen(File) :-
%%% 	load_file(File),
%%% 	run_cogen.
	
%%% %%
%%% % Run cogen and save the results to a file
%%% %%
%%% run_cogen_and_save(File, Save) :-
%%% 	tell(Save),
%%% 	run_cogen(File),
%%% 	told.
	


%%
% Run cogen and save the results to File.gx in the examples directory
%%
%run_cogen_and_save(File) :-
%	make_abs_example_filename(File, '.gx', AbsFile),
%	run_cogen_and_save(File, AbsFile).

%%
%  Load a gx file
%%
%load_gx(File) :-
%	make_abs_example_filename(File, '.gx', AbsFile),
%	load_gx_wo_expansion(AbsFile).

	
%%
%  Load a gx file
%%



%run_gx(Atom) :-
%    setprintmode(clpr), % print speciailised programs in clpr
%    copy_term(Atom,CAtom), /* added by mal: to prevent instantiation for Tcl/Tk */
%    (annfile:residual(CAtom) -> true ; (print(CAtom),print(' not residual!'),nl,fail)),
%	add_extra_argument("_m", CAtom, V, GenexCall),
%	reset_all,	
%	call(gx:GenexCall),
%    memo:print_spec_file_header,
%	numbervars(V,0,_),
%	direct_pp([clause(CAtom, V)]),  /* missing: this is not exported in module declaration */
%	flush_pp.















%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:


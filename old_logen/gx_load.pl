:- ensure_loaded('sicstus.pl').

:- use_module('prob/logen_preferences.pl').


:- use_module(library(lists), [append/3]).

:- use_module('gensym.pl', [reset_gennum/0]).
:- use_module('memo.pl', [delete_table/0, print_spec_file_header/2]).

:- use_module('memo_pp.pl').
:- use_module('gx_pp.pl').
:- use_module('pp/flatten_pp.pl').
:- use_module('cogen-tools.pl').


go(PlFile, Goal) :-
	%print('Runing gx\n'),
	add_extension(PlFile, '.ann', AnnFile),
	add_extension(PlFile, '.gx', GxFile),
	go(PlFile, AnnFile, GxFile, Goal).
	

go(PlFile, _AnnFile, GXFile, Goal)  :-
  init_and_load_preferences('logen_preferences.pl'),
  run_gx(PlFile, GXFile, Goal).
   %load_annfile(AnnFile),
   %load_gx_wo_expansion(GXFile),
   %run_gx_and_save_wo_expansion(Goal, PlFile).


add_extension(File, Ext, NewFile) :-
	string_concatenate(File, Ext, NewFile).

reset_all :-
	reset_flattenpp,
	reset_memopp,
	reset_gxpp,
	
	reset_gennum,
	delete_table, %% reset the memo table
	flags:reset_to_defaults.	
  
run_gx(File,  GXFile, Atom) :-
	use_module(GXFile),
	reset_all,
	add_extra_argument("_request", Atom, user, RC),	
	add_extra_argument("", RC, _, RequestCall),
	% register for specialisation
	call(RequestCall),
	%% Must tell the memo module what module we are dealing with	
	memo:service_all_requests(GXFile),		
	add_extension(File, '.spec', CodeFile),
		add_extension(File, '.memo', MemoFile),
	print_spec_file_header(MemoFile,CodeFile),
	assert_failing_predicate_declarations,
	flatten_clauses('gx_pp', [gx_clause(_,_)]).


/*
add_extra_argument(T,Call,V,ResCall) :-
  Call =.. [Pred|Args],res_name(T,Pred,ResPred),
  append(Args,[V],NewArgs),ResCall =.. [ResPred|NewArgs].

res_name(T,Pred,ResPred) :-
  name(PE_Sep,T),string_concatenate(Pred,PE_Sep,ResPred).


string_concatenate(File, Ext, NewFile) :-
	name(File,FileS),
	name(Ext,ExtS),
	append(FileS,ExtS, NewFileS),
	name(NewFile, NewFileS). */
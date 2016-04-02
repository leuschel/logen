% (c) 1996-2016 Michael Leuschel
% the original logen was developed by Jesper Jorgensen and Michael Leuschel
% parts of the code contains contributions by Armin Rigo
% see https://github.com/leuschel/logen for more details

:- module(cogen, [cogen_unittest/0, cogen_run/2, cogen_run/3,
  build_specialization_entry_call/4,flatten/2, set_cogen_relative_dir/1,
   check_specialisation_query/1]).

:- use_module(annloader,[ann_clause/3, ann_decl/2, predicate_defined_by_filter/2]).
:- use_module(annmeta, [cogenmeta/8,annotation_args/3]).
:- use_module(cogen_data).
%:- use_module(errormanager).

:- use_module(library(lists)).

:- use_module('tools/tools.pl',[string_concatenate/3,same_len/2]).
:- if(current_prolog_flag(dialect, ciao)).
:- use_module('tools/ciao_tools.pl',[environ/2]).
:- else.
:- use_module('tools/sics_tools.pl',[environ/2]).
:- endif.

%:- use_module('tools/self_check.pl',
% [assert_must_succeed/1,assert_must_fail/1,mnf/1,perform_self_check/0]).

:- use_module('tools/error_manager.pl', [add_error/3,add_message/4,verbosity_level/1]).


%:- use_module(runtime_checks).
:- include(runtime_checks_perform).


%% Output all the clauses in the clause DB ( cogen_data(clause,C))
print_gx_clauses(Stream) :-
	print_gx_module_decl(Stream),
        cogen_data(clause,C),
	portray_clause(Stream,C),
	fail.
print_gx_clauses(_).

print_gx_module_decl(S) :-
	%get_current_module(Mod),
	portray_clause(S,(:- module('$VAR'('_'), [main/1,main_gx/1]))).


/* ------- */
/* cogen/1 */
/* ------- */

cogen(Options) :-
	add_message(cogen,2, "Starting cogen... (cogen/1)", []),
	add_message(cogen,2, "Options: ~w", [Options]),
	clear_cogen_data, reset_gen_clause_nr,
	setup_relative_directory, 
	pp_mnf(process_options(Options)),
	add_message(cogen,2, "Importing files", []),
	import_file_into_gx('runtime_checks.pl',[mnf/1,pp_mnf/1,add_postfix_to_pred/3]),
	import_file_into_gx('tools/error_manager.pl',all),
	import_file_into_gx('tools/tools.pl',[is_list_skel/1]),
	import_file_into_gx('op_decl.pl',all),
	import_file_into_gx('gxmodules.pl',all),
	import_file_into_gx('flatten.pl',all),
	import_file_into_gx('gximports/generalise_and_filter.pl',all),
	import_file_into_gx('gximports/print_program_point.pl',all),
	(member(xml_report_mode,Options)
	 -> import_file_into_gx('gximports/reportgen_xml.pl',all)
	 ;  import_file_into_gx('gximports/reportgen.pl',all)),
	(member(sandbox_mode,Options)
	 -> import_file_into_gx('gximports/safe_call.pl',all) ; true),
	((cogen_data(watch,memo) ; cogen_data(watch,unfold))
	 -> import_online_files
	 ; (cogen_data(watch,_)
	     -> import_watchdog_builtin_files ; true)
	),
	import_file_into_gx('cogen.pl',[build_unfold_call/4,build_request_call/5]),

	add_message(cogen,2, "Generating clauses for gx file...",[]),
	generate_gx_header,
	collect_and_assert_clauses("Request Clauses", request_clause(_)), 
	collect_and_assert_clauses("Unfold Clauses", unfold_clause(_)),
	collect_and_assert_clauses("Entry Clause", entry_clause(_)),
	collect_and_assert_clauses("Type Clause", type_clause(_)),
	((member(watch(_),Options);member(watch_once(_),Options);online_filters_exist)
	 -> collect_and_assert_clauses("Static Functor Facts", static_functor_fact(_)) ; true), 
	(member(display_gx_file,Options) -> print_gx_clauses(user) ; true),
	add_message(cogen_run,3,"Finished running cogen.~n",[]).


generate_gx_header :- declaration_ids_to_include_in_gx(ID),
    ann_decl(ID,Decl),
    assert_gx_declaration(Decl),
    add_message(cogen_run,3,"Put Declaration into gx: ~w.~n",[Decl]),
    fail.
generate_gx_header :- declaration_ids_to_include_in_spec(ID),
    ann_decl(ID,Decl),
    mnf(assert_gx_clauses([clause(spec_data(declaration,Decl),true)])),
    add_message(cogen_run,3,"Marked Declaration for spec file: ~w.~n",[Decl]),
    fail.
generate_gx_header :-  copy_cogen_data_to_spec_data(X),cogen_data(X,true),
    mnf(assert_gx_clauses([clause(spec_data(X,true),true)])),
    fail.
generate_gx_header :- mnf(assert_gx_clauses([clause(spec_data(_,_),fail)])).

copy_cogen_data_to_spec_data(debug_mode).
copy_cogen_data_to_spec_data(debug_mode_full).
copy_cogen_data_to_spec_data(no_post_unfold).
copy_cogen_data_to_spec_data(aggressive_post_unfold).

generate_spec_header.

declaration_ids_to_include_in_gx(op).
declaration_ids_to_include_in_gx(use_library).  /* may need to be refined ! */
declaration_ids_to_include_in_gx(use_module).
declaration_ids_to_include_in_gx(ensure_loaded).
declaration_ids_to_include_in_gx(dynamic). % TO DO: may need to be adapted if pred has been specialised !!

declaration_ids_to_include_in_spec(op).
declaration_ids_to_include_in_spec(use_library).
declaration_ids_to_include_in_spec(use_module). 
declaration_ids_to_include_in_spec(dynamic). /* may need to be refined ! */
declaration_ids_to_include_in_spec(ensure_loaded).



:- dynamic cogen3_relative_directory/1.

make_unification_pre(X,Y,_Z) :- nonvar(X),nonvar(Y).
make_unification_post(_X,_Y,Z) :- nonvar(Z).

make_unification([],[], true).
make_unification([A|As],[B|Bs], (A=B,Res)) :-
	make_unification(As,Bs,Res).
	
single_disj((Vars1-Code),Vars2, Disj) :-
	mnf(make_unification(Vars1,Vars2,Uni)),
        Disj=(Uni,Code). 
make_disj([A],Vars2,Disj) :-
	single_disj(A,Vars2,Disj).
make_disj([A|As],Vars2,(Disj;RDisj)) :-
        single_disj(A,Vars2,Disj),
        make_disj(As,Vars2,RDisj).

on_exception(Ex, Goal, Hand) :-
	catch(Goal, Ex, Hand).


set_cogen_relative_dir(Dir) :-
   retractall(cogen3_relative_directory(_)),
   add_message(cogen,2,"Setting LOGENDIR: ~w~n",Dir),
   assert(cogen3_relative_directory(Dir)).

setup_relative_directory :-
   \+ cogen3_relative_directory(_),
   %retractall(cogen3_relative_directory(_)),
   !,
   on_exception(_Exc,absolute_file_name('cogen.pl',_File),
     (environ('cogen3_source_directory',Dir)
      -> on_exception(_, 
            (absolute_file_name('cogen.pl','','',Dir,_File,_,_),
             assert(cogen3_relative_directory(Dir))),
         add_error(setup_relative_directory,
               "Cannot find cogen.pl!\n Please set up Environment variable cogen3_source_directory.",[])
           )
      ; (add_error(setup_relative_directory,
               "Environment variable 'cogen3_source_directory' not defined!",[])
        )
     )
    ).
setup_relative_directory.    

open_in_logen_source(FileName,Mode,Stream) :-
    (cogen3_relative_directory(Dir) 
      -> (%atom_concat(Dir,FileName,AbsFile),
	     absolute_file_name(FileName, '','', Dir,AbsFile, _,_),
          on_exception(Exc,open(AbsFile,Mode,Stream),
                     (add_error(open_in_logen_source,"Cannot open file in directory: ~w",[(AbsFile,Exc)]),fail))
          )
      ;  on_exception(Exc2,open(FileName,Mode,Stream),
                     (add_error(open_in_logen_source,"Cannot open Logen source file (use -logen_dir option): ~w ",
                       [(FileName,Exc2)]),fail))
    ).


process_options_pre(X) :- ground(X), list(X).
process_options_post(_).

process_options([]).
process_options([Opt|Opts]) :-
	process_option(Opt),
	process_options(Opts).

%process_option(debug(L)) :- !, assert(cogen_data(debug,L)).
process_option(watch(once)) :- !, assert(cogen_data(watch_once,true)).
process_option(watch(default)) :- !, assert(cogen_data(watch,builtins)),
         assert(cogen_data(watch,unfold)),assert(cogen_data(watch,memo)),
         assert(cogen_data(watch,connectives)).
process_option(watch(L)) :- !, assert(cogen_data(watch,L)).
process_option(watch_once(L)) :- !, assert(cogen_data(watch_once,true)), process_option(watch(L)).
process_option(silent) :- !, assert(cogen_data(silent,true)).
process_option(display_gx_file) :- !, assert(cogen_data(display_gx_file,true)).
process_option(memo_table) :- !, assert(cogen_data(print_memo_table,true)).
process_option(no_post_unfold) :- !, assert(cogen_data(no_post_unfold,true)).
process_option(aggressive_post_unfold) :- !, assert(cogen_data(aggressive_post_unfold,true)).
process_option(debug_mode) :- !, assert(cogen_data(debug_mode,true)).
process_option(sandbox_mode) :- !, assert(cogen_data(sandbox_mode,true)).
process_option(xml_report_mode) :- !, assert(cogen_data(xml_report_mode,true)).
process_option(debug_mode_full) :- !, assert(cogen_data(debug_mode,true)),
            assert(cogen_data(debug_mode_full,true)).
process_option(logen_dir(_)) :- !. /* already dealt with earlier */
process_option(verbose_mode(_)) :- !. /* already dealt with earlier */
process_option(gx_file(_)) :- !. /* already dealt with earlier */
process_option(cogen_only) :- !. /* already dealt with earlier */
process_option(single_process) :- !. /* already dealt with earlier */
process_option(ciao_path(_)) :- !. /* already dealt with earlier */
process_option(spec_file(_)) :- !. /* already dealt with earlier */
process_option(Option) :-
     format(user_error,"% *** Unknown command-line option: ~w ***~n",[Option]),
     add_message(cogen,4,"Unknown cogen option: ~w.",[Option]). %% unknown option - ignore


/* ---------------------------- */
/*     import_file_into_gx/2    */
/* ---------------------------- */

%% only import if not already imported...
%%% ** kind of checks if previous import
%%     but will only match identical imports so
%%     import(File, all) and import(File, [pred/2])
%%     could cause multiple imports
%import_file_into_gx(File) :-
%	import_file_into_gx(File,all).

import_file_into_gx(File,Preds) :-
    pp_mnf(import_file_into_gx2(File,Preds)).
    
import_file_into_gx2_pre(File,Preds) :- 
    ground(File), ground(Preds), (Preds=all ; list(Preds)).
import_file_into_gx2_post(_,_).

import_file_into_gx2(File,Preds) :-
	cogen_data(imported,(File,Preds)),!.
import_file_into_gx2(File,Preds) :-
	get_clauses_from_file(File,Clauses, Preds),
	assert(cogen_data(imported,(File,Preds))),
	assert_gx_clauses(Clauses).


/* ---------------------------- */
/* collect_and_assert_clauses/2 */
/* ---------------------------- */

collect_and_assert_clauses(Name,Call) :-
	Call =.. [_,Clause],
	findall(Clause,Call , Clauses),
	length(Clauses, No),
	add_message(cogen,2,"~w ~s generated", [No, Name]),
    add_message(cogen,4,"~s: ~w",[Name,Clauses]),	
	mnf(cogen:assert_gx_clauses(Clauses)).

%% Get the module nsame from the annotation declarations
get_current_module(Module) :-
	ann_decl(module,Module),!.
get_current_module(default).


/* --------------------- */
/*  assert_gx_clauses/1  */
/* --------------------- */

%% Save a collection of clauses into the Clause DB
assert_gx_clauses([]).
assert_gx_clauses([clause(H,B)|Cs]) :-
	assert(cogen_data(clause,(H:-B))),
	assert_gx_clauses(Cs).
assert_gx_clauses([decl(H)|Cs]) :-
	assert(cogen_data(clause, (:-H))),
	assert_gx_clauses(Cs).

assert_gx_declaration(Declaration) :-
   mnf(assert_gx_clauses([decl(Declaration)])).
   



build_specialization_entry_call(Goal,ResGoal,Options,EntryCall) :-
	get_current_module(This),
	atom_concat(This, '_entry',Func),
    EntryCall =.. [Func,Goal,ResGoal,Options].


type_clause(clause(usertypedef(Type,Def),true)) :-
	ann_decl(type, '--->'(Type,Def)).

%% Makes the entry point for gx based on module name
entry_clause(clause(Entry,Body)) :-
	Opts = '$VAR'('Opts'),
	Goal = '$VAR'('Goal'), 
	ResCall = '$VAR'('ResCall'),
    mnf(build_specialization_entry_call(Goal, ResCall,Opts,Entry)),
	LOGENDATA = [[],entry],
	(cogen_data(print_memo_table,true) -> PM = print_memo_table(user) ; PM = true),
	 ((verbosity_level(Level), Level >= 1) -> PStats = format(user, "~N/* Specialisation time ~w ms (runtime) */~n", [Time]) ; PStats = true),
	Body = (
		  (member(outfile(Out), Opts) -> 
		     open(Out, write, Stream)
		    ;
		      Stream = user
		   ),

		 statistics(runtime,[T1,_]),
		 mnf(build_request_call('$VAR'('Goal'),crossmodule,'$VAR'('ResCall'),LOGENDATA,'$VAR'('REQ'))),
		 
		 '$VAR'('REQ'),
		 add_entry_point('$VAR'('ResCall'),'$VAR'('Goal')),  /* just register the entry point; for post-unfolding */
		 mnf(spec_driver),
		 PM,
		 mnf(print_clauses(Stream)),
		 statistics(runtime,[T2,_]),
		 Time is T2 - T1,
		 PStats,
		 
		 (member(add_entry, Opts) -> 
		  portray_clause(Stream, (Goal:-ResCall))
		 ;
		     true
		   ),
		 close(Stream)
		).

%entry_clause(clause(entry('$VAR'('Goal'),'$VAR'('ResCall')),Body)) :-	
%	build_specialization_entry_call('$VAR'('Goal'), '$VAR'('ResCall'),Body).

entry_clause(clause(main(Args), main_gx(Args))).
entry_clause(clause(main(Args),(format(user,"*** Error!~n*** Command Line Arguments:'~w'~n",[Args]),print_usage))).
entry_clause(clause(main_gx(Args), Body)) :-
	AtomGoal = '$VAR'('AtomGoal'),
	Goal = '$VAR'('Goal'),
	Opts = '$VAR'('Opts'),
	build_specialization_entry_call('$VAR'('Goal'), '$VAR'('_Res'),Opts,CallEntry),
	import_file_into_gx('tools/ciao_tools.pl',[read_from_chars/2]),
	import_file_into_gx('ciao_entry.pl',[get_options/3,print_options/0,print_option_args/2,print_usage/0]),
	import_file_into_gx('gx_options.pl',all),

	Body = (		   
		   get_options(Args, Opts,RemOpts),
		   (member(help, Opts) ->
		       print_usage %%fail to print usage
		   ;  (RemOpts=[AtomGoal|_]
		       -> (read_from_chars(AtomGoal,Goal)
		           -> ((catch(CallEntry,Exc,(functor(Goal,Fun,Arity),(Exc = error(existence_error(_,_),_)
		                    -> format(user_error,"~n*** Existence Exception during Specialisation of query: '~w'.~n*** You may need to add a filter declaration for ~w/~w.~n*** ~w~n",[AtomGoal,Fun,Arity,Exc])
		                    ;  format(user_error,"*** Exception Occured during Specialisation of '~w'.~n*** ~w~n",[AtomGoal,Exc])
		                    ),halt(1))) -> true
		                 ; (format(user_error,"*** Specialisation failed for: '~w'.~nCall Entry: '~w'.~n",[Goal,CallEntry]),halt(1))),
		               count_errors_occured(N),
		               (N>0 -> (format(user,"~n*** ~w errors occurred !~n",[N]),halt(1)) ; true)
		               )
		            ;  (format(user_error,"*** Error: Could not read_from_chars: '~w'~n",[AtomGoal]),fail)
		          )
		        ;  (format(user_error,"*** Error: No atom to specialise provided on command-line.~n",[]),fail)
		       )
		   )).



:- dynamic gen_clause_nr/1.
gen_clause_nr(1).
:- dynamic gen_local_clause_nr/3.
gen_local_clause_nr(ppp,2,1).

get_new_clause_nr(X) :-
  retract(gen_clause_nr(X)),
  X1 is X+1,
  assert(gen_clause_nr(X1)).

get_new_pred_clause_nr(Pred,Arity,X) :-
  (retract(gen_local_clause_nr(Pred,Arity,X)) -> true ; X=1),
  X1 is X+1,
  assert(gen_local_clause_nr(Pred,Arity,X1)).
	
reset_gen_clause_nr :-
  retractall(gen_clause_nr(_)),
  retractall(gen_local_clause_nr(_,_,_)),
  assert(gen_clause_nr(1)).
 

%entry_clause(clause(Entry,Body)) :-
%	Entry =.. [gx_entry,'$VAR'('Goal'), '$VAR'('ResCall')],
%	LOGENDATA = [[],entry],
%	Body = (		 
%		 build_request_call('$VAR'('Goal'),crossmodule,'$VAR'('ResCall'),LOGENDATA,'$VAR'('REQ')),
%		 '$VAR'('REQ'),
%		 spec_driver,
%		 print_clauses(user)		).

%% Specialised unfolders
unfold_clause(clause(NewCall, FullGxBody)) :-
	ann_clause(_,Call, Body),
	%portray_clause(body(Body)),
	mnf(cogen:get_new_clause_nr(CNR)),
	add_message(cogen_run,3,"Generating Unfolder ~n for ~w.~n",[CNR,Call]),
	logendata_varname(LOGENDATA),
	mnf(build_unfold_call(Call, SpecBody, LOGENDATA, NewCall)),
	functor(Call,Pred,Arity),
	get_new_pred_clause_nr(Pred,Arity,LocalCNR),
	pp_mnf(body(Body, GxBody, SpecBody,clause(CNR,Pred,Arity,LocalCNR))),

	(cogen_data(debug_mode_full,true)
	   -> FullGxBody = (tab_logendata_history(LOGENDATA,user,'+-'),
	                    format(user,"%Unfold clause: ~w ~w~n",[CNR,Call]),
	                    GxBody,
	                    tab_logendata_history(LOGENDATA,user,'| '),
	                    format(user,"%  Done unfold: ~w ~w~n",[CNR,Call]))
	   ;  FullGxBody=GxBody).

:- dynamic online_filters_exist/0.		 

import_online_files :-
	    import_file_into_gx('online.pl',all),
	    import_file_into_gx('gximports/watchdog_call.pl',all), %[wcall_error/2,wcall_exc/3,watch_gx_if_conjunction/6, watch_gx_or_conjunction/4, watch_gx_call/2]),
	    import_file_into_gx('homeomorphic.pl',all),
	    import_file_into_gx('msg.pl',all),
	    import_file_into_gx('gximports/safe_call.pl',all),
	    (online_filters_exist -> true ; assert(online_filters_exist)).
	    

import_watchdog_builtin_files :- 
	    import_file_into_gx('gximports/watchdog_call.pl',all), %[wcall_error/2,wcall_exc/3,watch_gx_if_conjunction/6, watch_gx_or_conjunction/4, watch_gx_call/2]),
	    import_file_into_gx('gximports/safe_call.pl',all).

   
   
filter_contains_online(online).
filter_contains_online(struct(_,L)) :- l_filter_contains_online(L).
filter_contains_online(type(list(T))) :- filter_contains_online(T).

/* still need to add this for user defined types !!*/

l_filter_contains_online([H|T]) :-
  filter_contains_online(H) ; l_filter_contains_online(T).
  
/* ---------------------- */
/* STATIC FUNCTOR CLAUSES */
/* ---------------------- */

static_functor_fact(clause(static_functor(F,Arity),true)) :-
  setof( p(FF,AA), static_functor_in_ann_program(FF,AA), List),
  member(p(F,Arity),List).
  
static_functor_in_ann_program(F,Arity) :-
    ann_clause(_,Head, Body),
    (functor_inside_term(Head,F,Arity) ;
     functor_inside_ann_term(Body,F,Arity)).


functor_inside_ann_term(X,F,A) :- annotation_args(X,Ann,UnAnn),!,
    (l_functor_inside_ann_term(Ann,F,A) ; l_functor_inside_term(UnAnn,F,A)).
functor_inside_ann_term(X,F,A) :-
     add_error(cogen_static_functors,"Unrecognised Annotated Term: ~w",[X]),
     functor_inside_term(X,F,A).
    
l_functor_inside_ann_term([H|T],F,A) :- 
    functor_inside_ann_term(H,F,A) ; l_functor_inside_ann_term(T,F,A).  

functor_inside_term(X,_,_) :- var(X),!,fail.
functor_inside_term(Term,F,A) :- functor(Term,F,A).
functor_inside_term(Term,F,A) :- 
    Term=..[_|Args], l_functor_inside_term(Args,F,A).
    
l_functor_inside_term([H|T],F,A) :- 
    functor_inside_term(H,F,A) ; l_functor_inside_term(T,F,A).    

/* --------------- */
/* REQUEST CLAUSES */
/* --------------- */

:- use_module('gximports/print_program_point').
check_specialisation_query(Query) :-
  functor(Query,FC,NA),
  (predicate_defined_by_filter(FC,NA)
   -> true
    ; (format(user_error,"*** Cannot specialize: ~w.~n",[Query]),
       functor(Q2,FC,NA),
       (ann_clause(_,Q2,_)
         ->  print_filter_error('No Filter Declaration for Specialization Query.',error,filter,[call(Query)],filter(FC,NA))
         ;   format(user_error,"*** Unknown Predicate: ~w/~w.~n",[FC,NA])
        ),
       abort_specialization
      )
  ).


request_clause(clause(Head,Body)) :-
	%% Could be multiple filters.... use setof
	setof(pred(FC,NA),predicate_defined_by_filter(FC,NA),Preds),
	member(pred(Pred,Arity),Preds), functor(Call,Pred,Arity),
	get_current_module(This),
	Call =.. [Func|Args],  %% create new function call
	same_len(Args,NewArgs),
	NewCall =.. [Func|NewArgs],
	logendata_varname(LOGENDATA),
	build_request_call(NewCall, Requestor, ResCall,LOGENDATA, Head),
	(cogen_data(clause, (Head:-_)) -> (!,fail) ; true), % if multiple filters, only make 1 clause
	
	findall(Filters, (ann_decl(filter,Call), Call=..[_|Filters]), AllFilters),
	import_file_into_gx('find_pattern.pl',all),
	(cogen_data(watch,memo) ->
	 ( (cogen_data(watch_once,_) -> Once=yes ; Once=no),
	    WatchedCode = 
	      (is_not_safe_to_add_memo_entry(GCall, ParentID,History) ->
	           (  (get_logendata_pp(LOGENDATA,OriginPP) -> PP=memo(GCall,OriginPP) ; PP=filter(Pred,Arity)),
			     /* History will be instantiated by is_safe_to_add */
			      confirm_user(online_unsafe_memo(Once), [GCall,History],_RetVal,PP)
			   )
			  ;    true % its safe
	       )
	 )
	;    WatchedCode = true /* here --> true  by Michael */
	),
	(cogen_data(debug_mode,true)
	  -> (DebugCode = format(user,"Memoizing: ~w~n",GCall),
	      (cogen_data(debug_mode_full,true)
	        -> (DebugCode_Found = format(user,"Already specialized: ~w~n",NewCall),
	            DebugCode_Found2 =format(user,"Generalized already specialized: ~w~n",GCall)
	           )
	        ;  (DebugCode_Found = true, DebugCode_Found2=true)
	      ))
	  ;  (DebugCode_Found=true,DebugCode=true,DebugCode_Found2=true)
	),
	((AllFilters = [Filters], l_filter_contains_online(Filters)) ->
	    %%% Its online:
	    import_online_files,	    
	    Body =
	     (
	       
	       (find_variant_pattern(This,NewCall, ResCall, Requestor) ->
		   DebugCode_Found
	       ;
		   
		   LOGENDATA = [_,ParentID|_],
		   generalise_online_mixed(Filters, NewCall, GCall, ParentID),
		   %generalise_online(NewCall, GCall, ParentID),
		   (find_pattern(This, GCall, ResCall, Requestor) 
		    -> DebugCode_Found2
	        ; (filter_online(NewCall, GCall, ResCall),
		       DebugCode,
		       init_memodata(pending(Requestor),ParentID,MEMODATA),
		       insert_pattern(This,GCall,ResCall,MEMODATA)
		     )
	       )
	       ),
	       GCall = NewCall
	    
	      )
	;
	    %% Offline 
	    Body =
	     ( generalise_call(AllFilters, NewCall, GCall)
	     -> ((find_pattern(This, NewCall, ResCall, Requestor) ->
		      true
	          ;
		      LOGENDATA = [_,ParentID|_],
		      filter_call(AllFilters,NewCall, GCall, ResCall),
		      DebugCode, WatchedCode,
		      init_memodata(pending(Requestor),ParentID,MEMODATA),
		      insert_pattern(This,GCall,ResCall,MEMODATA)
	         ),
	         GCall = NewCall)
	       ; (  format(user_error, "~n<| FILTER ERROR |> : MEMO Atom ~w~n",[NewCall]),
	            PPCODE, 
	            abort_specialization )
	      )
	       
	),		   
	(cogen_data(watch,_)
	  -> PPCODE = ((get_uncovered_correction(AllFilters, NewCall,CorrectionList)
	                 -> CorrPP = correctfilt(CorrectionList,filter(Pred,Arity))
	                 ;  CorrPP = filter(Pred,Arity)
	                ),
	               (get_logendata_pp(LOGENDATA,OriginPP2)  /* if fails: probably because entry call is not covered by filters */
	                  -> print_program_point_and_filter_error(MSG,
	                       error,memo_filter,[call(NewCall)],memo(NewCall,OriginPP2),CorrPP)
	                  ;  print_filter_error(MSG,error,filter,[call(NewCall)],CorrPP)
	              ))
	  ;  PPCODE = ((get_uncovered_correction(AllFilters, NewCall,CorrectionList)
	                 -> CorrPP = correctfilt(CorrectionList,filter(Pred,Arity))
	                 ;  CorrPP = filter(Pred,Arity)
	               ),
	               print_filter_error(MSG,error,filter,[call(NewCall)],CorrPP)
	             )
	               
	),
	MSG = 'A memoised call is not covered by its filter declaration and could not be specialized.',
	GCall = '$VAR'('GenCall'),
	Requestor = '$VAR'('Requestor'),
	ResCall = '$VAR'('ResidualCall'),
	History = '$VAR'('History'),
	ParentID = '$VAR'('ParentID').

	
	% correctfilt([make_dynamic(N),...]),PP) Res=<make_dynamic>1</make_dynamic>
	
%find_pattern_call(This, NewCall, ResCall, Requestor, Call) :-
%	Call = find_pattern(This, NewCall, ResCall, Requestor),
%	new_call(Call, NCall),
%	(cogen_data(clause,(NCall:-_)) ->
%	    true
%	;
%	    import_file_into_gx('find_pattern.pl')
%	    
%	).


new_call(Call, NCall) :-
	Call =.. [F|Args],
	same_len(Args, NArgs),
	NCall =.. [F|NArgs].



%% Map the annotations to the body to build the clauses

body_pre(Ann,_,_,_) :- nonvar(Ann).
body_post(_,G,_,_) :- nonvar(G).

body(Ann, GxFlat, SpecFlat,ProgramPoint) :-	
	cogenmeta(Ann, Cogen,Gx,Spec,Map,MapGx,MapSpec,ProgramPoint),
	functor(Ann,AFunctor,_),
	body_list(Map,MapGx,MapSpec,ProgramPoint,1,AFunctor),
	Cogen,
	flatten(Gx,GxFlat),
	flatten(Spec,SpecFlat).
	
body_list([],[],[],_PP,_,_).
body_list([A|As],[Gx|Gxs],[Spec|Specs],ProgramPoint,Index,AFunctor) :-
    InternalPP = ann(AFunctor,Index,ProgramPoint),
	body(A,Gx,Spec,InternalPP),
	I1 is Index + 1,
	body_list(As,Gxs,Specs,ProgramPoint,I1,AFunctor).



%% Helper functions

%% Simple flatten, always called with conjunctions
flatten(Conj, NA) :-	
	nonvar(Conj),
	Conj = (A,B),
	(
	  ( A == true, R = B)
	;
	  ( B == true, R = A)
	),!, flatten(R,NA).
flatten(A,A).


logendata_varname('$VAR'('__LOGENDATA')).

%%% load all the clauses from +Filename into -Clauses that meet pattern Preds ([f/1]) or specal all
get_clauses_from_file(Filename, Clauses, Preds) :-
	open_in_logen_source(Filename, read, Stream),
	read_all_clauses(Stream, Clauses, Preds),
	close(Stream).

read_all_clauses(Stream, C,Preds) :-
	read_term(Stream ,Clause, [variable_names(Names)]),
	(Clause = end_of_file ->
	    C = []
	;
	    read_all_clauses(Stream, Clauses,Preds),
	    %% Should we ignore or keep this clause?

	    (ignore_clause_import(Clause,Preds) ->
		% we ignore
		C = Clauses
	    ;
		ground_var_names(Names),
		(Clause = (H:-B) -> C1 = clause(H,B)
		;
		    (Clause = (:-H) ->
			C1 = decl(H)
		    ;
			H = Clause,
			C1 = clause(H, true)
		    )
		),
		C = [C1|Clauses]
	    )
	).

ignore_clause_import((:-module(_)),_).
ignore_clause_import((:-include(_)),_).
ignore_clause_import((:-module(_,_)),_).
ignore_clause_import(_,all) :- !,fail.
ignore_clause_import(Clause, Preds) :-
	(Clause = (H:-_);Clause =(:-H);Clause = H),
	functor(H,F,Arity),!, \+(member(F/Arity,Preds)).


ground_var_names([]).
ground_var_names([N=V|R]) :-
	'$VAR'(N)=V,
	ground_var_names(R).


build_unfold_call(Call, SpecCode, LogenData,UnfoldCall) :-
	Call =.. [Func|Args],
	atom_concat(Func, '_u', NewFunc),
	append(Args, [SpecCode, LogenData], NewArgs),
	UnfoldCall=..[NewFunc|NewArgs].


build_request_call(Call, Req, ResCall,LogenData,RequestCall) :-
	Call =.. [Func|Args],
	atom_concat(Func, '_m', NewFunc),
	append(Args, [Req,ResCall, LogenData], NewArgs),
	RequestCall=..[NewFunc|NewArgs].



%%%
% Unit Test
%%% 
cogen_unittest :-
	run_cogen_test(unfold),
	run_cogen_test(match).

run_cogen_test(ID) :-
	cogen_test(ID, C),
	test_answer(ID,C).

:- use_module(annloader,[load_annfile/1]).

cogen_test(unfold,Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/test.pl.ann'),
	clear_cogen_data, reset_gen_clause_nr,
	collect_and_assert_clauses("Unfold Clauses", unfold_clause(_)),
	findall(C,cogen_data(clause,C),Clauses).	
cogen_test(match,Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/match.pl.ann'),
	cogen([watch(all)]),	
	findall(C,cogen_data(clause,C), Clauses),
        open_in_logen_source('tests/match.gx.pl', write,S),
	print_gx_clauses(S),
	close(S).
cogen_test(append_online, Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/append_onlinepl.ann'),
	cogen([]),	
	findall(C,cogen_data(clause,C), Clauses),
	open_in_logen_source('tests/append_online.gx.pl', write,S),
	print_gx_clauses(S),
	close(S).
cogen_test(append, Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/append.pl.ann'),
	cogen([watch(all)]),	
	findall(C,cogen_data(clause,C), Clauses),
	open_in_logen_source('tests/append.gx.pl', write,S),
	print_gx_clauses(S),
	close(S).
cogen_test(rev, Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/rev.pl.ann'),
	cogen([]),	
	findall(C,cogen_data(clause,C), Clauses),
	open_in_logen_source('tests/rev.gx.pl', write,S),
	print_gx_clauses(S),
	close(S).	
cogen_test(vanilla, Clauses) :-
	%use_module(annloader,[load_annfile/1]),  % commented out by Michael
	annloader:load_annfile('tests/vanilla_list.pl.ann'),
	cogen([watch(all)]),	
	findall(C,cogen_data(clause,C), Clauses),
	open_in_logen_source('tests/vanilla_list.gx.pl', write,S),
	print_gx_clauses(S),
	close(S).
	
	
/* ----------- */
/* cogen_run/3 */
/* ----------- */

:- pp_mnf(cogen_run/3).


:- if(current_prolog_flag(dialect, ciao)).
:- pre cogen_run(A,F,O) : (ground(A), atomic(A), ground(F), atomic(F), ground(O)).
:- post cogen_run(_,_,_) : true.
:- endif.

cogen_run(Ann,Gx) :-
	cogen_run(Ann,Gx,[]).
cogen_run(AnnFilename,GXFile,Options) :- 
	add_message(cogen_run,3,"Loading annfile: '~w'.~n",[AnnFilename]),
	mnf(annloader:load_annfile(AnnFilename)),
	
	add_message(cogen_run,3,"Running cogen with options: '~w'.~n",[Options]),
	mnf(cogen(Options)),	
	
	add_message(cogen_run,3,"Printing Clauses to GX File: '~w'.~n",[GXFile]),
	open(GXFile, write,S),
	mnf(cogen:print_gx_clauses(S)),
	close(S).


%fold,[(my_head_u(A,B,bar(B)):-foo(A)),(my_head_u(C,D,(cond(C)->if(C);else(D))):-before(C),next(C))]) :- !.

test_answer(match,[(match_u(A,B,C):-match1_u(A,B,A,B,C)),(match1_u([D|_],[E|_],F,[_|G],(D\==E,H)):-match1_m(F,G,F,G,internal,H)),(match1_u([I|J],[I|K],L,M,N):-match1_u(J,K,L,M,N))]) :- !.


%% If answers fail:
test_answer(ID, _) :-
	add_error(cogen,"Test case failed ~w", [ID]),
	fail.






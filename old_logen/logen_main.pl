/* ---------------------------------- */
/* (c) Michael Leuschel, Jan/Feb 2001 */
/* ---------------------------------- */

/* load all necessary modules */





%:- use_module(library(tcltk)).
:- use_module(library(random)).
:- use_module(library(system)).%,
% 	Mal = '/Users/mal/cvs_root/cogen2/logen_source',
% 	MV = '/Users/mv/cvs_server/soton/cogen2/logen_source',
% 	(file_exists(Mal) ->
% 	    system:working_directory(_,Mal)  %%% to get rid of errors as I dont have this directory..... :-)
% 	; true),
% 	  (file_exists(MV) ->
% 	    system:working_directory(_,MV)  %%% to get rid of errors as I dont have this directory..... :-)
% 	; true).

:- multifile portray/1.
user:portray(call(A)) :-
	nonvar(A),
	write_term(A,[]).



:- ensure_loaded('sicstus.pl').  

:- use_module(library(lists)).
:- use_module(library(charsio)).



%:- use_module(memo,[find_pattern/4]).


:- use_module('run_cogen', [run_cogen_if_necessary/1,run_cogen_on_plfile/1]).
:- use_module('run_gx', [specialise_plfile/4,specialise_plfile/5,specialise_plfile/7]).

:- use_module('moduledriver').
:- use_module('run_binsolve').
:- use_module('run_binmemo').
:- use_module('prob/logen_preferences.pl').
:- use_module('prob/error_manager.pl').
:- use_module('prob/tools.pl').
%:- use_module('cogen-interface.pl').
:- use_module(bta,[annotateFile/3]).
:- use_module('./annotation/match_ann.pl').
:- use_module('./annotation/save_ann.pl').
:- use_module('./annotation/parser.pl').
:- use_module('flags.pl').

:- use_module(benchmark).


:- use_module('logen_post', [run_pp/3]).


atom_ok_to_specialise(Atom) :- annfile:residual(Atom),!.
atom_ok_to_specialise((A1,A2)) :- atom_ok_to_specialise(A1),atom_ok_to_specialise(A2).

python_run_spec_program(Goal, File, Module, Output) :-
	with_output_to_chars((
			      print(calling(Goal)),nl,
			      tcltk_run_spec_program(Goal,File,Module),
			      print(called(Goal)),nl
			     ), OutputS),
	name(Output,OutputS).

get_residual_goal_name(Goal , FGoal) :-
	memoizer:find_pattern(_,Goal, FGoal,_).



tcltk_run_spec_program(Goal,File,Module) :-
   print_message('Using specialised module: '), print_message(File),
   use_module(File),
   copy_term(Goal,CGoal),
   %(memo:find_pattern(_,CGoal,FGoal,_)
   (memoizer:find_pattern(_,CGoal,FGoal,_)
      -> (print_message(calling(FGoal)),
	      statistics(runtime,[Time1,_]),
          (call(Module:FGoal)
            -> (true) ; (print('Failed, '))),
	       statistics(runtime, [Time2,_]),
	       Time is Time2-Time1,
           print_message(done(FGoal)),
	     Goal = CGoal,
     
	     
           print('Runtime: '), print(Time), print(' ms'),nl)
      ;  (add_error(run_spec_program,'Illegal call: cannot filter :',Goal),nl)
   ),
   nl.



parse_file(Filename):-
	open(Filename,read, Stream),
	parse_file1(Stream),
	close(Stream).

parse_file1(S):-
	read_term(S,T,[]),
	(T \= end_of_file ->
	    %%% ADD ANY OPERATORS FROM FILE WE ARE TRYING TO PARSE...
	    (T = ':-'(op(N,FX,OP)) ->
		op(N,FX,OP)
	    ;
		true
	    ),
	    parse_file1(S)
	;
	    true).

	   

%%% dead code  below here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1




%%% mycall(Call, Err) :-
%%%     on_exception(E, (Call, Err=ok), err(E,Call,Err)).





%%% err(existence_error(_Call,_,file, FileName,_),_,Err) :-
%%%     format_to_chars("File ~a does not exist", [FileName], ErrS),
%%%     name(Err,ErrS).

%%% err(existence_error(_Call,_,procedure, Call,_),_,Err) :-
%%%     format_to_chars("Call ~w does not exist", [Call], ErrS),
%%%     name(Err,ErrS).


%%% err(Exception, _Call, Err) :-
%%%     print(Exception),  
%%%     format_to_chars("Exception: ~w\n", [Exception], ErrS),
%%%     name(Err,ErrS).





%bta(File) :-
%	run_binsolve(File,_Memo,_Spec).%%% ADD ENTRY HERE
	

%%% go :-
%%%     'cogen-interface':reset_all,
%%%     tk_new([name('LOGEN')], X),
%%%     tcl_eval(X, 'source logen.tcl', _),
%%%     tk_main_loop,
%%%     tcl_delete(X).
/* -------------------------------------------------------------------- */
%%% tcltk_initialise.



%%% save :- save_program('../bin/logen.sav').

%%% runtime_entry(start) :- go.

%:- nl,print('Type go to startLogen'),nl.
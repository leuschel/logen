% (c) 1996-2016 Michael Leuschel
% see https://github.com/leuschel/logen for more details


:- module(self_check,
    [assert_pre/2, assert_post/2,pp_mnf/1, pp_cll/1, mnf/1, mnf/2,
     assert_must_succeed/1,assert_must_fail/1,
     assert_must_succeed_multiple/1, assert_must_succeed_any/1,
     must_fail/1, must_succeed/1, add_self_check/1,
     
     run_time_type_check_mode/1,
     turn_on_run_time_type_checks/0,turn_off_run_time_type_checks/0,
     
     perform_self_check/0,
     perform_verbose_self_check/0,
     display_self_checks/0]).

:- use_module(gensym).
:- use_module(debug).
%:- use_module(tools).   
:- use_module('tools.pl').

:- use_module(typechecker).
:- use_module(error_manager).

/* Example use:

:- assert_pre(user:b_fd_type(_G,_L,_U),true).
:- assert_post(user:b_fd_type(G,L,U),(atomic(G),(integer(L),integer(U)))).

*/


/* some auxilary predicates that can be used for pre,post conditions: */

list_skeleton(X) :- 
  (nonvar(X),list_skel2(X) -> true
              ; (print_error('### not list skeleton: '), print_error(X),fail)).
list_skel2([]).
list_skel2([_H|T]) :- nonvar(T), list_skel2(T).



:- dynamic run_time_type_check_mode/1.
run_time_type_check_mode(on).

turn_on_run_time_type_checks :- 
      retract(run_time_type_check_mode(_)),
      assert(run_time_type_check_mode(on)).
      
turn_off_run_time_type_checks :- 
      retract(run_time_type_check_mode(_)),
      assert(run_time_type_check_mode(off)).


:- dynamic pre_condition/2, post_condition/2.

assert_pre(X,Pre) :- 
    ((nonvar(X),nonvar(Pre))
       -> (retractall(pre_condition(X,_)),assert(pre_condition(X,Pre)))
       ;  (print_error('### illegal variable(s) in: '),
           print_quoted(assert_pre(X,Pre)),nl,fail)
    ).
assert_post(X,Post) :- 
    ((nonvar(X),nonvar(Post))
       -> (retractall(post_condition(X,_)),assert(post_condition(X,Post)))
       ;  (print_error('### illegal variable(s) in: '),
           print_quoted(assert_post(X,Post)),nl,fail)
    ).

/* ===================================================== */

:- dynamic prepost_mnf_flag/1.

:- dynamic prepost_no_error_so_far/0.

prepost_no_error_so_far :- fail.  /* as errors are now displayed by error_manager,
 no need for interaction ?? */


/* ===================================================== */


pp_mnf(X) :- prepost_mnf_call(X).
pp_cll(X) :- prepost_call(X).
mnf(X) :- mnf_call(X).
mnf(ProgramPoint,X) :- mnf_call_with_pp(ProgramPoint,X).


/* ===================================================== */

prepost_call(X) :-
	/* print(pre(X)),nl, */
	(verify_pre(X) -> true ; print_error(verify_pre_failed(X))),
	call(X),
	/* print(post(X)),nl, */
	(verify_post(X) -> true ; print_error(verify_post_failed(X))).

verify_pre(Call) :-
  (run_time_type_check_mode(off) -> true
   ;
	(pre_condition(Call,Pre)
	->( (\+(call(Pre))
		-> (functor(Call,Pred,Arity),
		    add_error(verify_pre,'### PRE-CONDITION ERROR OCCURED: ',Pred/Arity),
	   	    print('### '),print_quoted(Call),nl,
                    prepost_user_interaction
		   )
		;  (true)
	     )
	  )
	; (functor(Call,Pred,Arity),
	   add_error(verify_pre,'### No PRE-CONDITION for ',Pred/Arity),
	   prepost_user_interaction )
	)).

verify_post(Call) :-
  (run_time_type_check_mode(off) -> true
   ;
	(post_condition(Call,Post)
	->( (\+(call(Post))
		-> (functor(Call,Pred,Arity),
		    add_error(verify_post,'### POST-CONDITION ERROR OCCURED: ',Pred/Arity),
	   	    print('### '),print_quoted(Call),nl,
                    prepost_user_interaction
		   )
		;  (true)
	     )
	  )
	; (functor(Call,Pred,Arity),
	   add_error(verify_post,"### No POST-CONDITION for ",[Pred/Arity]),
	   prepost_user_interaction )
	)).


prepost_user_interaction :- prepost_no_error_so_far,
       print_error('### => Stop at next error (y/n) => '),
       read(Answer),
       ((Answer='y') -> true ; retract(prepost_no_error_so_far)).

/* ===================================================== */


prepost_mnf_call(X) :-
  (run_time_type_check_mode(off)
   -> call(X)
   ;  if(prepost_call(X),true,
        (add_error(mnf,"### WARNING CALL HAS FAILED: ",[X]),
	     print('### '),print_quoted(X),nl,
         prepost_user_interaction,
         fail)
       )
  ).
  
mnf_call(X) :-
  (run_time_type_check_mode(off) -> 
   call(X)
   ;  if(call(X),true,
        (add_error(mnf,"### WARNING CALL HAS FAILED: ",[X]),
	     print('### '),print_quoted(X),nl,
         prepost_user_interaction,
         fail)
       )
  ).
	
	
mnf_call_with_pp(ProgramPoint,X) :-
  (run_time_type_check_mode(off)
   -> call(X)
   ;  if(call(X),true,
        (add_error(mnf,"### WARNING CALL HAS FAILED: ",[(X,ProgramPoint)]),
	     print('### '),print_quoted(X),nl,
	     print('### at program point:'),print(ProgramPoint),nl,
         prepost_user_interaction,
         fail)
       )
  ).
	

/* ===================================================== */



:- dynamic self_check/1.

add_self_check(X) :-
    (nonvar(X)
       -> (self_check(X) -> true ; assert(self_check(X)))
       ;  (print_error('### trying to assert variable as self_check: '),print_error(X),fail)
    ).

assert_must_succeed_any(X) :-
    add_self_check(must_succeed(X)).
assert_must_succeed(X) :-
    add_self_check(must_succeed_without_residue(X)).
assert_must_succeed_multiple(X) :-
    add_self_check(must_succeed_multiple_without_residue(X)).
assert_must_fail(X) :-
    add_self_check(must_fail(X)).

must_fail(X) :-
	copy_term(X,Y),
	X,!,
	add_error(must_fail,"### Self-Check Failed !!! Call succeeded: ~w",[Y]),
	print_error('### The call: '),
	print_error(Y),
	print_error('### should have failed but succeeded with:'),
	print_error(X).
must_fail(_X).


must_succeed(X) :-
	\+(X),!,
	add_error(must_succeed,"### Self-Check Failed !!! Call failed:~w ",[X]),
	print_error('### The call: '),
	print_error(X),
	print_error('### should have succeeded but failed !').
must_succeed(_X).

:- dynamic found_must_succeed_sol/0.

call_residue(Call, []) :- Call.

must_succeed_without_residue(X) :-
	must_succeed(X),
	retractall(found_must_succeed_sol),
	call_residue(X,CallResidue),
	((CallResidue=[]) -> true
	  ;  (add_error(must_succeed_without_residue,"### Self-Check has residue: ~w",[X]),
	      add_error(must_succeed_without_residue,"### Residue: ~w",[CallResidue]))
	),
	(found_must_succeed_sol
	  -> (add_error(must_succeed_without_residue,"### Self-Check has multiple solutions: ",[X]),
	      !)
	  ;  (assert(found_must_succeed_sol),fail)
	 ).

must_succeed_without_residue(_).


must_succeed_multiple_without_residue(X) :-
	must_succeed(X),
	retractall(found_must_succeed_sol),
	call_residue(X,CallResidue),
	((CallResidue=[]) -> true
	  ;  (add_error(must_succeed_multiple_without_residue,"### Self-Check has residue: ~w ",[X]),
	      add_error(must_succeed_multiple_without_residue,"### Residue: ~w ",[CallResidue]))
	),
	(found_must_succeed_sol
	  -> (true,!)
	  ;  (assert(found_must_succeed_sol),fail)
	 ).
must_succeed_multiple_without_residue(X) :- 
    add_error(must_succeed_multiple_without_residue,
              "### Self-Check did not succeed multiple times: ~w",[X]).




on_exception(Ex, Goal, Handler) :-
	catch(Goal, Ex, Handler).


safe_call(X) :- on_exception(Exception,
                call(X),
                (print(exception(X,Exception)),nl,nl,
                 add_error(safe_call,"### Exception occured during self-check: ~w",[Exception]))).

perform_self_check :- nl,
   add_message(self_check,"---------------------",[]),
   add_message(self_check,"Performing Self-Check",[]),
   self_check(X),
   statistics(runtime,_),
   safe_call(X),
   statistics(runtime,[_,T]),
   ((T>20) -> (add_message(self_check, "*~w*",[T])) ; true),
   
   fail.
perform_self_check :- nl,
   (error_manager:test_error_occured(_)
     -> print_message('Self-Check FAILED !!!'),fail
     ;  true),
   print_message('Self-Check Successful'),
   print_message('-------------------').
   
   
perform_verbose_self_check :- nl,
   add_message(self_check,"---------------------",[]),
   add_message(self_check,"Performing Self-Check",[]),
   self_check(X),
   add_message(self_check, "~w",[X]),
   statistics(runtime,_),
   safe_call(X),
   statistics(runtime,[_,T]),
   add_message(self_check, " ~w completed in ~w ms", [T]),
   ((T>20) -> (add_message(warning,'### LONG !',[])) ; true),
   fail.

perform_verbose_self_check :- nl,
   (error_manager:test_error_occured(_)
     -> add_error(self_check,"Self-Check FAILED !!!",[]),fail
     ;  true),
   add_message(self_check,"Self-Check Successful",[]),
   add_message(self_check,"---------------------",[]).


/* self_check:display_self_checks */
display_self_checks :- 
   add_message(self_check,"---------------------",[]),
   add_message(self_check,"List of Self-Checks",[]),
   self_check(X),
   add_message(self_check, "~w", [X]),
   
   fail.
display_self_checks :- 
	add_message(self_check,"---End of Self Check----",[]).

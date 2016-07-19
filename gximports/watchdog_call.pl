:- module(watchdog_call,[wcall_error/2, wcall_exc/3,
                         watch_gx_if_conjunction/11, watch_gx_or_conjunction/6, watch_gx_call/3,
                         wcatch_backprops/3]).

%:- use_module('gximports/safe_call.pl',[is_callable_builtin/1]).
%:- use_module('gximports/print_program_point.pl',[print_program_point/1, abort_specialization/0]).

/* no longer used (because of call overhead in Ciao): */
watchdog_call(X,PP) :-
   ((is_callable_builtin(X) ; \+is_known_builtin(X))
     -> catch(call(X),Exc,wcall_exc(X,PP,Exc))
     ;  wcall_error(X,PP)  
   ).

wcall_error(X,PP) :-
        format(user_error, "~n<| BUILT-IN ERROR |> : CALL Atom ~w~n",[X]),
        print_program_point_error('A built-in cannot be executed (it is not sufficiently or incorrectly instantiated). You may want to mark it rescall.',
                             error,builtin_error,[call(X)],correctann([annotation(rescall)],PP)),
	    abort_specialization.
	    
wcall_exc(X,PP,Exc) :-
        format(user_error,"~n<| BUILT-IN EXCEPTION |> : CALL Atom ~w~n  Exception: ~w~n",[X,Exc] ),
        print_program_point_error('An exception occurred while calling a built-in. You may want to mark it rescall.',
                error,builtin_error,[call(X)],
                correctann([annotation(rescall)],PP)),
                abort_specialization.
   

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(when)).
:- endif.

wcatch_backprops(X,PP,Once) :-              /* TO DO: will also be triggered by NUMBER VARS !! */
        varset(X,Vars), copy_term(X,CopyX),
        (construct_when_condition(Vars,C)
          ->  when(C,(format(user_error, "~n<| BACKPROPAGATION WARNING |> Possibly unsafe instantiation !~n  Call before: ~w~n  Call after : ~w~n",[CopyX,X]),
        print_program_point_error('A non-declarative built-in was instantiated by a backpropagation of bindings.',
                             warning,backpropagation_warning,[call(X)],propagated_onto(X,PP)),
        confirm_user_simple(Once)))
         ; true
       ).
                   
construct_when_condition([V],nonvar(V)).
construct_when_condition([V1,V|T],(nonvar(V1) ; Rest)) :-
  construct_when_condition([V|T],Rest).
   
wconnective_error(Call,Error,PP,Once) :-
        format(user_error, "~n<| CONNECTIVE WARNING |> Possibly unsafe instantiation !~n",[]),
        print_program_point_error(Error,
                             warning,connective_warning,[call(Call)],correctann([add_hide_nf([])],PP)),
        confirm_user_simple(Once).

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(terms_vars),[varset/2]). /* term_variables in SICStus */
:- else.
varset(A,B) :- term_variables(A,B).
:- endif.

watch_gx_if_conjunction(If,Th,El,Code1,Code2,Code3,PP,Once,IfOrigCode,ThOrigCode,ElOrigCode) :-
  varset((El,Code3),V3),
  watch_gx_call(If,Code1,Once),
  (check_var(V3) -> true
    ; (wconnective_error(IfOrigCode,'WARNING: If Condition has instantiated Else Branch. You may want to wrap the condition in hide_nf.',
         propagates_bindings(IfOrigCode,ann(resif,1,PP)),Once),
       write(user_error,condition(If)), write(user_error,vars(V3)),nl)
  ),
  varset((If,Code1),V1), varset((El,Code3),V3_2), 
  watch_gx_call(Th,Code2,Once),
  (check_var(V1) -> true
    ; (wconnective_error(ThOrigCode,'WARNING: Then Branch has instantiated If Condition. You may want to wrap the branch in hide_nf.',
         propagates_bindings(ThOrigCode,ann(resif,2,PP)),Once),
       write(user_error,then_branch(Th)), write(user_error,vars(V1)),nl)
  ),
  (check_var(V3_2) -> true
    ; (wconnective_error(ThOrigCode,'WARNING: Then Branch has instantiated Else Branch. You may want to wrap the branch in hide_nf.',
         propagates_bindings(ThOrigCode,ann(resif,2,PP)),Once),
       write(user_error,then_branch(Th)), write(user_error,vars(V3_2)),nl)
  ),
  varset((If,Code1),V1_2), varset((Th,Code2),V2), 
  watch_gx_call(El,Code3,Once),
  (check_var(V1_2) -> true
    ; (wconnective_error(ElOrigCode,'WARNING: Else Branch has instantiated If Condition. You may want to wrap the branch in hide_nf.',
         propagates_bindings(ElOrigCode,ann(resif,3,PP)),Once),
       write(user_error,else_branch(El)), write(user_error,vars(V1_2)),nl)
  ),
  (check_var(V2) -> true
    ; (wconnective_error(ElOrigCode,'WARNING: Else Branch has instantiated Then Branch. You may want to wrap the branch in hide_nf.',
         propagates_bindings(ElOrigCode,ann(resif,3,PP)),Once),
       write(user_error,else_branch(El)), write(user_error,vars(V2)),nl)
  ).
  
  
watch_gx_call(Call,Code,Once) :-
   if(Call,true,(
        write(user_error,'ERROR: call has failed. You have to wrap the branch in hide_nf.'),
        write(user_error,call(Call)),
        confirm_user_simple(Once),
        Code=fail /* not entirely correct: TO DO FIX; can fail as Code can already be instantiated to e.g. true ! */
        )   
     ).
   


watch_gx_or_conjunction(Call1,Call2,Code1,Code2,PP,Once) :-
   varset((Call2,Code2),V2),
   watch_gx_call(Call1,Code1,Once),
  (check_var(V2) -> true
    ; (wconnective_error(Call1,'WARNING: Or Branch 1 has instantiated Branch 2. You may want to wrap the branch in hide_nf.',
         propagates_bindings(Call1,ann(resdisj,1,PP)),Once),
       write(user_error,branch1(Call1)), write(user_error,vars(V2)))
  ),
   varset((Call1,Code1),V1),
   watch_gx_call(Call2,Code2,Once),
  (check_var(V1) -> true
    ; (wconnective_error(Call2,'WARNING: Or Branch 2 has instantiated Branch 1. You may want to wrap the branch in hide_nf.',
         propagates_bindings(Call2,ann(resdisj,2,PP)),Once),
      write(user_error,branch1(Call2)), write(user_error,vars(V1)),nl)
  ).
  
check_var([]).
check_var([V|T]) :- 
    (var(V) -> true
            ; (write(user_error,' *** Instantiation: '), write(user_error,V),nl(user_error),fail)),
    check_var(T).


:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(strings),[get_line/2]).
:- endif.

:- dynamic ignore_watchdog_warnings/0.
	
confirm_user_simple(true) :- !,abort_specialization.
confirm_user_simple(_) :- ignore_watchdog_warnings,!.
confirm_user_simple(_) :-
   format(user_error,"~nType 'c' to continue specialisation, 'C' to continue without further intervention, anything else to abort:~n",[]),
    get_line(user,Return),
	(append("c",_,Return) -> true
				 ; (append("C",_,Return)
						 -> assert(ignore_watchdog_warnings)
						 ;  abort_specialization)
	).
	

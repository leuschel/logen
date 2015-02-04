:- module(logen_dispatcher,[register_specialised_module/2, dispatch_call/2, clear_registry/0]).

/* A module to aid calling specialised modules created by logen */
  
/* --------------------------------------- */

:- dynamic specialisation_table/3.

:- use_module('~/cvs_root/cogen2/examples/ProB/b_interpreter.spec').
specialisation_table('b_interpreter.spec',
                     b_execute_top_level_operation(A, B, [bind(db,C)], D),
                     b_execute_top_level_operation__0(A, B, C, D)).
specialisation_table('b_interpreter.spec',
                     state_violates_invariant([bind(db,C)]),
                     state_violates_invariant([bind(db,C)])).

clear_registry :- retractall(specialisation_table(_,_,_)).  

/* --------------------------------------- */

/* add a specialized module along with specialized calls to the registry */
register_specialised_module(ModuleName,ListOfSpecCalls) :-
   use_module(ModuleName),
   add_entries(ModuleName,ListOfSpecCalls).
   
add_entries(_ModuleName,[]).
add_entries(ModuleName,[Call|T]) :-
   assert_specialisation_table(ModuleName,Call,Call),
   add_entries(ModuleName,T).
   
   
assert_specialisation_table(M,C,SpecCall) :-
  specialisation_table(M,C,_) -> true ; assert(specialisation_table(M,C,SpecCall)).
  
/* --------------------------------------- */


dispatch_call(OriginalModule,Call) :-
   %print(dispatch(OriginalModule,Call)),nl,
   (specialisation_table(SpecModule,Call,SpecCall)
    -> (%print(calling(SpecModule:SpecCall)),nl,
        SpecModule:SpecCall)
    ; OriginalModule:Call
   ).
% (c) 1996-2016 Michael Leuschel
% the original logen was developed by Jesper Jorgensen and Michael Leuschel
% parts of the code contains contributions by Armin Rigo
% see https://github.com/leuschel/logen for more details
:- module(unit_test,[unit_test/1, unit_test_all/1]).

% first tentative version at a unit testing package.
% try out as follows:
% use_module(unit_test), use_module(test_runtime_checks), unit_test(app(X,Y,Z)).
% use_module(unit_test), use_module(test_runtime_checks), unit_test_all(test_runtime_checks).
% (type query twice).

:- use_module('tools/error_manager').


%unit_test_all(Module) :-
%  atom_concat(Module,':unit_test_predicate',Pred),
%  TestCall =.. [Pred,TEST],
%  Call = '$:'(TestCall),
%  print(looking_up_preds_to_test(Call)),nl,
%  Call,
%  print(unit_test(TEST)),nl,
%  unit_test(TEST),fail.
unit_test_all(Module) :- 
  reset_errors_from_source(unit_test),
  atom_concat(Module,':unit_test_pred',Pred),
  TestCall =.. [Pred,TEST,SUCC,POST],
  Call = '$:'(TestCall),
  print(looking_up_preds_to_test(Call)),nl,
  Call,
  test_predicate(TEST,SUCC,POST),
  fail.
unit_test_all(Module) :- print(finished_unit_testing_module(Module)),nl,
  count_errors_occured_with_source(unit_test,N),
  format("Number of errors: ~w.~n",[N]).



test_predicate(TEST,det,POST) :- !,
   predicate_must_succeed_once_test(TEST,POST).
test_predicate(TEST,succ,POST) :- !,
   predicate_must_succeed_test(TEST,POST).
test_predicate(TEST,fail,_POST) :- !,
   predicate_must_fail_test(TEST).
test_predicate(TEST,SUCCPATTERN,_POST) :- 
   add_error(unit_test,"Unkown success pattern ~w in test ~w!~n",[SUCCPATTERN,TEST]).
  
  
predicate_must_fail_test(TEST) :-
  copy_term(TEST,CTEST),
  print(must_fail(TEST)),
  if(CTEST,
     add_error(unit_test,"*** Test did not fail!~n! --> Test: ~w.~n! --> Answer: ~w.~n",[TEST,CTEST]),
     (print(ok),nl)
     ),
  nl.
     
predicate_must_succeed_test(TEST,POST) :-
  print(must_succeed(TEST)),
  if(TEST,
     (POST -> (print(ok),nl)
         ; add_error(unit_test,"*** Post Condition failed!~n --> Post:~w.~n! --> Test:~w.~n",[POST,TEST])),
     add_error(unit_test,"*** Test did not succeed!~n! --> Test: ~w.~n",[TEST])
     ).
     
:- dynamic must_succeed_once_flag/1.

predicate_must_succeed_once_test(TEST,POST) :-
  get_call(TEST,CT),
  print(must_succeed_once(CT)),
  retractall(must_succeed_once_flag(_)),
  copy_term(TEST,CTEST),
  if(CTEST,
     (POST
       -> (must_succeed_once_flag(PreviousAns)
            -> add_error(unit_test,"*** Test succeeded more than once !~n! --> Test:~w.~n! --> First two answers: ~w,~w.~n",[CT,PreviousAns,CTEST])
            ; (assert(must_succeed_once_flag(CTEST)),fail)
          )
       ; add_error(unit_test,"*** Post Condition failed!~n! --> Post:~w.~n! --> Test:~w.~n",[POST,CT])
     ),
     add_error(unit_test,"*** Test did not succeed!~n! --> Test: ~w.~n",[CT])
     ),!.
predicate_must_succeed_once_test(_TEST,_POST) :- print(ok),nl.
     

get_call('$:'(MetaCall),Call) :- !, get_call(MetaCall,Call).
get_call(Call,Call).

:- meta_predicate unit_test(goal).

unit_test(Call) :- nonvar(Call),
   add_postfix_to_pred(Call,'_succ',SCall),
   print(unit_test(SCall)),nl,
   SCall,
   print(testing_succ(Call)),
   if(Call,print(': ok'),print(': FAIL !!!')),
   nl,
   fail.
unit_test(Call) :- nonvar(Call),
   add_postfix_to_pred(Call,'_fail',SCall),
   SCall,
   print(testing_fail(Call)),
   if(Call,print(': FAIL !!!'),print(': ok')),
   nl,
   fail.
unit_test(_) :- print('Done'),nl. 



/* ---------------------------------------------------------- */
/* auxiliary predicate to add a postfix to a predicate call   */
/* ---------------------------------------------------------- */

/* for some reason Ciao gives us the '$:' top-level functor */

add_postfix_to_pred('$:'(Call),Extension,'$:'(Post)) :- !,
  add_postfix_to_pred(Call,Extension,Post).
add_postfix_to_pred(Call,Extension,PostCall) :- 
  Call =.. [F|Args],
  atom_concat(F,Extension,PostF),
  PostCall =.. [PostF|Args].
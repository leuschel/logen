% (c) 1996-2016 Michael Leuschel
% the original logen was developed by Jesper Jorgensen and Michael Leuschel
% parts of the code contains contributions by Armin Rigo
% see https://github.com/leuschel/logen for more details

:- module(runtime_checks,[mnf/1, pp_mnf/1, pp_call/1]).



%:- new_declaration(assert_must_succeed/1).


:- use_module('tools/error_manager.pl', [add_error/3,add_message/4,verbosity_level/1]).

/* ---------------------------------------------------------- */
/* mnf/1: calls a goal and checks that the goal does not fail */
/* ---------------------------------------------------------- */

:- meta_predicate mnf(0).
mnf(X) :- if(X,true,(add_error(runtime_checks,"Call failed: ~w~n",[X]),fail)).



/* ---------------------------------------------------------- */
/* pp_mnf/1: like mnf but also checks pre and post conditions */
/* ---------------------------------------------------------- */

/* if you check predicate P/N then you need P_pre/N and P_post/N
   to be defined */

:- meta_predicate pp_mnf(0).

pp_mnf(X) :- %print(pp_mnf(X)),nl,
    add_postfix_to_pred(X,'_pre',Pre),
    %print(checking_pre(Pre)),nl,
    (Pre -> true ; add_error(runtime_checks,"Pre Condition failed: ~w~n",[Pre])),
    mnf(X),
    add_postfix_to_pred(X,'_post',Post),
    %print(checking_post(Post)),nl,
    (Post -> true ; add_error(runtime_checks,"Post Condition failed: ~w~n",[Post])).
    
/* ---------------------------------------------------------- */
/* pp_call/1: like pp_mnf but without checking non-failure    */
/* ---------------------------------------------------------- */

/* if you check predicate P/N then you need P_pre/N and P_post/N
   to be defined */

:- meta_predicate pp_mnf(0).

pp_call(X) :- %print(pp_mnf(X)),nl,
    add_postfix_to_pred(X,'_pre',Pre),
    %print(checking_pre(Pre)),nl,
    (Pre -> true ; add_error(runtime_checks,"Pre Condition failed: ~w~n",[Pre])),
    X,
    add_postfix_to_pred(X,'_post',Post),
    %print(checking_post(Post)),nl,
    (Post -> true ; add_error(runtime_checks,"Post Condition failed: ~w~n",[Post])).

/* ---------------------------------------------------------- */
/* auxiliary predicate to add a postfix to a predicate call   */
/* ---------------------------------------------------------- */

/* for some reason Ciao gives us the '$:' top-level functor */

add_postfix_to_pred('$:'(Call),Extension,'$:'(Post)) :- !,
  add_postfix_to_pred(Call,Extension,Post).
add_postfix_to_pred(M:Call,Extension,M:Post) :- !,
  add_postfix_to_pred(Call,Extension,Post).
add_postfix_to_pred(Call,Extension,PostCall) :- 
  Call =.. [F|Args],
  atom_concat(F,Extension,PostF),
  PostCall =.. [PostF|Args].
  

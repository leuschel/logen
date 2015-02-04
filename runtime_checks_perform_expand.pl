
:- module(runtime_checks_perform_expand,[ ignore_mnf/3 ],[ ]).

:- op(500,yfx,pre).
:- op(500,yfx,post).
:- op(500,yfx,unit_test).

:- use_module(library(dynamic)).
:- use_module(library(write)).

:- dynamic perform_mnf_on_pred/2.
:- dynamic perform_pp_on_pred/2.

ignore_mnf(term_expansion(_,_), [], _M).

ignore_mnf(( :- mnf(Pred/Arity) ), [], _) :- print(mnf(Pred/Arity)),nl,
       assert(perform_mnf_on_pred(Pred,Arity)).
ignore_mnf(( :- pp_mnf(Pred/Arity) ), [], _) :-
       assert(perform_mnf_on_pred(Pred,Arity)),
       assert(perform_pp_on_pred(Pred,Arity)).
ignore_mnf(( :- pp_call(Pred/Arity) ), [], _) :-
       assert(perform_pp_on_pred(Pred,Arity)).
ignore_mnf((Head :- Body), (Head :- IBody), _M) :- 
       ignore_mnf_body(Body,IBody),!.

ignore_mnf(( :- pre Pred:PreCond ), (PredHead :- PreCond), _) :-
       add_postfix_to_pred(Pred,'_pre',PredHead).
ignore_mnf(( :- post Pred:PostCond ), (PredHead :- PostCond), _) :-
       add_postfix_to_pred(Pred,'_post',PredHead).
%ignore_mnf(( :- unit_test Test:Post ), unit_test_pred(Test,det,Post), _).

ignore_mnf_body(X,X) :- var(X),!.
ignore_mnf_body(mnf(X),Code) :-
   Code = ( %print(mnf(X)),nl,
          if(X,true,(add_error(runtime_checks,"Call failed: ~w~n",[X]),fail))).
ignore_mnf_body(pp_mnf(X),Code) :-
   ignore_mnf_body(mnf(X),MNFCall),
   add_postfix_to_pred(X,'_pre',Pre),
   add_postfix_to_pred(X,'_post',Post),
   Code = 
 (   
     %print(checking_pre(Pre)),nl,
    (Pre -> true ; add_error(runtime_checks,"Pre Condition failed: ~w~n",[Pre])),
    MNFCall,
     %print(checking_post(Post)),nl,
    (Post -> true ; add_error(runtime_checks,"Post Condition failed: ~w~n",[Post]))
 ).
ignore_mnf_body(pp_call(X),Code) :-
   add_postfix_to_pred(X,'_pre',Pre),
   add_postfix_to_pred(X,'_post',Post),
   Code = 
 ( (Pre -> true ; add_error(runtime_checks,"Pre Condition failed: ~w~n",[Pre])),
    X,
   (Post -> true ; add_error(runtime_checks,"Post Condition failed: ~w~n",[Post]))
 ).
ignore_mnf_body((A,B),(IA,IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body((A;B),(IA;IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body((A->B),(IA->IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body(\+(A),\+(IA)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(findall(V,A,R),findall(V,IA,R)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(setof(V,A,R),setof(V,IA,R)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(Call,TCall) :-
    nonvar(Call),functor(Call,F,N),
    (perform_mnf_on_pred(F,N)
       -> (perform_pp_on_pred(F,N)
            -> ignore_mnf_body(pp_mnf(Call),TCall)
            ;  ignore_mnf_body(mnf(Call),TCall)
          )
       ;  (perform_pp_on_pred(F,N)
            -> ignore_mnf_body(pp_call(Call),TCall)
            ;  TCall = Call
          )
    ).
ignore_mnf_body(X,X).

%ignore_mnf(end_of_file, end_of_file,_M).

    

add_postfix_to_pred('$:'(Call),Extension,'$:'(Post)) :- !,
  add_postfix_to_pred(Call,Extension,Post).
add_postfix_to_pred(Call,Extension,PostCall) :- 
  Call =.. [F|Args],
  atom_concat(F,Extension,PostF),
  PostCall =.. [PostF|Args].
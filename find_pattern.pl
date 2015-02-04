%%% This code will be copied into gx files for now
:- dynamic memo_table/4.
:- dynamic multiple_use/2.
:- dynamic spec_clause/2.

:- use_module(library(terms)).
:- use_module(library(terms_check)).


find_pattern(ID, Call, ResCall, _Requestor) :-
	%portray_clause(find_pattern(Call, ResCall)),
        copy_term(Call, CallCopy),
        memo_table(ID,Call, ResCall,_MEMODATA ),
	%portray_clause(match(CallCopy)),
        variant(CallCopy, Call),
        functor(ResCall,F,N),
        (multiple_use(F,N) -> true ; assert(multiple_use(F,N))).

find_variant_pattern(ID, Call, ResCall, _Requestor) :-
	%portray_clause(find_variant_pattern(Call, ResCall)),
        copy_term(Call, CallCopy),
        memo_table(ID,Call, ResCall,_MEMODATA ),
	%portray_clause(match(CallCopy,Call)),
        variant(CallCopy, Call),
        memo_table(ID,Call2,_,_),
        variant(Call,Call2),
        functor(ResCall,F,N),
        (multiple_use(F,N) -> true ; assert(multiple_use(F,N))).
       %,print(variant_ok),nl.
        

insert_pattern(ID, GCall, FCall, MEMODATA) :-
	%% set pending flag somehow
	assert(memo_table(ID, GCall, FCall, MEMODATA)).

update_status(ID, GCall, FCall, Req) :-
	retract(memo_table(ID, GCall, FCall, MEMODATA)),
	get_memodata_requestor(MEMODATA,pending(Req)),
	set_memodata_requestor(MEMODATA,Req, MEMODATAPRIME),
	assert(memo_table(ID,GCall, FCall, MEMODATAPRIME)).

spec_driver :-
	memo_table(ID,GCall, FCall, [pending(Req)|_MEMODATA]),
	update_status(ID, GCall, FCall, Req),	
	!,
	generate_code(GCall, FCall),
	spec_driver.
spec_driver.

:- dynamic failing/3, deterministic/3.

generate_code(Call, ResCall) :-
    (spec_data(debug_mode,true) -> format(user,"Specializing: ~w <-> ~w~n",[Call,ResCall]) ; true),
	copy_term((Call,ResCall), (CCall,CResCall)),
	build_unfold_call(Call, Res, [[CCall],CResCall], UnfoldCall),
	%add_two_extra_arguments('_u', Call, Res,
	findall((ResCall:-FRes) , (UnfoldCall,flatten(Res,FRes)), Clauses),
	functor(ResCall,F,N),
	(Clauses = [] ->
	  (assert(failing(ResCall,F,N)),save_clauses([(ResCall:-fail)]))
	;
	   ((Clauses=[_] -> assert(deterministic(ResCall,F,N)) ; true),
	    save_clauses(Clauses)
	   )
	),
    (spec_data(debug_mode_full,true) -> format(user,"Finished Specializing: ~w <-> ~w~n",[Call,ResCall]) ; true).


save_clauses([]).
save_clauses([C| Cs]) :-
	(C = (Head :- _Body) -> true ; Head=C),
    assert(spec_clause(Head,C)),
    save_clauses(Cs).


print_memo_table(S) :-
    memo_table(A,B,C,D),
    format(S,"/* ~w. */~n",[memo_table(A,B,C,D)]),
    fail.
print_memo_table(_).

print_clauses(S) :-
    spec_data(declaration,Decl),
    portray_clause(S, (:- Decl)),
    fail.
print_clauses(_) :- logen_entry_point(ResCall,_),
  print('/* '),  print(entry(ResCall)), print('*/'),nl,fail.
print_clauses(S) :-
    memo_table(_ID,Orig,Head,_),
    copy_term([Orig,Head], Copy),prettyvars(Copy),
    (predicate_required(Head) -> format(S,"~n/*  ~w :- ~w. */~n",Copy) ; true),
    spec_clause(Head,C), /* for -wprop modus: somehow this portray clause triggers the earlier when declaration;  Ciao keeps the Co-Routines when asserting !!!!  */
    (spec_data(no_post_unfold,true) -> PC=C ; post_unfold(C,PC)),
    portray_clause(S,PC),
    fail.   %%% NOTE: Failing predicates are added at code generation time, so no need for if.
    %	if(spec_clause(Head,C),
%	    (portray_clause(S, C), fail),
%	    (portray_clause((Head :- fail)),fail)).
%print_clauses(_) :- multiple_use(F,N), functor(Head,F,N), memo_table(ID,Orig,Head,_),
%  print('/* '),  print(mul(ID,Orig,Head)), print('*/'),nl,fail.
%print_clauses(_) :- deterministic(F,N,R), print('/* '),  print(deterministic(F,N,R)),print('*/'),nl,fail.
%print_clauses(_) :- failing(F,N,R), print('/* '), print(deterministic(F,N,R)),print('*/'),nl,fail.
print_clauses(S) :-
  (gx_error(_) -> (write(S,'/* ------------------------------------------ */'),nl(S),
                   write(S,'/* Error(s) occurred during specialization !! */'),nl(S),
                   write(S,'/* ------------------------------------------ */'),nl(S))
               ; true). /* improve ?! */

predicate_required(H) :- functor(H,F,N), functor(CH,F,N),predicate_required2(CH,F,N),!.
predicate_required(H) :- \+ spec_data(aggressive_post_unfold,true),
              H=..[_F|Args], nonvar_or_nonlinear_arg(Args,[]).  /* unifying with H would instantiate */
predicate_required2(CH,_,_) :- logen_entry_point(CH,_).
predicate_required2(CH,_,_) :- \+ deterministic(CH,_,_). /* we print non-deterministic predicates; they will not be post-unfolded */
predicate_required2(_,F,N) :- multiple_use(F,N).  /* predicates that are used multiple of times are not post-unfolded */

post_unfold(':-'(H,B),':-'(H,PB)) :- 
    predicate_required(H),!,
    post_unfold_body(B,PB).

nonvar_or_nonlinear_arg([H|T],PrevVars) :-
          nonvar(H) -> true ; 
                   (my_exact_member(H,PrevVars) -> true ; nonvar_or_nonlinear_arg(T,[H|PrevVars])).
my_exact_member(X,[Y|T]) :-
   ((X==Y) -> true ; my_exact_member(X,T)).
   
post_unfold_body(','(A,B),','(PA,PB)) :- !, post_unfold_body(A,PA), post_unfold_body(B,PB).
post_unfold_body(';'(A,B),';'(PA,PB)) :- !, post_unfold_body(A,PA), post_unfold_body(B,PB).
post_unfold_body('->'(A,B),'->'(PA,PB)) :- !, post_unfold_body(A,PA), post_unfold_body(B,PB).
post_unfold_body('\+'(A),'\+'(PA)) :- !, post_unfold_body(A,PA).
post_unfold_body(Call,Res) :- deterministic(Call,F,N), \+ multiple_use(F,N),
   % print(det(Call)),nl,
    (spec_data(aggressive_post_unfold,true) -> true  % TO DO: check for side-effects + allow instantiation if ok
      ; (copy_term(Call,CC), numbervars(CC,1,_), spec_clause(CC,_))), /* unfolding will not instantiate */
   % print(no_inst),nl,
    spec_clause(Call,Clause),!, 
    (Clause = (Call :- Body) -> post_unfold_body(Body,Res) ; Res = true).
post_unfold_body(X,X).


:- dynamic logen_entry_point/2.

add_entry_point(ResCall,OrigCall) :- assert(logen_entry_point(ResCall,OrigCall)).
 % print('% ENTRY'(ResCall)),nl.

:- dynamic gx_error/1.
:- dynamic gx_warning/1.

add_gx_error(Err) :- 
   assert(gx_error(Err)).
add_gx_error(Err) :- 
   assert(gx_warning(Err)).



get_logendata_id([_,ID|_LogenData], ID).
set_logendata_id(ID, [H,_OLDID|LogenData], [H,ID|LogenData]) :- !.
set_logendata_id(H,LD,LD2) :- write(user_error,failed_set_logendata_id(H,LD,LD2)), nl(user_error),LD2=LD.

get_logendata_history([History|_LogenData], History).
set_logendata_history(History, [_|LogenData], [CHistory|LogenData]) :- copy_term(History, CHistory),!.
set_logendata_history(H,LD,LD2) :-
   write(user_error,failed_set_logendata_history(H,LD,LD2)), nl(user_error),LD2=LD.
   
tab_logendata_history([Hist|_],Stream,String) :- tab2(Hist,Stream,String).
tab2(X,_,_) :- var(X),!.
tab2([],_Stream,_String).
tab2([_|T],Stream,String) :- write(Stream,String), tab2(T,Stream,String).

set_logendata_pp(PP,[H,ID,_|_LDTAIL],[H,ID,PP|_LDTAIL]) :- !.
set_logendata_pp(PP,[H,ID],[H,ID,PP]) :- !.
set_logendata_pp(H,LD,LD2) :- write(user_error,failed_set_logendata_pp(H,LD,LD2)), nl(user_error),LD2=LD.
get_logendata_pp([_,_,PP|_LogenData], PP).


get_memodata_requestor([REQ|_MEMODATA],REQ).
set_memodata_requestor([_OLD|MEMODATA],Req, [Req|MEMODATA]).

get_memodata_id([_Req,ID|_MEMODATA], ID).
set_memodata_id([Req,_|MEMODATA], ID, [Req,ID,MEMODATA]).


init_memodata(Requestor, Parent, [Requestor, Parent]).


%%% insert_pattern_with_filter_types(Module, Call, FilterTypes, Requestor, ResidualCall) :-
%%%         % this call should really be specialized away into the gx file
%%%         filter_atom_with_filter_types(Module,Call,FilterTypes,ResidualCall),
%%%         assert(memo_table(Module, Call, ResidualCall)),
%%%         patid(Module, Call, ResidualCall, Id),
%%%         setattr(Id, pending, true),
%%%         (Requestor = internal -> true
%%%           ; setattr(Id, crossmodule, true)
%%%         ).


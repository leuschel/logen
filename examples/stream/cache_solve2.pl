/* cache_solve2.pl:  An interpreter that caches previous results for selected functions */
/* written and annotated for Logen by Michael Leuschel, Sept. 2004 */

/* a slightly reworked version of cache_solve.pl */

:- use_module(prolog_reader).
   
/*
To compile a Prolog File with table_function/1 facts do:
  % logen cache_solve.pl "run_file_all(argv(1),_)" PrologFile
  
  Another simple test:
  % logen cache_solve.pl "test(X)" --spec_file loop_spec.pl
  
  test(13) should give result of 95
*/


/* clever_solve(CallToPredicate, CacheList, NewCacheList) */
/* binding type of CacheList:  list(table(static,static,dynamic,dynamic)) */
/* possible improvement: separate third argument into input and output arguments
   and only keep input and annotate as list(dynamic) */
   
/* Note: the CacheList retains for selected predicates the last computed result;
  the predicates must be functional as only a single solution is stored */

clever_solve(Call,CacheList,NCL) :-
  (prolog_reader:is_built_in(Call)
    -> (call(Call),NCL=CacheList)
    ;  clever_solve2(Call,CacheList,NCL)
   ),
   print(solved(Call)),nl.
   
clever_solve2(A,C,_) :- print(clever_solve2(A,C)),nl,fail.
clever_solve2(Call,CacheList,NCL) :-
   /* possible improvement here: use predicate dependency graph to remove
      unnecessary cache entries for Call, maybe RAF postprocessing can do this for us !? */
     same_skeleton(CacheList,NCL), /* BTI: Binding Time Improvement */
     (cache(Call,CacheList)
        -> cache_solve(Call,CacheList,NCL)
        ;  solve_atom(Call,CacheList,NCL)
      ).
    
cache_solve(Call,[table(Pred,Arity,CacheArgs,Res)|T],[table(Pred,Arity,NA,NR)|NewT]) :-
    (functor(Call,Pred,Arity) /* check if we cache this predicate */
     -> 
    (Call =.. [Pred|CallArgs],
     NewT = T,
     (check_same_arguments(CallArgs,CacheArgs)
      -> (debug_print(used_cache(Res)),
          set_answer(CallArgs,Res),  /* reuse cached result */
          equal_args(NA,CacheArgs), NR = Res /* = Res */
         )
      ;  (debug_print(recomputing(Call)),
          solve_atom(Call,[],[]), /* we need to recompute cache, should be improved to keep cache */
          print(get_args(CallArgs,NA,NR)),nl,
          get_args(CallArgs,NA,NR),
          debug_print(recomputed(Call)))
     )
    )
    ; (NA=CacheArgs,Res=NR,
       cache_solve(Call,T,NewT))
   ).


%solve(A,C,_) :- print(solve(A,C)),nl,fail.
solve(true,C,C).
solve(A,CL,NCL) :- A \= (_,_), A \= (_->_;_), A \= true, clever_solve(A,CL,NCL).
solve((A,B),CL,NCL) :-
     solve(A,CL,ICL), 
     same_skeleton(CL,ICL), /* BTI */
     solve(B,ICL,NCL).
solve((A->B;C),CL,NCL) :-
     (solve(A,CL,ICL)
      -> (same_skeleton(CL,ICL), /* BTI */
          solve(B,ICL,NCL))
      ;  (same_skeleton(CL,ICL), /* BTI */
          solve(C,ICL,NCL))
     ).
          

solve_atom(A,C,_) :- print(start_solve_atom(A,C)),nl,fail.
solve_atom(A,CL,NCL) :- prolog_reader:get_clause(A,B), solve(B,CL,NCL).

/* check whether a call should be cached */
cache(Call,CacheList) :- functor(Call,Pred,Arity), cache2(Pred,Arity,CacheList).
cache2(P,A,[table(P,A,_,_)|_]).
cache2(P,A,[_|T]) :- cache2(P,A,T).
    
/* check whether the functions' arguments are the same as the cached version */
check_same_arguments([_],_). /* do not check return value */
check_same_arguments([CallArg|T],[CachedArg|CT]) :-
   CallArg == CachedArg,
   check_same_arguments(T,CT).
   
debug_print(Msg) :- print(Msg),nl.
   
same_skeleton([],[]).
same_skeleton([table(P,A,CacheArgs,_)|T],[table(P,A,CA2,_)|ST]) :-
      same_skeleton(T,ST),
      length(CacheArgs,Len), length(CA2,Len).
%      functor(Res,Op,Ar),functor(R2,Op,Ar).

%equal_pred(P,P2) :- P=..[Pred|Args], P2=..[Pred|A2], equal_args(Args,A2).

equal_args([],[]).
equal_args([H|T],[H2|T2]) :- H=H2, equal_args(T,T2).

get_args([Res],[],Res).
get_args([H,HH|T],[H2|T2],Res) :- H=H2, get_args([HH|T],T2,Res).

set_answer([A],Res) :- A=Res.
set_answer([_,HH|T],Res) :- set_answer([HH|T],Res).
      
set_up_table(Table) :-
   findall(table(Pred,Arity,CachedArgs,Res),
           (prolog_reader:get_clause(table_function(Pred/Arity),true),
            A1 is Arity-1,
            length(CachedArgs,A1),CachedArgs=[invalid|_], 
            /* assumes we cache with at least arity 2 */
            Res=invalid),
           Table).

run_file(File,Call,Table) :-
   prolog_reader:load_file(File),
   clever_solve(Call,Table,_).

run_file_all(File,Call) :-
   prolog_reader:load_file(File),
   set_up_table(Table), same_skeleton(Table,RT),
   prolog_reader:is_user_pred(Call),
   solve_atom(Call,Table,RT).
   
run_loop(Call) :- run_file_all('loop.pl',Call).

test(X) :- run_loop(loop(X,R)), print(res(X,R)),nl.

run_stream(X) :- run_file_all('spatial_stream.pl',main(X)).
run_auto(X) :-  run_file_all('autoplay_nocirc.pl',main(X)).

main(_X) :- run_file_all('loop2.pl',loop(25,_R)).
   
%loop_nocache(X,R) :- run_file('loop.pl',loop(X,R),[]).
%loop_cache(X,R) :- run_file('loop.pl',[table(func,2,[invalid,invalid],func(invalid,invalid))]).

%:- solve_atom(loop(13,R),[],S), print(res(R,S)),nl.
%:- loop_cache(13,R), print(loop_cache(13,R)),nl.

/* --------------------------------------------- */

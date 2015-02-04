/* cache_solve.pl:  An interpreter that caches previous results for selected functions */
/* written and annotated for Logen by Michael Leuschel, Sept. 2004 */

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
   ).
   
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
          Call = Res,  /* reuse cached result */
          NA = CacheArgs, NR = Res
         )
      ;  (debug_print(recomputing(Call)),
          solve_atom(Call,[],[]), /* we need to recompute cache, should be improved to keep cache */
          NA = CallArgs,
          NR = Call)
     )
    )
    ; (NA=CacheArgs,Res=NR,
       cache_solve(Call,T,NewT))
   ).


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
same_skeleton([table(P,A,_,_)|T],[table(P,A,_,_)|ST]) :-
      same_skeleton(T,ST).
      
set_up_table(Table) :-
   findall(table(Pred,Arity,[invalid|_],invalid),
           prolog_reader:get_clause(table_function(Pred/Arity),true),
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

main(X) :- run_auto(X).
   
%loop_nocache(X,R) :- run_file('loop.pl',loop(X,R),[]).
%loop_cache(X,R) :- run_file('loop.pl',[table(func,2,invalid,invalid)]).

%:- solve_atom(loop(13,R),[],S), print(res(R,S)),nl.
%:- loop_cache(13,R), print(loop_cache(13,R)),nl.

/* --------------------------------------------- */

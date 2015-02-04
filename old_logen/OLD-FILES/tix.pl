/* ----------- */
/*    T I X    */
/* ----------- */

:- module(tix,
	    [trace_memo_call/2,
	     trace_generalise/2]).

:- use_module(pp).
:- use_module(memo).
:- use_module('cogen-ml').
:- use_module(builtins).

trace_memo_call(Call,FilteredCall) :-
  (find_pattern(Call,FilteredCall) ->
                          print('+') ;
                          (trace_generalise(Call,GCall),
                          print('start specialise '),
                          print(Call), print(' GCall: '),
                          print(GCall),nl,
			  insert_pattern(GCall,FilteredHd),
                           findall(NClause,
                                   (trace_unfold_call(GCall,Body),
				    flatten(Body,FBody),
				    print(flatten(Body,FBody)),nl,
				   NClause = clause(FilteredHd,FBody)),
                                   NClauses),
			   pp(NClauses),
			   find_pattern(Call,FilteredCall),
			   print('end specialise '),
			   print(Call), print(' FCall: '),
			   print(FilteredCall),nl)
   ).
   
trace_generalise(Call,GCall) :-
    ((print(try_filter(Call)),cgs:filter(Call,ArgTypes),
      print(cgs:filter(Call,ArgTypes)),nl,
      Call =.. [F|FArgs],
      l_generalise(ArgTypes,FArgs,GArgs),
      print(lg),nl,
      (GCall =..[F|GArgs]) )
    ->
       true
    ;
     (print('*** WARNING: unable to generalise: '), print(Call),nl,
      GCall = Call)
    ).
    
trace_unfold_call(Call,ResBody) :- nl, print(unfold(Call)),
  tools:ann_clause(ClNr,Call,Body),
  print(clause(ClNr,Call)),nl,
  trace_unfold_body(Body,ResBody),
  print(end_unfold(Call)),nl.

trace_unfold_body((G,GS),GRes) :- print(' , '), !,
  trace_unfold_body(G,G1),
  filter_cons(G1,GS1,GRes,true),
  trace_unfold_body(GS,GS1).

trace_unfold_body(unfold(Call),Body) :- print(' u '), !,
   trace_unfold_call(Call,Body).
trace_unfold_body(memo(Call),FilteredCall) :- print(' m '), !,
    trace_memo_call(Call,FilteredCall).

trace_unfold_body(true,true) :- print(' t '), !.
trace_unfold_body(call(Call),true) :- print(call(Call)), !,call(Call).
trace_unfold_body(rescall(Call),Call) :- print(' r '), !.
trace_unfold_body(semicall(dif(X,Y)),ResCall) :- print(' s '), !,
    (\+(X=Y) -> (ResCall = true) ; 
     ((X==Y) -> (fail) ; (ResCall = dif(X,Y)))).
trace_unfold_body(semicall(Call),ResCall) :- print(' s '), !,
 (varlike_imperative(Call)
   -> (call(Call), ResCall = Call)
   ;
    (groundlike_imperative(Call) ->
      (Call -> (ResCall=true) ; (ResCall=Call))
      ;
      (ResCall = Call)
    )
  ).
	 
	 
trace_unfold_body(if(G1,G2,G3),    /* Static if: */
     ResCode) :- print(' if '), !,
     (trace_unfold_body(G1,_)
      -> trace_unfold_body(G2,ResCode)
      ;  trace_unfold_body(G3,ResCode)).
      
trace_unfold_body(resif(G1,G2,G3), /* Dynamic if: */
     ((VS1) -> (VS2) ; (VS3))) :- print(' resif '), !,
	trace_unfold_body(G1,VS1),
	trace_unfold_body(G2,VS2),
	trace_unfold_body(G3,VS3).
	
trace_unfold_body(semif(G1,G2,G3), /* Semi-online if: */
            SpecCode) :- print(' semif '), !,
      (trace_unfold_body(G1,VS1),flatten(VS1,FlatVS1),
      ((FlatVS1 == true)
        -> (trace_unfold_body(G2,SpecCode))
	;  ((FlatVS1 == fail)
            -> (trace_unfold_body(G3,SpecCode))
	    ;  (trace_unfold_body(G2,VS2),trace_unfold_body(G3,VS3),
	        (SpecCode = ((FlatVS1) -> (VS2) ; (VS3))))
	   )
     )).
	
trace_unfold_body(try(G1,G2), /* custom exception handling mechanism: */
     CODE) :- print(' try '), !,
	(trace_unfold_body(G1,VS1) ->
	  (CODE = ((VS1) -> true ; (VS2)))
	  ;
	  (CODE = VS2)
	 ),
	trace_unfold_body(G2,VS2). /* G2 should not fail and bind anything */
     
trace_unfold_body(resfindall(Vars,G2,Sols), /* Dynamic findall: */
     findall(Vars,VS2,Sols)) :- print(' resfindall '), !,
	trace_unfold_body(G2,VS2).
     

trace_unfold_body(resdisj(G1,G2),(VS1 ; VS2)) :-
        print(' resdisj '), !, /* residual disjunction */
	trace_unfold_body(G1,VS1),
	trace_unfold_body(G2,VS2).
trace_unfold_body( (G1;G2), SpecCode) :-
        print(' ; '), !,/* static disjunction */
	(trace_unfold_body(G1,SpecCode) ;
	 trace_unfold_body(G2,SpecCode)).
	
trace_unfold_body(not(G1),true) :-
     print(' not '), !,    /* Static declarative not: */
     \+(trace_unfold_body(G1,_)).
trace_unfold_body(resnot(G1), \+(VS1)) :- /* Dynamic declarative not: */
     print(' resnot '), !, trace_unfold_body(G1,VS1).
trace_unfold_body(restnot(G1), tnot(VS1)) :- /* Dynamic tabled not: */
     print(' restnot '), !, trace_unfold_body(G1,VS1).

trace_unfold_body(hide_nf(G1),ResCode) :- 
       print(' hide_nf '), !,
       user:varlist(G1,VarsG1), 
       print(findall(G1)),nl,
       findall((Code,VarsG1),trace_unfold_body(G1,Code),ForAll1),
       make_disjunction(ForAll1,VarsG1,ResCode).
       
trace_unfold_body(hide(G1),ResCode) :- 
       print(' hide '), !, user:varlist(G1,VarsG1), 
       findall((Code,VarsG1),trace_unfold_body(G1,Code),ForAll1),
       ForAll1 = [_|_], /* detect failure */
       make_disjunction(ForAll1,VarsG1,ResCode).
	 

/* some special annotations: */
trace_unfold_body(ucall(Call),SpecCode) :- !,
        print(ucall(Call)),nl,
	trace_unfold_call(Call,SpecCode).
trace_unfold_body(mcall(Call),SpecCode) :- !,
	trace_memo_call(Call,SpecCode).
trace_unfold_body(det(Call),C) :- !,
    copy_term(Call,CCall),
    (call(CCall)->(C=(Call=CCall));(C=fail)).


trace_unfold_body(X,_SpecCode) :-
    print('*** WARNING: unknown annotation: '),
    print(X),nl,fail.
    

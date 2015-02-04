/* ----------- */
/*    L I X    */
/* ----------- */

:- module(mix,
	    [memo_call/2,
	     unfold_call/2,
	     unfold_body/2]).

:- use_module(cgs).
:- use_module(pp).
:- use_module(memo).
:- use_module(builtins).
:- use_module('cogen-ml').

memo_call(Call,FilteredCall) :-
  (find_pattern(Call,FilteredCall) ->
                          true ;
                          (generalise(Call,GCall),
                           print(generalise(Call,GCall)),nl,
			  insert_pattern(GCall,FilteredHd),
                           findall(NClause,
                                   (unfold_call(GCall,Body),
				   flatten(Body,FBody),
				   NClause = clause(FilteredHd,FBody)),
                                   NClauses),
			   pp(NClauses),
                           print(finished(GCall)),nl,
			   find_pattern(Call,FilteredCall))
   ).
   
unfold_call(Call,ResBody) :- 
  tools:ann_clause(_,Call,Body),
  unfold_body(Body,ResBody).


unfold_body((G,GS),GRes) :-
  unfold_body(G,G1),
  filter_cons(G1,GS1,GRes,true),
  unfold_body(GS,GS1).

unfold_body(unfold(Call),Body) :-
   unfold_call(Call,Body).
unfold_body(memo(Call),FilteredCall) :-
    memo_call(Call,FilteredCall).

unfold_body(true,true).
unfold_body(call(Call),true) :- call(Call).
unfold_body(rescall(Call),Call).
unfold_body(semicall(dif(X,Y)),ResCall) :- !,
    (\+(X=Y) -> (ResCall = true) ; 
     ((X==Y) -> (fail) ; (ResCall = dif(X,Y)))).
unfold_body(semicall(Call),ResCall) :-
 (varlike_imperative(Call)
   -> (call(Call), ResCall = Call)
   ;
    (groundlike_imperative(Call) ->
      (Call -> (ResCall=true) ; (ResCall=Call))
      ;
      (equallike_imperative(Call) ->
        (call(Call), ResCall = true) ; (ResCall = Call))
    )
  ).
	 
	 
unfold_body(if(G1,G2,G3),    /* Static if: */
     ResCode) :-
     unfold_body(G1,_)
      -> unfold_body(G2,ResCode)
      ;  unfold_body(G3,ResCode).
      
unfold_body(resif(G1,G2,G3), /* Dynamic if: */
     ((VS1) -> (VS2) ; (VS3))) :-
	unfold_body(G1,VS1),
	unfold_body(G2,VS2),
	unfold_body(G3,VS3).
	
unfold_body(semif(G1,G2,G3), /* Semi-online if: */
            SpecCode) :-
      (unfold_body(G1,VS1),flatten(VS1,FlatVS1),
      ((FlatVS1 == true)
        -> (unfold_body(G2,SpecCode))
	;  ((FlatVS1 == fail)
            -> (unfold_body(G3,SpecCode))
	    ;  (unfold_body(G2,VS2),unfold_body(G3,VS3),
	        (SpecCode = ((FlatVS1) -> (VS2) ; (VS3))))
	   )
     )).
	
unfold_body(try(G1,G2), /* custom exception handling mechanism: */
     CODE) :- 
	(unfold_body(G1,VS1) ->
	  (CODE = ((VS1) -> true ; (VS2)))
	  ;
	  (CODE = VS2)
	 ),
	unfold_body(G2,VS2). /* G2 should not fail and bind anything */
     
unfold_body(resfindall(Vars,G2,Sols), /* Dynamic findall: */
     findall(Vars,VS2,Sols)) :-
	unfold_body(G2,VS2).

unfold_body(resdisj(G1,G2),(VS1 ; VS2)) :- /* residual disjunction */
	unfold_body(G1,VS1),
	unfold_body(G2,VS2).
unfold_body( (G1;G2), SpecCode) :- /* static disjunction */
	(unfold_body(G1,SpecCode) ;
	 unfold_body(G2,SpecCode)).
	
unfold_body(not(G1),true) :-    /* Static declarative not: */
     \+(unfold_body(G1,_)).
unfold_body(resnot(G1), \+(VS1)) :- /* Dynamic declarative not: */
     unfold_body(G1,VS1).
unfold_body(restnot(G1), tnot(VS1)) :- /* Dynamic declarative tabled not: */
     unfold_body(G1,VS1).

unfold_body(hide_nf(G1),ResCode) :- 
       user:varlist(G1,VarsG1), 
       findall((Code,VarsG1),unfold_body(G1,Code),ForAll1),
       make_disjunction(ForAll1,VarsG1,ResCode).
       
unfold_body(hide(G1),ResCode) :- 
       user:varlist(G1,VarsG1), 
       findall((Code,VarsG1),unfold_body(G1,Code),ForAll1),
       ForAll1 = [_|_], /* detect failure */
       make_disjunction(ForAll1,VarsG1,ResCode).
	 

/* some special annotations: */
unfold_body(ucall(Call),SpecCode) :-
	unfold_call(Call,SpecCode).
unfold_body(mcall(Call),SpecCode) :-
	memo_call(Call,SpecCode).
unfold_body(det(Call),C) :-
    copy_term(Call,CCall),
    (call(CCall)->(C=(Call=CCall));(C=fail)).

/*
unfold_body(X,_SpecCode) :-
    print('*** WARNING: unknown annotation: '),
    print(X),nl,fail. */
    

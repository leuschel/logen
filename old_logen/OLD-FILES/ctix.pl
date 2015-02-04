/* ----------------------- */
/*    CONJUNCTIVE T I X    */
/* ----------------------- */

:- ensure_consulted('pp').
  
trace_memo_calls([],true).
trace_memo_calls([C1|Cs],(ResC1,ResCs)) :-
  trace_memo_call(C1,ResC1),
  trace_memo_calls(Cs,ResCs).

trace_memo_call(true,true) :- !.
trace_memo_call(Call,FilteredCall) :-
  print(try_find_pattern(Call,FilteredCall)),nl,
  (find_pattern(Call,FilteredCall) ->
                          print('+') ;
                          (trace_generalise(Call,GCall),
                          print('START SPECIALISE '),
                          print(Call), print(' GCall: '),
                          print(GCall),nl,
			  insert_pattern(GCall,FilteredHd),
			  print(inserted(FilteredHd)),nl,
                           findall(NClause,
                                   (trace_unfold(GCall,Body,Conj),
				    flatten(Conj,FlatConj),
				    split(FlatConj,Split),
				    print(split(Split)),nl,
				    trace_memo_calls(Split,FiltConj),
				    flatten((Body,FiltConj),FBody),
				    print(flatten(Body,FBody)),nl,
				   NClause = clause(FilteredHd,FBody)),
                                   NClauses),
			   pp(NClauses),
			   find_pattern(Call,FilteredCall),
			   print('END specialise '),
			   print(Call), print(' FCall: '),
			   print(FilteredCall),nl)
   ).
   
trace_generalise(Call,GCall) :-
    ((filter(Call,ArgTypes),
      print(filter(Call,ArgTypes)),nl,
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

trace_unfold(Goal,Body,Conj) :-
  insert_unfolds(Goal,GU),
  print(unfolding_conjunction(GU)),nl,
  trace_unfold_body(GU,Body,Conj).
  
insert_unfolds(true,true) :- !.
insert_unfolds((C,Cs),(CU,CsU)) :- !,
  insert_unfolds(C,CU),
  insert_unfolds(Cs,CsU).
insert_unfolds(C,unfold(C)).
  

trace_unfold_body((G,GS),GRes,CRes) :- print(' , '), !,
  trace_unfold_body(G,G1,C1),
  filter_cons(G1,GS1,GRes,true),
  filter_cons(C1,CS1,CRes,true),
  trace_unfold_body(GS,GS1,CS1).

trace_unfold_body(unfold(Call),ResBody,Conj) :- print(' u '), !,
  nl, print(unfold(Call)),
   ann_clause(ClNr,Call,Body),
  print(clause(ClNr)),nl,
  trace_unfold_body(Body,ResBody,Conj),
  print(end_unfold(Call)),nl.
trace_unfold_body(memo(Call),FilteredCall,true) :- print(' m '), !,
    trace_memo_call(Call,FilteredCall).
trace_unfold_body(leaf(Call),true,Call) :- print(' c '), !.

trace_unfold_body(true,true,true) :- print(' t '), !.
trace_unfold_body(call(Call),true,true) :- print(call(Call)), !,call(Call).
trace_unfold_body(rescall(Call),Call,true) :- print(' r '), !.
trace_unfold_body(semicall(Call),ResCall,true) :- print(' semcall '), !,
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

trace_unfold_body(hide_nf(G1),ResCode) :- 
       print(' hide_nf '), !,
       varlist(G1,VarsG1), 
       print(findall(G1)),nl,
       findall((Code,VarsG1),trace_unfold_body(G1,Code),ForAll1),
       make_disjunction(ForAll1,VarsG1,ResCode).
       
trace_unfold_body(hide(G1),ResCode) :- 
       print(' hide '), !, varlist(G1,VarsG1), 
       findall((Code,VarsG1),trace_unfold_body(G1,Code),ForAll1),
       ForAll1 = [_|_], /* detect failure */
       make_disjunction(ForAll1,VarsG1,ResCode).
	 

/* some special annotations: */
trace_unfold_body(ucall(Call),SpecCode) :-
	trace_unfold_call(Call,SpecCode).
trace_unfold_body(mcall(Call),SpecCode) :-
	trace_memo_call(Call,SpecCode).
trace_unfold_body(det(Call),C) :-
    copy_term(Call,CCall),
    (call(CCall)->(C=(Call=CCall));(C=fail)).


trace_unfold_body(X,_SpecCode) :-
    print('*** WARNING: unknown annotation: '),
    print(X),nl,fail.
    
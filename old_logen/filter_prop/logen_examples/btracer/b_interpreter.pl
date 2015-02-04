/* :- module(b_interpreter,
   [b_test_boolean_expression/3,
    b_not_test_boolean_expression/3,
    
    b_compute_expression/4,
    
    b_execute_statement/4,
    b_execute_statements_in_parallel/4,
    b_execute_top_level_statement/4,
    
    b_execute_operation/4,
    b_execute_operation_wo_normalise/5]).
    
   


:- use_module(b_global_sets).
:- use_module(store).
:- use_module(library(lists)).
:- use_module(b_global_sets).
:- use_module(bmachine,[b_operation/4]).

*/  



/* ----------------------------------*/
/*     b_test_boolean_expression     */
/* ----------------------------------*/

b_test_boolean_expression([],_,_).
b_test_boolean_expression('PredicateParenthesis'(_,[BExpr]), LocalState,State) :-
        b_test_boolean_expression(BExpr,LocalState,State).
b_test_boolean_expression('Not'(_,[BExpr]), LocalState,State) :-
        b_not_test_boolean_expression(BExpr,LocalState,State).
b_test_boolean_expression('And'(_,[LHS,RHS]), LocalState,State) :-
        debug_print(and1(LHS)),debug_nl,
        b_test_boolean_expression(LHS,LocalState,State),
        debug_print(and2(RHS)),nl,
        b_test_boolean_expression(RHS,LocalState,State).
b_test_boolean_expression('Implication'(_,[LHS,RHS]), LocalState,State) :-
        (b_not_test_boolean_expression(LHS,LocalState,State) ;
         (b_test_boolean_expression(LHS,LocalState,State),
          b_test_boolean_expression(RHS,LocalState,State))).
b_test_boolean_expression('Equivalence'(_,[LHS,RHS]), LocalState,State) :-
        ((b_test_boolean_expression(LHS,LocalState,State),
         b_test_boolean_expression(RHS,LocalState,State))
         ;
        (b_not_test_boolean_expression(LHS,LocalState,State),
         b_not_test_boolean_expression(RHS,LocalState,State))).
b_test_boolean_expression('Or'(_,[LHS,RHS]), LocalState,State) :-
        (b_test_boolean_expression(LHS,LocalState,State)  ;
         (b_not_test_boolean_expression(LHS,LocalState,State),
          b_test_boolean_expression(RHS,LocalState,State))).
b_test_boolean_expression('In'(_,[Rel,'IntegerSet'(IntSet,_)]),
                          LocalState,State) :- 
        kernel_mappings:integerset_in_boolean_type(IntSet,Kernel_predicate),
        debug_print(integerset_in_boolean_type(IntSet,Kernel_predicate)),debug_nl,!,
        b_compute_expression(Rel,LocalState,State,RelValue),
        (RelValue =  cons_expr(RV,nil_expr) -> true
          ; (RV = RelValue)),   /* check !! */
         /* for some reason first arg to In is a list expression in the XML,
            but BTypes removes it for the invariant */
        KernelCall =.. [Kernel_predicate,RV],
        debug_print(integerset_in_boolean_type(KernelCall)),debug_nl,
        call(KernelCall).
b_test_boolean_expression('In'(_,[Rel,Expression]),
                          LocalState,State) :- 
        Expression =.. [BOP,_,[Arg1,Arg2]],
        kernel_mappings:binary_in_boolean_type(BOP,Kernel_predicate),
        debug_print(binary_in_bool(BOP,Arg1,Arg2)),debug_nl,!,
        b_compute_expression(Arg1,LocalState,State,SV1),debug_nl,
        b_compute_expression(Arg2,LocalState,State,SV2),debug_nl,
        b_compute_expression(Rel,LocalState,State,RelValue),
        (RelValue =  cons_expr(RV,nil_expr) -> true
          ; (RV = RelValue)),   /* check !! */
         /* for some reason first arg to In is a list expression in the XML,
            but BTypes removes it for the invariant */
        KernelCall =.. [Kernel_predicate,RV,SV1,SV2],
        debug_print(binary_in_bool_kernel_call(KernelCall)),debug_nl,
        call(KernelCall).
b_test_boolean_expression('In'(_,[El,Type]), LocalState,State) :-
        b_compute_expression(El,LocalState,State,Element),
        ((Element=cons_expr(EV,nil_expr)) -> true ; (EV = Element)),  /* check !! */
        b_compute_expression(Type,LocalState,State,TV),debug_nl,
        ((TV = global_set(GTV))
           -> (debug_print(in_type(EV,GTV)),debug_nl,
               global_type(EV,GTV))
           ;  (debug_print(check_element_of(EV,TV)),debug_nl,
               check_element_of(EV,TV))
         ).
b_test_boolean_expression(Expression,LocalState,State) :-
        Expression =.. [BOP,_,[Arg1,Arg2]],
        kernel_mappings:binary_boolean_operator(BOP,Kernel_predicate),
        debug_print(binary_bool(BOP)),debug_nl,!,
        b_compute_expression(Arg1,LocalState,State,SV1),debug_nl,
        b_compute_expression(Arg2,LocalState,State,SV2),debug_nl,
        KernelCall =.. [Kernel_predicate,SV1,SV2],
        debug_print(binary_bool_kernel_call(KernelCall)),debug_nl,
        call(KernelCall).
b_test_boolean_expression(Expression,LocalState,State) :-
        Expression =.. [BOP,_,[Arg1,Arg2]],
        kernel_mappings:binary_boolean_operator_arg1islist(BOP,Kernel_predicate),
        debug_print(binary_bool_arg1(BOP,Arg1,Arg2)),debug_nl,!,
        b_compute_expression(Arg1,LocalState,State,Element),
        ((Element=cons_expr(SV1,nil_expr)) -> true ; (SV1 = Element)),  /* check !! */
	debug_nl,
        b_compute_expression(Arg2,LocalState,State,SV2),debug_nl,
        KernelCall =.. [Kernel_predicate,SV1,SV2],
        debug_print(binary_bool_kernel_call(KernelCall)),debug_nl,
        call(KernelCall).
/* b_test_boolean_expression(E,_,_) :-
    print('### uncovered boolean expression:'),
    print(E),nl,fail. */
   



/* --------------------------------------*/
/*     b_not_test_boolean_expression     */
/* --------------------------------------*/
b_not_test_boolean_expression([],_,_) :- print(b_not_empty),nl,!,fail.
b_not_test_boolean_expression('PredicateParenthesis'(_,[BExpr]), LocalState,State) :-
        b_not_test_boolean_expression(BExpr,LocalState,State).
b_not_test_boolean_expression('Not'(_,[BExpr]), LocalState,State) :-
        b_test_boolean_expression(BExpr,LocalState,State).
b_not_test_boolean_expression('And'(_,[LHS,RHS]), LocalState,State) :-
        (b_not_test_boolean_expression(LHS,LocalState,State)  ;
         (b_test_boolean_expression(LHS,LocalState,State),
          b_not_test_boolean_expression(RHS,LocalState,State))).
b_not_test_boolean_expression('Implication'(_,[LHS,RHS]), LocalState,State) :-
        (b_test_boolean_expression(LHS,LocalState,State),
         b_not_test_boolean_expression(RHS,LocalState,State)).
b_not_test_boolean_expression('Equivalence'(_,[LHS,RHS]), LocalState,State) :-
        ((b_test_boolean_expression(LHS,LocalState,State),
          b_not_test_boolean_expression(RHS,LocalState,State))
         ;
        (b_not_test_boolean_expression(LHS,LocalState,State),
         b_test_boolean_expression(RHS,LocalState,State))).
b_not_test_boolean_expression('Or'(_,[LHS,RHS]), LocalState,State) :-
        b_not_test_boolean_expression(LHS,LocalState,State),
        b_not_test_boolean_expression(RHS,LocalState,State).
b_not_test_boolean_expression('In'(_,[Rel,'IntegerSet'(IntSet,_)]),
                          LocalState,State) :- 
        kernel_mappings:integerset_not_in_boolean_type(IntSet,Kernel_predicate),
        debug_print(integerset_not_in_boolean_type(ntSet,Kernel_predicate)),debug_nl,!,
        b_compute_expression(Rel,LocalState,State,RelValue),
        (RelValue =  cons_expr(RV,nil_expr) -> true
          ; (RV = RelValue)),   /* check !! */
         /* for some reason first arg to In is a list expression in the XML,
            but BTypes removes it for the invariant */
        KernelCall =.. [Kernel_predicate,RV],
        debug_print(integerset_not_in_boolean_type(KernelCall)),debug_nl,
        call(KernelCall).
b_not_test_boolean_expression('In'(_,[Rel,Expression]),
                          LocalState,State) :- 
        Expression =.. [BOP,_,[Arg1,Arg2]],
        kernel_mappings:binary_not_in_boolean_type(BOP,Kernel_predicate),
        debug_print(binary_not_in_bool(BOP,Arg1,Arg2)),debug_nl,!,
        b_compute_expression(Arg1,LocalState,State,SV1),debug_nl,
        b_compute_expression(Arg2,LocalState,State,SV2),debug_nl,
        b_compute_expression(Rel,LocalState,State,RelValue),
        (RelValue =  cons_expr(RV,nil_expr) -> true
          ; (RV = RelValue)),   /* check !! */
         /* for some reason first arg to In is a list expression in the XML,
            but BTypes removes it for the invariant */
        KernelCall =.. [Kernel_predicate,RV,SV1,SV2],
        debug_print(binary_not_in_bool_kernel_call(KernelCall)),debug_nl,
        call(KernelCall).
        
b_not_test_boolean_expression(Expression,LocalState,State) :-
        Expression =.. [BOP,_XML,[Arg1,Arg2]],
        kernel_mappings:negate_binary_boolean_operator(BOP,NBOP),!,
        NExpression =.. [NBOP,_XML,[Arg1,Arg2]],
        b_test_boolean_expression(NExpression,LocalState,State).

/* b_not_test_boolean_expression(E,_,_) :-
    print('### uncovered boolean expression (not)'),
    print(E),nl,fail. */
        

  
  
  
  /* -----------------------------*/
/*     b_compute_expression     */
/* -----------------------------*/
/* b_compute_expression(X,_Y,_Z,_V) :-
      nonvar(X), X=..[F|A], print(b_compute_expression(X,_Y,_Z,_V)),nl,fail.*/
b_compute_expression([],_,_,nil_expr) :- !.
b_compute_expression([H|T],LS,S,cons_expr(HR,TR)) :-
        b_compute_expression(H,LS,S,HR),
        b_compute_expression(T,LS,S,TR),!.
b_compute_expression('ExpressionParenthesis'(_,[Ex]),LocalState,State,Value) :-
        b_compute_expression(Ex,LocalState,State,Value).
b_compute_expression('Boolean'('FALSE',_),_LState,_State,Res) :- Res = term(false).
b_compute_expression('Boolean'('TRUE',_),_LState,_State,Res) :- Res = term(true).
b_compute_expression('BooleanSet'(_,_),_LState,_State,Res) :-
      Res = [term(true),term(false)].
b_compute_expression('EmptySet'(_,_),_LState,_State,Res) :- empty_set(Res).
b_compute_expression('EmptySequence'(_,_),_LState,_State,Res) :- empty_sequence(Res).
b_compute_expression('Identifier'(Id,_),LocalState,State,Value) :-
        debug_print(call_lookup_value(Id,LocalState,State,Value)),debug_nl,
        lookup_value_in_store_and_global_sets(Id,LocalState,State,Value),
        debug_print(b_comp_expr_id(Id,Value)),debug_nl.
b_compute_expression('IdentifierComposed'(_,[IEx]),LocalState,State,Value) :-
        /* just ignore IdentifierComposed; does it serve any purpose ? */
        /* USEFUL if we refer to other machines */
        b_compute_expression(IEx,LocalState,State,Value).

b_compute_expression('ListExpression'(_,ListOfEx),LocalState,State,Value) :-
        /*  marks a list of expressions separated with comma @@@ */
        b_compute_list_of_expression(ListOfEx,LocalState,State,Value).

b_compute_expression('ExtensionSet'(_,[Ex]),LocalState,State,ValueSet) :-
        /* convert list of expressions into  set  of elements ? */
        /* nothing to do: list already in proper format */
        b_compute_expression(Ex,LocalState,State,Value),
        convert_list_of_expressions_into_set(Value,ValueSet).
b_compute_expression('ExtensionSet'(Ex),LocalState,State,ValueSet) :-
        /* convert list of expressions into  set  of elements ? */
        /* nothing to do: list already in proper format */
        b_compute_expression(Ex,LocalState,State,Value),
        debug_print('SimpleExtensionSet'(Value)),nl,
        ValueSet = [Value].
b_compute_expression('ExtensionSequence'(_,[Ex]),LocalState,State,ValueSeq) :-
        /* convert list of expressions into  sequence of expr ? */
        b_compute_expression(Ex,LocalState,State,Value),
        convert_list_of_expressions_into_sequence(Value,ValueSeq).
b_compute_expression('PredicateExpression'(_,[El1]),LocalState,State,Val1) :-
        print(pred_exp(El1)),
        (b_test_boolean_expression(El1,LocalState,State), Val1 = term(true))
        ;
        (b_not_test_boolean_expression(El1,LocalState,State), Val1 = term(false)).
b_compute_expression('OrderedPair'(_,[El1,El2]),LocalState,State,(Val1,Val2)) :- debug_print(pair),
        b_compute_expression(El1,LocalState,State,Val1),
        b_compute_expression(El2,LocalState,State,Val2), debug_print(pair(Val1,Val2)).

b_compute_expression(Expression,LocalState,State,Value) :- 
        nonvar(Expression),
        Expression =.. [Op,_,[Arg1]],
        kernel_mappings:unary_function(Op,KernelFunction),
       !, debug_print(unary_fun(Op)),
        b_compute_expression(Arg1,LocalState,State,SV1), debug_print(arg1(Op,SV1)),
        expand_global_sets(SV1,ESV1),
        KernelCall =.. [KernelFunction,ESV1,Value],
        debug_print(kernel_call(KernelCall)),debug_nl,
        call(KernelCall), debug_print(kernel_res(KernelCall)),debug_nl.
b_compute_expression(Expression,LocalState,State,Value) :- 
        Expression =.. [Op,_,[Arg1,Arg2]],
        kernel_mappings:binary_function(Op,KernelFunction),
       !, debug_print(binary_fun(Op)),
        b_compute_expression(Arg1,LocalState,State,SV1), debug_print(arg1(Op,SV1)),
        b_compute_expression(Arg2,LocalState,State,SV2), debug_print(arg2(Op,SV2)),
        expand_global_sets(SV1,ESV1),
        expand_global_sets(SV2,ESV2),
        KernelCall =.. [KernelFunction,ESV1,ESV2,Value],
        call(KernelCall), debug_print(kernel_res(KernelCall)),debug_nl.

%My Code
/*
b_compute_expression('Overriding'(_,[Rel,ExtSet]),LocalState,State,Value) :- debug_print(union),
        b_compute_expression(Rel,LocalState,State,SV1), debug_print(union_set1(SV1)),
        b_compute_expression(ExtSet,LocalState,State,SV2), debug_print(union_set2(SV2)),
        union(SV1,SV2,Value), debug_print(union_res(Value)),debug_nl.
*/

b_compute_expression('Integer'(Val,_),_LocalState,_State,int(Val)) :- !. /* new */

/* b_compute_expression(E,_S,_L,R) :-
   (var(R) -> (print(uncovered_expression(E)),nl)
            ; (print(b_compute_expression_failed(E,_S,_L,R)),nl)),fail.
*/



/* maybe this should be delayed into kernel ?! */
expand_global_sets(G,Res) :- /* translate global SETS into explicit sets */
    when(nonvar(G),(
   ((G=global_set(GS)) -> 
       (all_elements_of_type(GS,Res),print('^'),print(GS),print('^ '))
                       ; (Res=G)))).
/* IMPROVE to avoid expanding variables: done !? */








convert_list_of_expressions_into_sequence(nil_expr,nil_seq).
convert_list_of_expressions_into_sequence(cons_expr(H,T),cons(H,ConvT)) :-
  convert_list_of_expressions_into_sequence(T,ConvT).
    
convert_list_of_expressions_into_set(nil_expr,[]).
convert_list_of_expressions_into_set(cons_expr(H,T),[H|ConvT]) :-
  convert_list_of_expressions_into_set(T,ConvT).

   
b_compute_list_of_expression([],_,_,nil_expr).
b_compute_list_of_expression([H|T],LS,S,cons_expr(HR,TR)) :-
        b_compute_expression(H,LS,S,HR),
        b_compute_list_of_expression(T,LS,S,TR).



/* --------------------------------- */


lookup_value_in_store_and_global_sets(Id,State,Val) :- 
 (lookup_value(Id,State,RVal) -> (RVal=Val) 
    ; (b_global_set(Id) -> (Val=global_set(Id))  /* global set evaluates to itself */
        ; (b_reserved_identifier(Id) -> (Val = reserved_id(Id))
           ; (b_global_sets:lookup_global_constant(Id,VGC)
              -> (Val = VGC)
              ;  (print(lookup_failed(Id,State)),nl,
                  Val = term(Id)  /* Val=fail */
                 )
          )))
  ).
lookup_value_in_store_and_global_sets(Id,LocalState,State,Val) :- 
 (lookup_value(Id,LocalState,State,RVal) -> (RVal=Val) 
    ; (b_global_set(Id)
        ->  (Val=global_set(Id))  /* global set evaluates to itself */
        ; (b_reserved_identifier(Id) -> (Val = reserved_id(Id))
           ; (b_global_sets:lookup_global_constant(Id,VGC)
              -> (Val = VGC)
              ;  (print(llookup_failed(Id,State)),nl,
                  Val = term(Id)  /* Val=fail */
                 )
          )))
  ).  
  
b_reserved_identifier('NAT').
b_reserved_identifier('NAT1').
b_reserved_identifier('NATURAL').
b_reserved_identifier('INT').
b_reserved_identifier('INTEGER').
b_reserved_identifier('BOOL').




/* -----------------------------*/
/*    b_compute_expressions     */
/* -----------------------------*/

b_compute_expressions([], _, _, []).
b_compute_expressions([EXPRsHd|EXPRsTl],LocalState,InState,[ValHd|ValTl]) :-
  b_compute_expression(EXPRsHd,LocalState,InState,ValHd),
  b_compute_expressions(EXPRsTl,LocalState,InState,ValTl). 


/* -----------------------------*/
/*     b_execute_statement      */
/* -----------------------------*/
/* b_execute_statement(XMLStatements:input, StateofLocalVariables:input,
                      StateOfGlobalVariables:input,
                      ListOfUpdateBindingsForGlobalVariablesAndResultVariables:output) */

%My Code
b_execute_statement('SubstitutionBecomeEqualVariables'(_,[LHS,'ListExpression'(_, EXPRs)]),  
/*  Var1, ..., VarN := Expr1, ..., ExprN */
   LocalState,InState,OutState) :-
  debug_print('assign: '),
  get_identifiers(LHS, IDs), %IDs - a list of variables from LHS
  b_compute_expressions(EXPRs,LocalState,InState,VALs),% VALs - a list of computed values 
  store_values(IDs, VALs, OutState).

b_execute_statement('SubstitutionBegin'(_,[Statement]),LocalState,InState,OutState) :-
    b_execute_statement(Statement,LocalState,InState,OutState).
    
    
    
b_execute_statement('SubstitutionChoice'(_,[LHS,RHS]),  
  /*  Choice LHS Or RHS End */
   LocalState,InState,OutState) :-
   debug_print(choice(LHS,RHS)),debug_nl,
    (b_execute_statement(LHS,LocalState,InState,OutState) ;
     b_execute_statement(RHS,LocalState,InState,OutState)).
    
    
b_execute_statement('SubstitutionBecomeEqualVariables'(_,[LHS,RHS]),  
  /*  LHS := RHS */
   LocalState,InState,OutState) :-
  debug_print('assign: '),  
  get_identifier_from_single_arg(LHS,Id), debug_print(id), debug_print(' := '),
  b_compute_expression(RHS,LocalState,InState,Val), 
  debug_print(val(Val)),debug_nl,
  ((Val = abort(AbortMsg),
   store_value(abort__flag__info,AbortMsg,[],S1),print(abort_assign(AbortMsg)),nl,
   store_value(Id,Val,S1,OutState))
  ;
  (not_top_level_functor(Val,abort),
   store_value(Id,Val,[],OutState)
  )).
  
b_execute_statement('SubstitutionSetIn'(_,[LHS,RHS]),  /* LHS :: RHS */
   LocalState,InState,OutState) :-
  debug_print('assign from set: '),  
  get_identifier_from_single_arg(LHS,Id), debug_print(id), debug_print(' := '),
  b_compute_expression(RHS,LocalState,InState,ValueSet),
  debug_print(set(ValueSet)),debug_nl,
  member(Val,ValueSet), 
  debug_print(val(Val)),debug_nl,
  ((Val = abort(AbortMsg),
   store_value(abort__flag__info,AbortMsg,[],S1),print(abort_assign(AbortMsg)),nl,
   store_value(Id,Val,S1,OutState))
  ;
  (not_top_level_functor(Val,abort),
   store_value(Id,Val,[],OutState)
  )).

b_execute_statement('SubstitutionBecomeEqualFunction'(_,
                     ['CallFunction'(_,[Fun,FunArg]), RHS]),     
		     /*  Fun(X)  :=  RHS */
   LocalState,InState,OutState) :- debug_print(assignfun),debug_nl,
  get_identifier_from_single_arg(Fun,Fid), debug_print(Fid),
  b_compute_expression(FunArg,LocalState,InState,ArgVal),
  debug_print(ArgVal),  debug_print(' := '),
  b_compute_expression(RHS,LocalState,InState,RHSVal), 
  debug_print(rhsval(RHSVal)),debug_nl,
  lookup_value_for_existing_id(Fid,InState,DB),
   /* we do not examine local state: these vars cannot be modified by operations !? */
  override(DB,ArgVal,RHSVal,DB2), /* <---- */
  store_value(Fid,DB2,[],OutState).



b_execute_statement('SubstitutionPrecondition'(_,[PreCond,Body]),
   LocalState,InState,OutState) :-
  print('*** Non-outermost PRE construct'),nl,
  (  (b_test_boolean_expression(PreCond,LocalState,InState),
         /* PRECONDITION SATISFIED */
      print('PRE ok'), nl,
      b_execute_statement(Body,LocalState,InState,OutState),
      debug_print(newstate(OutState)),
      debug_nl
     )
   ;
     (b_not_test_boolean_expression(PreCond,LocalState,InState),
          /* PRE-CONDITION VIOLATED */
      print('PRE false  => ABORT'),nl,
      store_value(abort,pre_condition_violated(PreCond),InState,OutState)
     )
  	
  ).

%My Code
b_execute_statement('SubstitutionSelect'(_, [ PreCond, 'Then'(_,[Body]) | SubstSelTl ]),
  LocalState, InState, OutState) :-
  (
	(  
		b_test_boolean_expression(PreCond,LocalState,InState), %check PRE
		debug_print('In PreCond'), debug_nl,
		b_execute_statement(Body,LocalState,InState,OutState)
  	);
  	(	
		member('When'(_, [PreCond1, Body1]), SubstSelTl),
		b_test_boolean_expression(PreCond1,LocalState,InState), %check PRE
		debug_print('In When'), debug_nl,
		b_execute_statement(Body1,LocalState,InState,OutState)
  	); 
	(	debug_print('In Else of Select'), debug_nl,
	 /* MISSING: test that all other conditions are false */
		member('Else'(_, [Body2]), SubstSelTl), 
		b_execute_statement(Body2,LocalState,InState,OutState)
  	)
  ).

/*
b_execute_statement(SelectInternals, LocalState, InState, OutState) :-
	member('Else'(_, [Body2]), SelectInternals), 
	b_execute_statement(Body2,LocalState,InState,OutState).
*/



b_execute_statement('SubstitutionAny'(_,[Variables,PreCond,Body]),   /* ANY _ WHERE */
   LocalState,InState,OutState) :- debug_print(any),
   Variables = 'ListIdent'(_,LA),
   l_get_identifier(LA,Parameters), debug_print(par(Parameters)),
    same_length(Parameters,FreshVars),
    set_up_localstate(Parameters,FreshVars,LocalState,NewLocalState),
  debug_print(where),debug_nl,
  b_test_boolean_expression(PreCond,NewLocalState,InState), /* check WHERE */
  debug_print(where_precondition_ok),debug_nl,
  b_execute_statement(Body,NewLocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl. /* Missing: CHECK that Variables are actually bound */
b_execute_statement('Then'(_,[Body]),    /* THEN */
   LocalState,InState,OutState) :-debug_print(then),
  b_execute_statement(Body,LocalState,InState,OutState).

b_execute_statement('SubstitutionSkip'(_,[]),    /* SKIP */
   _LocalState,_InState,OutState) :-debug_print(skip),
   OutState = [].
b_execute_statement('Parallel'(_,Statements),  /*   LHS  || RHS  */ %@@@
   LocalState,InState,OutState) :-  !, debug_print(par),
   b_execute_statements_in_parallel(Statements,LocalState,InState,OutState),
   debug_print(end_par),debug_nl.
   
b_execute_statement('SubstitutionIf'(_,[Test,'Then'(_,[ThenBody]),'Else'(_,[ElseBody])]),
   LocalState,InState,OutState) :-
  debug_print(if_then_else),debug_nl,
  b_execute_if_then_else(Test,ThenBody,ElseBody,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
  
b_execute_statement('SubstitutionIf'(_,[Test,'Then'(_,[ThenBody])]),
   LocalState,InState,OutState) :-
  debug_print(if_then),debug_nl,
  b_execute_if_then_else(Test,ThenBody,
           'SubstitutionSkip'(_,_),LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
  
b_execute_statement(E,_,_,_) :-
   print(uncovered_statement(E)),nl,fail.
  
b_execute_if_then_else(Test,ThenBody,_ElseBody,LocalState,InState,OutState) :-  
  b_test_boolean_expression(Test,LocalState,InState), /* check IF test */
  debug_print(if_test_ok),debug_nl,
  b_execute_statement(ThenBody,LocalState,InState,OutState).
b_execute_if_then_else(Test,_ThenBody,ElseBody,LocalState,InState,OutState) :-
  b_not_test_boolean_expression(Test,LocalState,InState), /* check negation of IF test */
  debug_print(if_neg_test_ok),debug_nl,
  b_execute_statement(ElseBody,LocalState,InState,OutState).
  



store_values([], [], []).
store_values([IDsHd|IDsTl], [VALsHd|VALsTl], ResState) :-
  ((VALsHd = abort(AbortMsg))
   -> (store_value(abort__flag__info,AbortMsg,[],[OutAbortHd]),
       store_value(IDsHd,VALsHd,[],[OutStateHd]),
       print(abort_multi_assign(AbortMsg)),nl,
       ResState = [OutAbortHd,OutStateHd|OutStateTl])
   ;  (store_value(IDsHd,VALsHd,[],[OutStateHd]),
       ResState = [OutStateHd|OutStateTl])
   ),
  store_values(IDsTl, VALsTl, OutStateTl).
  


/* -----------------------------------------*/
/*     b_execute_statements_in_parallel     */ 
/* -----------------------------------------*/
/* execute a parallel composition of statements */
b_execute_statements_in_parallel([],_,_,[]).
b_execute_statements_in_parallel([Stmt|TS],LocalState,InState,OutState) :-
  b_execute_statement(Stmt,LocalState,InState,OutState1),
  debug_print(par1(OutState1)),debug_nl,
  b_execute_statements_in_parallel(TS,LocalState,InState,OutState2),
  debug_print(app(OutState1,OutState2)),debug_nl,
  append(OutState1,OutState2,OutState). /* merge updates */
  
  

/* ---------------------------------------*/
/*     b_execute_top_level_statement      */
/* ---------------------------------------*/

/* this is just like b_execute_statement with the
 only difference that a PRE-condition is treated like
 a select, i.e., it does not generate an abort if used
 outside of its condition */

b_execute_top_level_statement('SubstitutionPrecondition'(_,[PreCond,Body]),
   LocalState,InState,OutState) :-
  debug_print(top_level_pre),debug_nl,
  b_test_boolean_expression(PreCond,LocalState,InState), %check PRE
  debug_print(top_level_precondition_ok),debug_nl,
  b_execute_statement(Body,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
b_execute_top_level_statement(XML,LocalState,InState,OutState) :- 
  \+(XML = 'SubstitutionPrecondition'(_,[_,_])),
  b_execute_statement(XML,LocalState,InState,OutState).
  
  
  
  
    
/* -----------------------------*/
/*      b_execute_operation     */
/* -----------------------------*/
/* execute a single operation */
b_execute_operation(FullOperation,InState,NewState,AbortVal) :-
    b_execute_operation(_,FullOperation,InState,NewState,AbortVal).
    
b_execute_operation(Name,FullOperation,InState,NewState,AbortVal) :-
    b_execute_operation2(Name,FullOperation,InState,AbortVal,NewOutState),
    store_updates_and_normalise(NewOutState,InState,NewState) /* actually execute the updates
     to the global store computed by b_execute_statement */.
     
/* now a version which does not normalise: for state based model checking */    
b_execute_operation_wo_normalise(Name,FullOperation,InState,NewState,AbortVal) :-
    b_execute_operation2(Name,FullOperation,InState,AbortVal,NewOutState),
    store_updates(NewOutState,InState,NewState) /* actually execute the updates
     to the global store computed by b_execute_statement */.
     
     
 b_execute_operation2(Name,FullOperation,InState,AbortVal,NewOutState) :-
    b_operation(Name,Parameters,Results,Body),
    same_length(Parameters,FreshVars),
    set_up_localstate(Parameters,FreshVars,[],LocalState),
    /* same_length(Results,RFreshVars),
       set_up_localstate(Results,RFreshVars,LocalState0,LocalState), */
    Operation =.. [Name|FreshVars],
    debug_nl,
    debug_print('Attempting Operation ------------> '),
    debug_print(Operation),debug_nl,
    debug_print(call(b_execute_top_level_statement(Body,LocalState,InState,OutState))),
    debug_nl,
    b_execute_top_level_statement(Body,LocalState,InState,OutState),
    lookup_value_for_existing_id(abort__flag__info,OutState,InState,AbortVal), 
       /* first look in OutState then in InState */
    debug_print('Operation Successful ------> '),
    get_results(Results,OutState,ResultValues,NewOutState),
    debug_print(results(ResultValues)),debug_nl,
    ((ResultValues = [])
      -> /* we have an ordinary operation */
         (FullOperation = Operation)
      ;  (FullOperation =.. ['-->',Operation,ResultValues])
      ).

    
get_results([],OutState,[],OutState).
get_results([R|Results],OutState,[RV|ResultValues],NewOutState) :-
    lookup_and_delete_value(R,RV,OutState,OutState2),
    get_results(Results,OutState2,ResultValues,NewOutState).
  




/* =========================================================================== */


debug_print(X).
debug_nl.


member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

/* =========================================================================== */





/* =========================================================================== */



/* --------------------------------------------------------- */
  
  
/* XML Structure manipulations */
  
get_arguments([],XMLArgs,XMLArgs).
get_arguments([Tag|T],XMLArgs,Res) :-
  Struct =.. [Tag,_,NewArgs],
  member(Struct,XMLArgs),
  get_arguments(T,NewArgs,Res).
get_arguments([Tag|_],XMLArgs,_) :-
  Struct =.. [Tag,_,_],
  \+(member(Struct,XMLArgs)),
  print('### illegal XMLArgs in get_arguments'),nl,
  print('### expected tag: '),print(Tag),nl,
  print('### XMLArgs: '),print(XMLArgs),nl,
  fail.
  
get_name(Args,Name) :-
  member('Name'(_,NArgs),Args), /* print(NArgs),nl,*/
  get_identifier(NArgs,Name).  
  
  
l_get_identifier([],[]).
l_get_identifier([H|L],[HIdent|LA]) :- 
     get_identifier_from_single_arg(H,HIdent),
     l_get_identifier(L,LA).
   
get_identifier_from_single_arg(Arg,Id) :- get_identifier([Arg],Id).



%My Code
get_identifiers('Identifiers'(_, Args), IDs) :-
    get_identifiers1(Args, IDs).

get_identifiers1([], []).
get_identifiers1(['IdentifierComposed'(_, ['Identifier'(IDsHd, _)])|ArgsTl], [IDsHd|IDsTl]) :-
    get_identifiers1(ArgsTl, IDsTl).




get_identifier(Args,Id) :-  
    member('Identifier'(Id,_),Args).
get_identifier(Args,Id) :-  
    member('IdentifierComposed'(_,Args2),Args),
    get_identifier(Args2,Id).
get_identifier(Args,Id) :-  
    member('Identifiers'(_,Args2),Args),
    get_identifier(Args2,Id).
    
    
get_identifier_nr_from_composed(Args,Id,0) :-  
    member('Identifier'(Id,_),Args).
get_identifier_nr_from_composed(Args,Id,Nr) :-  
    member('IdentifierComposed'(_,Args2),Args),
    get_identifier_nr_from_composed(Args2,Id,N1),
    Nr is N1+1.
    
    

/* =========================================================================== */





/* EXPRESSIONS */

unary_function('First',first_sequence).
unary_function('Size',size_of_sequence).
unary_function('Front',front_sequence).
unary_function('Tail',tail_sequence).
unary_function('Rev',rev_sequence).
unary_function('Last',last_sequence).
unary_function('Domain',domain).
unary_function('Range',range).
unary_function('Iseq',injective_sequence).
unary_function('Seq',is_sequence).
unary_function('Seq1',finite_non_empty_sequence).
unary_function('Card',cardinality_as_int). /* new */
unary_function('Inverse',invert_relation). /* new */
unary_function('PowerSet',power_set). /* new */

binary_function('ConcatSequence',concat_sequence).
binary_function('Union',union).
binary_function('Intersection',intersection).
binary_function('SetMinus',difference_set).
binary_function('CallFunction',apply_to).
binary_function('CartesianProduct',cartesian_product_or_times). /* new */
binary_function('Plus',plus). /* new */
binary_function('NatRange',nat_range). /* new */
binary_function('Minus',int_minus). /* new */
binary_function('DomainSubstraction',domain_subtraction).
binary_function('Image',image).

binary_function('Power',unimplemented_binary) :- km_error('Power',binary_function).
/* must be implemented:
  binary_function('Power',power).
  + IntegerSet
   */
   
   
/* BOOLEAN EXPRESSIONS */

binary_in_boolean_type('PartialFunction',partial_function).
binary_in_boolean_type('TotalFunction',total_function).
binary_in_boolean_type('PartialInjection',partial_injection).
binary_in_boolean_type('TotalInjection',total_injection).
binary_in_boolean_type('Bijection',bijection).

binary_not_in_boolean_type('PartialFunction',not_partial_function).
binary_not_in_boolean_type('TotalFunction',not_total_function).
binary_not_in_boolean_type('PartialInjection',not_partial_injection).

integerset_in_boolean_type('NAT',is_natural).
integerset_in_boolean_type('NAT1',is_natural1).

integerset_not_in_boolean_type('NAT',is_not_natural).
integerset_not_in_boolean_type('NAT1',is_not_natural1).

binary_boolean_operator('NotEqualSet',not_equal_object).
    /* Sometimes NotEqualSet/EqualSet seems to be called even if the args are not sets !! */
binary_boolean_operator('EqualSet',equal_object).
binary_boolean_operator('Equal',equal_object). /* sometimes Equal instead of EqualSet is used */
binary_boolean_operator('LessThan',less_than).
binary_boolean_operator('LessThanOrEqual',less_than_equal).
binary_boolean_operator('GreaterThan',greater_than).
binary_boolean_operator('GreatherThan',greater_than). /* spelling mistake in Java parser */
binary_boolean_operator('GreatherThanOrEqual',greater_than_equal).
binary_boolean_operator('GreaterThanOrEqual',greater_than_equal).
binary_boolean_operator('Equal',equal_object). /* unclear to me when EqualSet and when Equal is used */
binary_boolean_operator('NotEqual',not_equal_object). /* check whether correct tag */

 /* note: spelling mistake in XML Greather instead of Greater */

/* for some reasons some operators have list expressionss as first arg
 in the XML even though they do not allow lists there ?! */
binary_boolean_operator_arg1islist('NotSetMemberShip',not_element_of).
binary_boolean_operator_arg1islist('Subset',check_subset_of).
binary_boolean_operator_arg1islist('StrictSubset',strict_subset_of).
binary_boolean_operator_arg1islist('NotStrictSubset',not_strict_subset_of).
binary_boolean_operator_arg1islist('NotSubset',not_subset_of).

negate_binary_boolean_operator(X,Y) :-
   (negate_binary_boolean_operator2(X,Y) ;
   negate_binary_boolean_operator2(Y,X)).
negate_binary_boolean_operator2('In','NotSetMemberShip').
negate_binary_boolean_operator2('EqualSet','NotEqualSet').
negate_binary_boolean_operator2('Equal','NotEqualSet').
negate_binary_boolean_operator2('LessThan','GreatherThanOrEqual').
negate_binary_boolean_operator2('LessThanOrEqual','GreatherThan').
negate_binary_boolean_operator2('Subset','NotSubset').
negate_binary_boolean_operator2('StrictSubset','NotStrictSubset').
negate_binary_boolean_operator2('expressionEqual','expressionNotEqual').




km_error(Operator,Type) :-
   print('### WARNING: Uncovered B Operator'),nl,
   print('### OPERATOR: '), print(Operator),nl,
   print('### TYPE: '), print(Type),nl,
   print('### '),nl.
  



/* =========================================================================== */







state_satisfies_invariant(State) :- 
   b_set_up_valid_state(State), /* added */
   b_extract_types_and_invariant(_Variables,_VarTypes,Invariant),
   b_test_boolean_expression(Invariant,[],State).
state_violates_invariant(State) :- 
   b_set_up_valid_state(State), /* added */
   b_extract_types_and_invariant(_Variables,_VarTypes,Invariant),
   b_not_test_boolean_expression(Invariant,[],State).
   
   

